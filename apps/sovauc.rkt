#lang racket

(require racket/pretty)
(require racket/trace)

(require "lang.rkt")

(provide sovauc-application)
(provide sovauc-escrow)

(define next-scratch 0)
(define (new-scratch!)
  (begin
    (let ([c next-scratch])
      (set! next-scratch (+ c 1))
      c)))

(define sovauc-next-identifier 0)
(define (sovauc-id!)
  (let ([id sovauc-next-identifier])
    (set! sovauc-next-identifier (+ 1 sovauc-next-identifier))
    id))

(define sovauc-create (sovauc-id!))
(define sovauc-bind-escrow (sovauc-id!))
(define sovauc-optin (sovauc-id!))
(define sovauc-open-auction (sovauc-id!))
(define sovauc-enter-bid (sovauc-id!))
(define sovauc-payout-bid (sovauc-id!))
(define sovauc-invalidate-bid (sovauc-id!))
(define sovauc-close-auction (sovauc-id!))
(define sovauc-destroy (sovauc-id!))

;; (rec)  txn 0 pays fees and/or fills minbalance
;; (this) txn 1 opts in
(define sovauc-escrow-setup
  '(and (= (txn TypeEnum) 4)
        (= (txn RekeyTo) (global ZeroAddress))
        ;; ignore fee; account is "ephemeral"
        (or (= (txn XferAsset) (int TMPL_SOV))
            (= (txn XferAsset) (int TMPL_USDC)))
        (= (txn AssetAmount) 0)
        (= (txn AssetCloseTo) (global ZeroAddress))))

;; (rec)  txn 0 pays fees
;; (any)  txn 1 payouts the bid in the hardwired app (and fails if it cannot)
;; (this) txn 2 pays out the token
(define sovauc-escrow-payout
  `(and (= (txn GroupIndex) 2)

        (= (txn TypeEnum) 4)
        (= (txn XferAsset) (int TMPL_SOV))
        (= (txn AssetSender) (global ZeroAddress))
        (= (txn AssetCloseTo) (global ZeroAddress))
        (= (txn RekeyTo) (global ZeroAddress))

        (= (gtxn 1 TypeEnum) 5)
        (= (gtxn 1 ApplicationID) (btoi (byte TMPL_APPID)))
        (= (gtxn 1 ApplicationArgs 0) ,sovauc-payout-bid)))

;; (satisfies-xchange-rate? (gtxn 1 ApplicationArgs 1) (txn AssetAmount))))
;; enforced by app

;; (rec)  txn 0 pays fees (recommended)
;; (auth) txn 1 notifies the hardwired app to close the auction (and fails if it cannot)
;; (this) txn 2 and 3 move all leftover escrowed assets to the auction owner
;; (this) txn 4 closes account out to auction owner
(define sovauc-escrow-close-auction
  `(and (< (txn GroupIndex) 5)
        (> (txn GroupIndex) 1)
        (= (txn RekeyTo) (global ZeroAddress))
        (= (gtxn 2 Sender) (gtxn 3 Sender))
        (= (gtxn 3 Sender) (gtxn 4 Sender))

        (= (gtxn 1 TypeEnum) 5)
        (= (gtxn 1 ApplicationID) (btoi (byte TMPL_APPID)))
        (= (gtxn 1 Note) ,sovauc-close-auction)
        (= (gtxn 1 ApplicationArgs 0) ,sovauc-close-auction)

        (= (gtxn 2 TypeEnum) 4)
        (= (gtxn 2 XferAsset) (int TMPL_SOV))
        (= (gtxn 2 AssetAmount) 0)
        (= (gtxn 2 AssetCloseTo) (addr TMPL_OWNER))

        (= (gtxn 3 TypeEnum) 4)
        (= (gtxn 3 XferAsset) (int TMPL_USDC))
        (= (gtxn 3 AssetAmount) 0)
        (= (gtxn 3 AssetCloseTo) (addr TMPL_OWNER))

        (= (gtxn 4 TypeEnum) 1)
        (= (gtxn 4 Amount) 0)
        (= (gtxn 4 CloseRemainderTo) (addr TMPL_OWNER))))

(define sovauc-escrow
  `(or ,sovauc-escrow-setup
       ,sovauc-escrow-payout
       ,sovauc-escrow-close-auction))

(define (sovauc-check-escrow escrow-address escrow-suffix)
  `(and (= (sha512_256 ,escrow-suffix) (byte TMPL_ESUFFIXH))
        (= ,escrow-address (sha512_256 (concat (byte "\"Program\"")
                                               (byte TMPL_EPREFIX)
                                               (itob (txn ApplicationID))
                                               ,escrow-suffix)))))

(define (verify-128-bit-division dividend1 dividend2 divisor quotient remainder)
  (let ([dividend-low (new-scratch!)]
        [dividend-high (new-scratch!)]
        [product-low (new-scratch!)]
        [product-high (new-scratch!)]
        [final-low (new-scratch!)]
        [carry (new-scratch!)])
    `(begin
       (note "128-bit div")
       (store2! ,dividend-low ,dividend-high (mulw ,dividend1 ,dividend2))
       (store2! ,product-low ,product-high (mulw ,divisor ,quotient))
       (store2! ,final-low ,carry (plusw ,remainder (load ,product-low)))
       (and (= (load ,dividend-high) (+ (load ,product-high) (load ,carry)))
            (= (load ,dividend-low) (load ,final-low))))))
       
;; expects 4-decimal-digits of precision
(define (sovauc-correct-tranche-size?
         tranche-size
         remainder
         anchor
         tranche-index
         num-tranches
         init-tranches-size
         supply
         supply-pct-hths
         lookback
         min-tranche-size
         tranche-ring-sum
         usdc-ring-sum)
  `(cond [(< ,tranche-index ,lookback) (= ,tranche-size ,init-tranches-size)]
         [(= ,usdc-ring-sum 0) (= ,min-tranche-size ,init-tranches-size)]
         [(<= (* ,supply ,supply-pct-hths) ,tranche-ring-sum)
          (= ,min-tranche-size ,init-tranches-size)]
         [(>= ,remainder (+ (* ,lookback ,anchor ,supply-pct-hths)
                            (* ,num-tranches ,usdc-ring-sum)))
          0]

         [else ,(verify-128-bit-division `(* 2 ,usdc-ring-sum)
                                         `(- (* ,supply ,supply-pct-hths) ,tranche-ring-sum)
                                         `(+ (* ,lookback ,anchor ,supply-pct-hths)
                                             (* ,num-tranches ,usdc-ring-sum))
                                         tranche-size
                                         remainder)]))
                                        
         ;; [else (= (+ ,remainder
         ;;             (* ,tranche-size
         ;;                (+ (* ,lookback ,anchor ,supply-pct-hths)
         ;;                   (* ,num-tranches ,usdc-ring-sum))))
         ;;          (* 2 ,usdc-ring-sum
         ;;             (- (* ,supply ,supply-pct-hths) ,tranche-ring-sum)))]))

(define (sovauc-payout-value-ok? payout bid remainder auction-raised tranche-supply)
  `(and (< ,remainder ,auction-raised)
        ;; TODO need wide ops here
        ,(verify-128-bit-division tranche-supply
                                  bid
                                  auction-raised
                                  payout
                                  remainder)))
        ;; (= (+ (* ,payout ,auction-raised) ,remainder)
        ;;    (* ,tranche-supply ,bid))))

(define (sovauc-usdc-ring-old-usdc)
  '(app-global-get (concat (byte 0x0001)
                            (itob (% (app-global-get tranche-index)
                                     (app-global-get lookback))))))

(define (sovauc-usdc-ring-new-usdc! val)
  `(app-global-put! (concat (byte 0x0001)
                            (itob (% (app-global-get tranche-index)
                                     (app-global-get lookback))))
                    ,val))

(define (sovauc-usdc-ring-old-tranche)
  '(app-global-get (concat (byte 0x0002)
                            (itob (% (app-global-get tranche-index)
                                     (app-global-get lookback))))))

(define (sovauc-usdc-ring-new-tranche! val)
  `(app-global-put! (concat (byte 0x0002)
                            (itob (% (app-global-get tranche-index)
                                     (app-global-get lookback))))
                    ,val))

(define sovauc-application
  ;; const; set at initialization
  `((gvars (admin              (addr ax))
           (anchor             (int ac))
           (num-tranches       (int nt))
           (supply             (int sp))
           (supply-pct-hths    (int rs)) ; fixed-pt, quantized to .01%
           (init-tranches-size (int fs))
           (lookback           (int lk))
           (min-tranche-size   (int ms))
           (auction-duration   (int dr))

           ;; set const; set at initialization second stage
           (escrow (addr es))

           ;; updates per auction
           ;; TODO change the rings so that they're an array of size 10
           (usdc-raised-ring  (int ur))
           (tranche-size-ring (int tz))
           (tranche-index     (int ti))

           ;; moving average
           (usdc-ring-sum    (int u_))
           (tranche-ring-sum (int t_))

           ;; state for current auction
           (auction-raised       (int ar))
           (tranche-supply       (int as))
           (auction-deadline     (int ad))
           (outstanding-receipts (int rc)))

    (lvars (bid-receipts (int br)))

    (prog
     (with ([args (calltype)])
           (cond [(= calltype ,sovauc-create)
                  (note "create")
                  (with ([args (init-anchor
                                init-num-tranches
                                init-supply
                                init-supply-pct-hths
                                init-init-tranches-size
                                init-lookback
                                init-min-tranche-size
                                init-auction-duration)])

                        (app-global-put! admin (txn Sender))

                        (app-global-put! anchor (btoi init-anchor))
                        (app-global-put! num-tranches (btoi init-num-tranches))
                        (app-global-put! supply (btoi init-supply))
                        (app-global-put! supply-pct-hths (btoi init-supply-pct-hths))
                        (app-global-put! init-tranches-size (btoi init-init-tranches-size))
                        (app-global-put! lookback (btoi init-lookback))
                        (app-global-put! auction-deadline (btoi init-auction-duration))

                        (and (= (txn ApplicationID) 0)
                             (= (txn NumAppArgs) 9)
                             (= (txn NumAccounts) 0)
                             (= (txn OnCompletion) ,NoOp)))]

                 [(= calltype ,sovauc-bind-escrow)
                  (note "bind escrow")
                  (with ([args (bind-escrow escrow-suffix)])
                        (app-global-put! escrow bind-escrow)
                        (and ,(sovauc-check-escrow 'bind-escrow 'escrow-suffix)
                             (not (= (txn ApplicationID) 0))
                             (= (txn NumAppArgs) 3)
                             (= (txn NumAccounts) 0)
                             (= (txn OnCompletion) ,NoOp)))]

                 [(= calltype ,sovauc-optin)
                  (note "optin")
                  (and (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int ,OptIn)))]

                 [(= calltype ,sovauc-destroy)
                  (note "destroy")
                  (and (not (= (txn ApplicationID) 0))
                       (= (app-global-get tranche-index) (app-global-get num-tranches))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int ,DeleteApplication)))]

                 ;; (auth)         txn 0 funds escrow with tranche size
                 ;; (this = admin) txn 1 sets open auction state
                 [(= calltype ,sovauc-open-auction)
                  (note "open auction")
                  (with ([args (remainder)])
                        (assert (and (= (app-global-get auction-deadline) 0)
                                     (< (app-global-get tranche-index)
                                        (app-global-get num-tranches))))

                        (app-global-put! tranche-supply (gtxn 0 AssetAmount))
                        (app-global-put! auction-deadline
                                         (+ (global LatestTimestamp)
                                            (app-global-get auction-duration)))

                        (and (= (txn GroupIndex) 1)
                             (= (txn Sender) (app-global-get admin))
                             (not (= (txn ApplicationID) 0))
                             (= (txn NumAppArgs) 1)
                             (= (txn NumAccounts) 0)
                             (= (txn OnCompletion) ,NoOp)

                             (= (gtxn 0 TypeEnum) 4)
                             (= (gtxn 0 XferAsset) (int TMPL_SOV))
                             ,(sovauc-correct-tranche-size?
                               '(gtxn 0 AssetAmount)
                               '(btoi remainder)
                               '(app-global-get anchor)
                               '(app-global-get tranche-index)
                               '(app-global-get num-tranches)
                               '(app-global-get supply)
                               '(app-global-get supply-pct-hths)
                               '(app-global-get init-tranches-size)
                               '(app-global-get lookback)
                               '(app-global-get min-tranche-size)
                               '(app-global-get tranche-ring-sum)
                               '(app-global-get usdc-ring-sum))
                             (= (gtxn 0 AssetReceiver) (app-global-get escrow))
                             (= (gtxn 0 AssetCloseTo) (global ZeroAddress))
                             (not (= (gtxn 0 AssetSender) (global ZeroAddress)))))]

                 ;; (any)  txn 0 deposits into escrow
                 ;; (this) txn 1 puts receipts into bidder
                 [(= calltype ,sovauc-enter-bid)
                  (note "enter bid")
                  (app-local-put! 0 bid-receipts (+ (app-local-get 0 bid-receipts)
                                                    (gtxn 0 AssetAmount)))
                  (app-global-put! outstanding-receipts
                                   (+ (app-global-get outstanding-receipts)
                                      (gtxn 0 AssetAmount)))
                  (app-global-put! auction-raised
                                   (+ (app-global-get auction-raised)
                                      (gtxn 0 AssetAmount)))

                  (and (= (txn GroupIndex) 1)
                       (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) ,NoOp)
                       (< (global LatestTimestamp) (app-global-get auction-deadline))

                       (= (gtxn 0 TypeEnum) 4)
                       (= (gtxn 0 XferAsset) (int TMPL_USDC))
                       (= (gtxn 0 AssetReceiver) (app-global-get escrow))
                       (= (gtxn 0 AssetSender) (global ZeroAddress)))]

                 ;; (any)    txn 0 pays fees
                 ;; (escrow) txn 1 sends token to bidder
                 ;; (this)   txn 2 clears receipts
                 [(= calltype ,sovauc-payout-bid)
                  (note "payout bid")
                  (with ([accs (bidder)]
                         [args (remainder)])

                        (assert ,(sovauc-payout-value-ok?
                                  '(gtxn 0 AssetAmount)
                                  '(app-local-get-acct bidder bid-receipts)
                                  '(btoi remainder)
                                  '(app-global-get auction-raised)
                                  '(app-local-get-acct bidder tranche-supply)))

                        (app-global-put! outstanding-receipts
                                         (- (app-global-get outstanding-receipts)
                                            (app-local-get-acct bidder bid-receipts)))
                        (app-local-put-acct! bidder bid-receipts 0)

                        (and (not (< (global LatestTimestamp) (app-global-get auction-deadline)))

                             (= (txn GroupIndex) 2)
                             (not (= (txn ApplicationID) 0))
                             (= (txn NumAppArgs) 1)
                             (= (txn NumAccounts) 1)
                             (= (txn OnCompletion) ,NoOp)))]

                 ;; (this) clears receipts
                 [(= calltype ,sovauc-invalidate-bid)
                  (note "invalidate bid")
                  (with ([accs (bidder)])

                        (app-global-put! outstanding-receipts
                                         (- (app-global-get outstanding-receipts)
                                            (app-local-get-acct bidder bid-receipts)))
                        (app-local-put-acct! bidder bid-receipts 0)

                        (and (= (app-opted-in? bidder (int TMPL_SOV)) 0)
                             (not (< (global LatestTimestamp) (app-global-get auction-deadline)))

                             (not (= (txn ApplicationID) 0))
                             (= (txn NumAppArgs) 1)
                             (= (txn NumAccounts) 1)
                             (= (txn OnCompletion) ,NoOp)))]

                 ;; (rec)          txn 0 pays fees (recommended)
                 ;; (this = admin) txn 1 notifies the hardwired app to close the auction (and fails if it cannot)
                 ;; (escrow)       txn 2 and 3 move all leftover escrowed assets to the auction owner
                 ;; (escrow)       txn 4 closes account out to auction owner
                 [(= calltype ,sovauc-close-auction)
                  (note "close auction")
                  (assert (and (= (app-global-get outstanding-receipts) 0)
                               (not (< (global LatestTimestamp) (app-global-get auction-deadline)))))

                  (app-global-put! usdc-ring-sum
                                   (+ (- (app-global-get usdc-ring-sum)
                                         ,(sovauc-usdc-ring-old-usdc))
                                      (app-global-get auction-raised)))
                  ,(sovauc-usdc-ring-new-usdc! '(app-global-get auction-raised))

                  (app-global-put! tranche-ring-sum
                                   (+ (- (app-global-get tranche-ring-sum)
                                         ,(sovauc-usdc-ring-old-tranche))
                                      (app-global-get tranche-supply)))
                  ,(sovauc-usdc-ring-new-tranche! '(app-global-get tranche-supply))

                  (app-global-put! tranche-index (+ 1 (app-global-get tranche-index)))

                  (app-global-del! auction-raised)
                  (app-global-del! tranche-supply)
                  (app-global-del! auction-deadline)

                  (and (= (txn Sender) (app-global-get admin))
                       (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 0)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) ,NoOp))]
                 [else 0])))

    (onclear (begin
               (app-global-put! outstanding-receipts
                                (- (app-global-get outstanding-receipts)
                                   (app-local-get 0 bid-receipts)))
               1))))
