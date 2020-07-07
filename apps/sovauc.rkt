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

;; <group-0>
;; (rec)   txn 0 pays fees and fills minbalance
;; (admin) txn 1 opens auction
;; (this)  txn 2 and 3 opt into tokens
;; (any)   txn 4 funds auction with tranche size
(define sovauc-escrow-setup
  `(and (< (txn GroupIndex) 4)
        (> (txn GroupIndex) 1)
        (= (txn RekeyTo) (global ZeroAddress))

        (= (gtxn 2 Sender) (gtxn 3 Sender))

        (= (gtxn 1 TypeEnum) 6)
        (= (gtxn 1 ApplicationID) (btoi (byte TMPL_APPID)))
        (= (gtxn 1 NumAccounts) 0)
        ;; (= (gtxn 1 NumAppArgs) 1) ;; checked earlier
        (= (gtxn 1 OnCompletion) ,NoOp)

        (= (gtxn 2 TypeEnum) 4)
        (= (gtxn 2 AssetAmount) 0)
        (= (gtxn 2 AssetCloseTo) (global ZeroAddress))

        (= (gtxn 3 TypeEnum) 4)
        (= (gtxn 3 AssetAmount) 0)
        (= (gtxn 3 AssetCloseTo) (global ZeroAddress))))

;; <group-1>
;; (rec)  txn 0 pays fees
;; (any)  txn 1 pays out the bid in the hardwired app or fails if it cannot
;; (this) txn 2 pays out the token
(define sovauc-escrow-payout
  `(and (= (txn GroupIndex) 2)
        (= (txn RekeyTo) (global ZeroAddress))

        (= (gtxn 1 TypeEnum) 6)
        (= (gtxn 1 ApplicationID) (btoi (byte TMPL_APPID)))
        ;; (= (gtxn 1 NumAccounts) 1) ;; checked earlier
        (= (gtxn 1 NumAppArgs) 1)
        (= (gtxn 1 OnCompletion) ,NoOp)

        (= (gtxn 2 TypeEnum) 4)))

;; <group-2>
;; (rec)   txn 0 pays fees (recommended)
;; (admin) txn 1 destroys the app or fails
;; (this)  txn 2 and 3 move all leftover escrowed assets to the seller
;; (this)  txn 4 closes account out to seller
(define sovauc-escrow-close-auction
  `(and (< (txn GroupIndex) 5)
        (> (txn GroupIndex) 1)
        (= (txn RekeyTo) (global ZeroAddress))

        (= (gtxn 2 Sender) (gtxn 3 Sender))
        (= (gtxn 3 Sender) (gtxn 4 Sender))

        (= (gtxn 1 TypeEnum) 6)
        (= (gtxn 1 ApplicationID) (btoi (byte TMPL_APPID)))
        (= (gtxn 1 NumAccounts) 0)
        (= (gtxn 1 NumAppArgs) 2)
        (= (gtxn 1 OnCompletion) ,NoOp)

        (= (gtxn 2 TypeEnum) 4)
        (= (gtxn 2 AssetAmount) 0)

        (= (gtxn 3 TypeEnum) 4)
        (= (gtxn 3 AssetAmount) 0)

        (= (gtxn 4 TypeEnum) 1)
        (= (gtxn 4 Amount) 0)))

(define sovauc-escrow
  `(cond [(= (gtxn 1 NumAccounts) 1) ,sovauc-escrow-payout]
         [(= (gtxn 1 NumAppArgs) 1) ,sovauc-escrow-setup]
         [else ,sovauc-escrow-close-auction]))

(define (sovauc-compute-escrow escrow-suffix)
  `(sha512_256 (concat (byte "\"Program\"")
                       (byte TMPL_EPREFIX)
                       (itob (global CurrentApplicationID))
                       ,escrow-suffix)))

(define (sovauc-check-escrow escrow-suffix)
  `(= (sha512_256 ,escrow-suffix) (byte TMPL_ESUFFIXH)))

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
  (let ([divisor (new-scratch!)])
    `(begin
       (note "correct tranche size check")
       (store! ,divisor (+ (* ,lookback ,anchor ,supply-pct-hths)
                           (* ,num-tranches ,usdc-ring-sum)))

       (cond [(< ,tranche-index ,lookback)
              (= ,tranche-size ,init-tranches-size)]
             [(or (= ,usdc-ring-sum 0)
                  (<= (* ,supply ,supply-pct-hths) ,tranche-ring-sum))
              (= ,tranche-size ,min-tranche-size)]
             [(>= ,remainder (load ,divisor))
              0]

             [else ,(verify-128-bit-division `(* 2 ,usdc-ring-sum)
                                             `(- (* ,supply ,supply-pct-hths) ,tranche-ring-sum)
                                             `(load ,divisor)
                                             tranche-size
                                             remainder)]))))

(define (sovauc-payout-value-ok? payout bid remainder auction-raised tranche-supply)
  `(and (< ,remainder ,auction-raised)
        ,(verify-128-bit-division tranche-supply
                                  bid
                                  auction-raised
                                  payout
                                  remainder)))

;; (define (sovauc-usdc-ring-old-usdc)
;;   '(app-global-get (concat (byte 0x0001)
;;                             (itob (% (app-global-get tranche-index)
;;                                      (app-global-get lookback))))))

;; (define (sovauc-usdc-ring-new-usdc! val)
;;   `(app-global-put! (concat (byte 0x0001)
;;                             (itob (% (app-global-get tranche-index)
;;                                      (app-global-get lookback))))
;;                     ,val))

;; (define (sovauc-usdc-ring-old-tranche)
;;   '(app-global-get (concat (byte 0x0002)
;;                             (itob (% (app-global-get tranche-index)
;;                                      (app-global-get lookback))))))

;; (define (sovauc-usdc-ring-new-tranche! val)
;;   `(app-global-put! (concat (byte 0x0002)
;;                             (itob (% (app-global-get tranche-index)
;;                                      (app-global-get lookback))))
;;                     ,val))

(define (sovauc-update-rings!)
  (let ([index (new-scratch!)])
    `(begin
       (store! ,index (itob (% (app-global-get tranche-index) (app-global-get lookback))))

       (app-global-put! usdc-ring-sum
                        (+ (- (app-global-get usdc-ring-sum)
                              (app-global-get (concat (byte 0x0001) (load ,index))))
                           (app-global-get auction-raised)))

       (app-global-put! (concat (byte 0x0001) (load ,index))
                        (app-global-get auction-raised))

       (app-global-put! tranche-ring-sum
                        (+ (- (app-global-get tranche-ring-sum)
                              (app-global-get (concat (byte 0x0002) (load ,index))))
                           (app-global-get tranche-supply)))

       (app-global-put! (concat (byte 0x0002) (load ,index))
                        (app-global-get tranche-supply)))))

(define (asset-opted-in? acct asset)
  (let ([opted-in (new-scratch!)])
    `(begin
       (store! ,opted-in (asset_holding_get ,acct ,asset AssetBalance))
       (pop)
       (load ,opted-in))))

;; available actions (indexed by either (OnCompletion) or number of (accounts, arguments))
;; [E] represents an action that groups with an escrow
;; (0, 12)  -> create auction series
;; (optin)  -> opt into auction
;; (0, 1)   ->   open auction [E]     (when no open auctions)
;; (0, 0)   ->     enter bids         (before deadline)
;; (1, 1)   ->       payout bid [E]   (after deadline)
;; (1, 0)   ->       destroy bid      (after deadline)
;; (clear)  ->      forefeit bid
;; (0, 2)   ->   close auction [E]    (after deadline)
;; (delete) -> destroy auction series (when last auction has closed)
(define sovauc-application
  ;; const; set at initialization
  `((gvars (admin  (addr ax))
           (seller (addr sx))

           (bid-asset  (int b$))
           (sale-asset (int s$))

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
           ;; TODO change the rings so that they're an array of size X
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
     (cond
       [(= (txn ApplicationID) 0)
        (note "create")
        (with ([args (init-seller

                      init-bid-asset
                      init-sale-asset

                      init-anchor
                      init-num-tranches
                      init-supply
                      init-supply-pct-hths
                      init-init-tranches-size
                      init-lookback
                      init-min-tranche-size
                      init-auction-duration

                      escrow-suffix)])

              (app-global-put! admin (txn Sender))
              (app-global-put! seller init-seller)
              (app-global-put! escrow ,(sovauc-compute-escrow 'escrow-suffix))

              (app-global-put! bid-asset (btoi init-bid-asset))
              (app-global-put! sale-asset (btoi init-sale-asset))

              (app-global-put! anchor (btoi init-anchor))
              (app-global-put! num-tranches (btoi init-num-tranches))
              (app-global-put! supply (btoi init-supply))
              (app-global-put! supply-pct-hths (btoi init-supply-pct-hths))
              (app-global-put! init-tranches-size (btoi init-init-tranches-size))
              (app-global-put! lookback (btoi init-lookback))
              (app-global-put! min-tranche-size (btoi init-min-tranche-size))
              (app-global-put! auction-duration (btoi init-auction-duration))

              (and ,(sovauc-check-escrow 'escrow-suffix)
                   (= (txn ApplicationID) 0)
                   (= (txn NumAppArgs) 12)
                   (= (txn NumAccounts) 0)
                   (= (txn OnCompletion) ,NoOp)))]

       [(or (= (txn OnCompletion) (int ,OptIn))
            (= (txn OnCompletion) (int ,DeleteApplication)))
        (note "optin/destroy")
        (and (= (txn NumAppArgs) 0)
             (= (txn NumAccounts) 0)
             (or (= (txn OnCompletion) (int ,OptIn))
                 (= (app-global-get tranche-index) (app-global-get num-tranches))))]

       [else
        (assert (= (txn OnCompletion) (int ,NoOp)))

        (cond
          [(= (txn NumAccounts) 1)
           (note "payout/invalidate bid")
           (with ([accs (bidder)]
                  [args (remainder)])

                 (unless (= (txn NumAppArgs) 0)
                   (assert ,(sovauc-payout-value-ok?
                             '(gtxn 2 AssetAmount)
                             '(app-local-get-acct bidder bid-receipts)
                             '(btoi remainder)
                             '(app-global-get auction-raised)
                             '(app-global-get tranche-supply))))

                 (app-global-put! outstanding-receipts
                                  (- (app-global-get outstanding-receipts)
                                     (app-local-get-acct bidder bid-receipts)))
                 (app-local-del-acct! bidder bid-receipts)

                 (and (not (< (global LatestTimestamp) (app-global-get auction-deadline)))

                      ;; payout
                      ;; <group-1>
                      ;; (rec)  txn 0 pays fees
                      ;; (this) txn 1 pays out the bid in the hardwired app or fails if it cannot
                      ;; (escr) txn 2 pays out the token
                      (if (= (txn NumAppArgs) 1)
                          (and (= (txn GroupIndex) 1)

                               (= (gtxn 2 Sender) (app-global-get escrow))
                               (= (gtxn 2 XferAsset) (app-global-get sale-asset))
                               (= (gtxn 2 AssetReceiver) bidder))

                          ;; invalidate
                          (and (= (txn NumAppArgs) 0)
                               (= ,(asset-opted-in? 'bidder '(app-global-get sale-asset)) 0)))))]

          [else
           (assert (and (= (txn NumAccounts) 0)
                        (< (txn NumAppArgs) 3)))

           (cond
             [(= (txn NumAppArgs) 1)
              (note "open auction")
              (with ([args (remainder)])
                    (assert (and (= (app-global-get auction-deadline) 0)
                                 (< (app-global-get tranche-index)
                                    (app-global-get num-tranches))))

                    (app-global-put! tranche-supply (gtxn 4 AssetAmount))
                    (app-global-put! auction-deadline
                                     (+ (global LatestTimestamp)
                                        (app-global-get auction-duration)))

                    ;; <group-0>
                    ;; (rec)        txn 0 pays fees and fills minbalance
                    ;; (this=admin) txn 1 opens auction
                    ;; (escr)       txn 2 and 3 opt into tokens
                    ;; (any)        txn 4 funds auction with tranche size
                    (and (= (txn GroupIndex) 1)
                         (= (txn Sender) (app-global-get admin))

                         (= (gtxn 2 Sender) (app-global-get escrow))
                         (= (gtxn 2 XferAsset) (app-global-get bid-asset))

                         (= (gtxn 3 XferAsset) (app-global-get sale-asset))

                         (= (gtxn 4 TypeEnum) 4)
                         (= (gtxn 4 XferAsset) (app-global-get sale-asset))
                         (= (gtxn 4 AssetReceiver) (app-global-get escrow))
                         ,(sovauc-correct-tranche-size?
                           '(gtxn 4 AssetAmount)
                           '(btoi remainder)

                           '(app-global-get anchor)
                           '(app-global-get tranche-index)
                           '(app-global-get num-tranches)
                           '(app-global-get init-tranches-size)
                           '(app-global-get supply)
                           '(app-global-get supply-pct-hths)

                           '(app-global-get lookback)
                           '(app-global-get min-tranche-size)
                           '(app-global-get tranche-ring-sum)
                           '(app-global-get usdc-ring-sum))))]

             [(= (txn NumAppArgs) 2)
              ;; both arguments are ignored
              (note "close auction")

              (assert (not (< (global LatestTimestamp) (app-global-get auction-deadline))))

              ,(sovauc-update-rings!)

              (app-global-put! tranche-index (+ 1 (app-global-get tranche-index)))

              (app-global-del! auction-raised)
              (app-global-del! tranche-supply)
              (app-global-del! auction-deadline)
              ;; <group-2>
              ;; (rec)        txn 0 pays fees (recommended)
              ;; (this=admin) txn 1 destroys the app or fails
              ;; (escrow)     txn 2 and 3 move all leftover escrowed assets to the seller
              ;; (escrow)     txn 4 closes account out to seller
              (and (= (txn GroupIndex) 1)
                   (= (txn Sender) (app-global-get admin))
                   (= (app-global-get outstanding-receipts) 0)

                   (= (gtxn 2 Sender) (app-global-get escrow))

                   (= (gtxn 2 XferAsset) (app-global-get bid-asset))
                   (= (gtxn 2 AssetCloseTo) (app-global-get seller))

                   (= (gtxn 3 XferAsset) (app-global-get sale-asset))
                   (= (gtxn 3 AssetCloseTo) (app-global-get seller))

                   (= (gtxn 4 CloseRemainderTo) (app-global-get seller)))]

             ;; <group-4>
             ;; (any)  txn 0 deposits into escrow
             ;; (this) txn 1 puts receipts into bidder
             [else
              (note "enter bid")

              (app-local-put! 0 bid-receipts (+ (app-local-get 0 bid-receipts)
                                                (gtxn 0 AssetAmount)))
              (app-global-put! outstanding-receipts
                               (+ (app-global-get outstanding-receipts)
                                  (gtxn 0 AssetAmount)))
              (app-global-put! auction-raised
                               (+ (app-global-get auction-raised)
                                  (gtxn 0 AssetAmount)))

              (and (< (global LatestTimestamp) (app-global-get auction-deadline))

                   (= (txn GroupIndex) 1)

                   (= (gtxn 0 TypeEnum) 4)
                   (= (gtxn 0 XferAsset) (app-global-get bid-asset))
                   (= (gtxn 0 AssetReceiver) (app-global-get escrow)))])])]))

    (onclear (begin
               (app-global-put! outstanding-receipts
                                (- (app-global-get outstanding-receipts)
                                   (app-local-get 0 bid-receipts)))
               1))))
