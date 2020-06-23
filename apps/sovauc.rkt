#lang racket

(require racket/pretty)
(require racket/trace)

(require "lang.rkt")

(provide sovauc-application)
(provide sovauc-escrow)

(define sovauc-next-identifier 0)
(define (sovauc-id!)
  (let ([id sovauc-next-identifier])
    (set! sovauc-next-identifier (+ 1 sovauc-next-identifier))
    id))

(define sovauc-create (sovauc-id!))
(define sovauc-init-escrow (sovauc-id!))
(define sovauc-optin (sovauc-id!))
(define sovauc-open-auction (sovauc-id!))
(define sovauc-enter-bid (sovauc-id!))
(define sovauc-discharge-bid (sovauc-id!))
(define sovauc-close-auction (sovauc-id!))
(define sovauc-destroy (sovauc-id!))

;; (rec)  txn 0 pays fees and/or fills minbalance
;; (this) txn 1 opts in
(define sovauc-escrow-setup
  '(and (= (txn TypeEnum) 4)
        ;; (= (txn RekeyTo) (global ZeroAddress))
        ;; ignore fee; account is "ephemeral"
        (or (= (txn XferAsset) (int TMPL_SOV))
            (= (txn XferAsset) (int TMPL_USDC)))
        (= (txn AssetAmount) 0)))

;; (rec)  txn 0 pays fees
;; (any)  txn 1 discharges the bid in the hardwired app (and fails if it cannot)
;; (this) txn 2 pays out the token
(define sovauc-escrow-payout
  `(and (= (txn GroupIndex) 2)

        (= (txn TypeEnum) 4)
        (= (txn XferAsset) (int TMPL_SOV))
        (= (txn AssetSender) (global ZeroAddress))
        (= (txn AssetCloseTo) (global ZeroAddress))
        ;; (= (txn RekeyTo) (global ZeroAddress))

        (= (gtxn 1 TypeEnum) 5)
        (= (gtxn 1 ApplicationID) (btoi (byte TMPL_APPID)))
        (= (gtxn 1 Note) ,sovauc-discharge-bid)))
        ;; (= (gtxn 1 ApplicationArgs 0) ,sovauc-discharge-bid))) ;; TODO replace

;; TODO (see below) may need way to destroy bid without paying out,
;; if receiver had not opted in. bids should be nontransferrable
;; to prevent bid-shuffling DoS attacks

;; (satisfies-xchange-rate? (gtxn 1 ApplicationArgs 1) (txn AssetAmount))))
;; enforced by app

;; (rec)  txn 0 pays fees (recommended)
;; (auth) txn 1 notifies the hardwired app to close the auction (and fails if it cannot)
;; (this) txn 2 and 3 move all leftover escrowed assets to the auction owner
;; (this) txn 4 closes account out to auction owner
(define sovauc-escrow-close-auction
  `(and (< (txn GroupIndex) 5)
        (> (txn GroupIndex) 1)
        ;; (= (txn RekeyTo) (global ZeroAddress))
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

;; TODO need scaling factors to handle different quanta
;; expects 4-decimal-digits of precision 
(define (sovauc-current-tranche-size tranche-index      ; exact
                                     anchor             ; exact
                                     num-tranches       ; exact
                                     init-tranches-size ; exact
                                     supply             ; exact
                                     supply-pct-hths    ; exact
                                     lookback           ; exact
                                     tranche-ring-sum   ; 4 digits prec
                                     usdc-ring-sum      ; 4 digits prec (6 total; 2 cents + 4 rest)
                                     min-tranche-size)  ; exact
  `(cond [(< ,tranche-index ,lookback)
          ,init-tranche-size]
         [(= ,usdc-ring-sum 0) ,min-tranche-size]
         [(< (* (/ ,supply 10000) ,supply-pct-hths) ,tranche-ring-sum) ,min-tranche-size]

         ;; TODO run all checks at compile time
         ;; note: both num and denom multiplied by 100 to reduce accuracy loss
         ;; (i.e., scaling ratio between usdc and supply)
         ;; note: could divide by 2; num-tranches typically even?
         ;; loss from (* num-tranches usdc-ring-sum) probably small...
         [else (/ (* 2 ,usdc-ring-sum (- (* ,lookback (/ ,supply 10000) ,supply-pct-hths)
                                         (* ,lookback ,tranche-ring-sum)))
                  (+ (* ,lookback (/ ,anchor 100) ,supply-pct-hths)
                     (* ,num-tranches ,usdc-ring-sum)))]))

(define (sovauc-usdc-ring-old-usdc)
  '(app-global-gets (concat (byte 0x0001)
                            (itob (% (app-global-gets tranche-index)
                                     (app-global-gets lookback))))))
      
(define (sovauc-usdc-ring-new-usdc! val)
  `(app-global-put! (concat (byte 0x0001)
                            (itob (% (app-global-gets tranche-index)
                                     (app-global-gets lookback))))
                    ,val))

(define (sovauc-usdc-ring-old-tranche)
  '(app-global-gets (concat (byte 0x0002)
                            (itob (% (app-global-gets tranche-index)
                                     (app-global-gets lookback))))))
      
(define (sovauc-usdc-ring-new-tranche! val)
  `(app-global-put! (concat (byte 0x0002)
                            (itob (% (app-global-gets tranche-index)
                                     (app-global-gets lookback))))
                    ,val))

(define (sovauc-value->num-bids val) `(* ,val 1)) ;; TODO

(define sovauc-application
  ;; const; set at initialization
  `((gvars (anchor            (int ac))
           (num-tranches      (int nt))
           (init-tranche-size (int fs))
           (supply            (int sp))
           (supply-pct-hths   (int rs)) ; fixed-pt, quantized to .01%
           (lookback          (int lk))
           (auction-duration  (int dr))
           (min-tranche-size  (int ms))

           ;; set const; set at initialization second stage
           (escrow (addr es))

           ;; updates per auction
           ;; TODO change the rings so that they're an array of size 10
           (usdc-raised-ring  (int ur))
           (tranche-size-ring (int tz))
           (tranche-index     (int ti))

           ;; caches
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
                  (with ([args (init-auction-duration
                                init-num-tranches
                                init-supply
                                init-begin-percent-num
                                init-begin-percent-denom
                                init-supply-percent-num
                                init-supply-percent-denom)])
                        (app-global-put! auction-duration init-auction-duration)
                        (app-global-put! num-tranches init-num-tranches)
                        (app-global-put! supply init-supply)
                        (app-global-put! begin-percent-num init-begin-percent-num)
                        (app-global-put! begin-percent-denom init-begin-percent-denom)
                        (app-global-put! supply-percent-num init-supply-percent-num)
                        (app-global-put! supply-percent-denom init-supply-percent-denom)

                        (and (= (txn ApplicationID) 0)
                             (= (txn NumAppArgs) 8)
                             (= (txn NumAccounts) 0)
                             (= (txn OnCompletion) (int noop))))]

                 [(= calltype ,sovauc-init-escrow)
                  (note "init escrow")
                  (with ([args (init-escrow escrow-prefix escrow-suffix)])
                        (app-global-put! escrow init-escrow)
                        (and ,(sovauc-check-escrow 'init-escrow 'escrow-suffix)
                             (not (= (txn ApplicationID) 0))
                             (= (txn NumAppArgs) 4)
                             (= (txn NumAccounts) 0)
                             (= (txn OnCompletion) (int noop))))]

                 [(= calltype ,sovauc-optin)
                  (note "optin")
                  (and (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int optin)))]

                 [(= calltype ,sovauc-destroy)
                  (note "destroy")
                  (and (not (= (txn ApplicationID) 0))
                       (= (app-global-gets tranche-index) (app-global-gets num-tranches))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int delete)))]

                 ;; (auth)        txn 0 funds escrow with tranche size
                 ;; (this = auth) txn 1 sets open auction state
                 [(= calltype ,sovauc-open-auction)
                  (note "open auction")
                  (assert (and (= (app-global-gets auction-deadline) 0)
                               (< (app-global-gets tranche-index) (app-global-gets num-tranches))))

                  (app-global-put! tranche-supply (gtxn 0 AssetAmount))
                  (app-global-put! auction-deadline
                                   (+ (global LatestTimestamp)
                                      (app-global-gets auction-duration)))

                  (and (= (txn GroupIndex) 1)
                       (= (txn Sender) (gtxn 0 Sender))
                       (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int noop))

                       (= (gtxn 0 TypeEnum) 4)
                       (= (gtxn 0 XferAsset) (int TMPL_SOV))
                       (= (gtxn 0 AssetAmount)
                          ,(sovauc-current-tranche-size
                            '(app-global-gets tranche-index)
                            '(app-global-gets anchor)
                            '(app-global-gets num-tranches)
                            '(app-global-gets init-tranche-size)
                            '(app-global-gets supply)
                            '(app-global-gets supply-pct-hths)
                            '(app-global-gets lookback)
                            '(app-global-gets tranche-ring-sum)
                            '(app-global-gets usdc-ring-sum)
                            '(app-global-gets min-tranche-size)))
                       (= (gtxn 0 AssetReceiver) (app-global-gets escrow))
                       (= (gtxn 0 AssetCloseTo) (global ZeroAddress))
                       ;; ensures (txn Sender) is clawback address
                       ;; TODO having clawback with SOV token is strange--
                       ;; perhaps instead have separate field of auction admin?
                       ;; maybe will have separate authorization mechanism...
                       (not (= (gtxn 0 AssetSender) (global ZeroAddress))))]

                 ;; (any)  txn 0 deposits into escrow
                 ;; (this) txn 1 puts receipts into bidder
                 [(= calltype ,sovauc-enter-bid)
                  (note "enter bid")
                  (app-local-put! 0 bid-receipts (+ (app-local-gets 0 bid-receipts)
                                                    (gtxn 0 AssetAmount)))
                  (app-global-put! outstanding-receipts
                                   (+ (app-global-gets outstanding-receipts)
                                      (gtxn 0 AssetAmount)))
                  (app-global-put! auction-raised
                                   (+ (app-global-gets auction-raised)
                                      (gtxn 0 AssetAmount)))

                  (and (= (txn GroupIndex) 1)
                       (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 1)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int noop))
                       (< (global LatestTimestamp) (app-global-gets auction-deadline))

                       (= (gtxn 0 TypeEnum) 4)
                       (= (gtxn 0 XferAsset) (int TMPL_USDC))
                       (= (gtxn 0 AssetReceiver) (app-global-gets escrow))
                       (= (gtxn 0 AssetSender) (global ZeroAddress)))]

                 [(= calltype ,sovauc-discharge-bid)
                  (note "discharge bid")
                  (app-local-put! bid-receipts
                                  (- (app-local-gets 0 bid-receipts)
                                     ,(sovauc-value->num-bids '(gtxn 0 AssetAmount))))
                  (app-global-put! outstanding-receipts
                                  (- (app-global-gets outstanding-receipts)
                                     ,(sovauc-value->num-bids '(gtxn 0 AssetAmount))))

                  ;; TODO need a way to force receipt discharge?
                  ;; could do: can burn money iff receiver did not opt in...

                  ;; TODO the principal for this is wrong: it must be <anyone>,
                  ;; and the bidder must be specified as an account argument
                  ;; otherwise, a bidder could simply refuse to cash in his receipt
                  ;; because of this, this transaction must check that the receiver
                  ;; of the earlier payment was the account argument

                  ;; TODO must check that auction deadline has passed

                  (and (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 0)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int noop)))]

                 [(= calltype ,sovauc-close-auction)
                  (note "close auction")
                  (assert (and (= (app-global-gets outstanding-receipts) 0)
                               (not (< (global LatestTimestamp) (app-global-gets auction-deadline)))))

                  (app-global-put! usdc-ring-sum
                                   (+ (- (app-global-gets usdc-ring-sum)
                                         ,(sovauc-usdc-ring-old-usdc))
                                      (app-global-gets auction-raised)))
                  ,(sovauc-usdc-ring-new-usdc! (app-global-gets auction-raised))

                  (app-global-put! tranche-ring-sum
                                   (+ (- (app-global-gets tranche-ring-sum)
                                         ,(sovauc-usdc-ring-old-tranche))
                                      (app-global-gets tranche-supply)))
                  ,(sovauc-usdc-ring-new-tranche! (app-global-gets tranche-supply))

                  (app-global-put! tranche-index (+ 1 (app-global-gets tranche-index)))

                  (app-global-del! auction-raised)
                  (app-global-del! tranche-supply)
                  (app-global-del! auction-deadline)

                  (and (not (= (txn ApplicationID) 0))
                       (= (txn NumAppArgs) 0)
                       (= (txn NumAccounts) 0)
                       (= (txn OnCompletion) (int noop)))]

                 [else 0])))
                       
    ;; TODO modify auction-raised to drop bid? probably unnecessary since funds committed are lost
    (onclear (begin
               (app-global-put! outstanding-receipts
                                (- (app-global-gets outstanding-receipts)
                                   (app-local-gets 0 bid-receipts)))
               1))))

;; (define marshall-islands 180000000)
;; (define ten18 10) ;; TODO

;; (define (tranche-size-ok? tranche-size
;;                           supply
;;                           auction-index
;;                           init-num init-denom
;;                           scale-num scale-denom
;;                           total-tranche-size
;;                           total-raised
;;                           num-auctions)
;;   (if (= auction-index 0)
;;       (=w (*w tranche-size init-denom) (*w supply init-num))
;;       (=w (*w 2
;;               tranche-size
;;               (+w (*w ,ten18 auction-index ,marshall-islands rs-TODO)
;;                  (*w num-auctions total-raised)))
;;           (*w total-raised
;;               (+ total-tranche-size (* supply rs-TODO))))))

(define avg-raise `(/ total-raised auction-index))
(define num `(- (/ (* supply scale-num) scale-denom) total-tranche-size))
(define a `(/ (* 180000000 scale-num) scale-denom))
(define b `(/ (/ (* num-auctions total-raised) auction-index) 42))
(define den `(/ (+ ,a ,b) 2))
(define num-tokens `(/ (* ,num ,avg-raise) ,den))
(define tranche-size `(/ ,num-tokens 42))

(define eqn `(= tranche-size ,tranche-size))

(define (rm-div-r eqn)
  (let ([left (second eqn)]
        [right (third eqn)])
    (let ([num (second right)]
          [denom (third right)])
      `(= (* ,left ,denom) ,num))))

(define (rm-2div expr)
  (let ([num (second (second expr))]
        [d2 (third expr)]
        [d1 (third (second expr))])
    `(/ ,num (* ,d1 ,d2))))

(define (swap-*/ expr)
  (let ([a (second expr)]
        [b (second (third expr))]
        [d (third (third expr))])
    `(/ (* ,a ,b) ,d)))

(define (factor-/ expr)
  (let ([op (first expr)]
        [rterm (third expr)]
        [num (second (second expr))]
        [denom (third (second expr))])
    `(/ (,op ,num (* ,denom ,rterm)) ,denom)))

(define (simpl-expr-l expr)
  (cond [(and (list? expr)
              (eq? (first expr) '/)
              (list? (second expr))
              (eq? (first (second expr)) '/))
         (rm-2div expr)]

        [(and (list? expr)
              (eq? (first expr) '*)
              (list? (third expr))
              (eq? (first (third expr)) '/))
         (swap-*/ expr)]

        [(and (list? expr)
              (or (eq? (first expr) '+)
                  (eq? (first expr) '-))
              (list? (second expr))
              (eq? (first (second expr)) '/))
         (factor-/ expr)]
              
              
        [(and (list? expr) (list? (second expr)) (list? (third expr)))
         `(,(first expr) ,(simpl-expr (second expr)) ,(simpl-expr (third expr)))]
        [(and (list? expr) (list? (second expr)))
         `(,(first expr) ,(simpl-expr (second expr)) ,(third expr))]
        [(and (list? expr) (list? (third expr)))
         `(,(first expr) ,(second expr) ,(simpl-expr (third expr)))]
        [else expr]))

(define (simpl-expr-r expr)
  (if (and (list? expr)
           (or (eq? (first expr) '+)
               (eq? (first expr) '*)))
      (simpl-expr-l (list (first expr) (third expr) (second expr)))
      expr))

(define (simpl-expr expr) (simpl-expr-r (simpl-expr-l expr)))

(define (simpl-l eqn)
  (cond [(and (list? (third eqn)) (eq? (first (third eqn)) '/)) (rm-div-r eqn)]
        [else `(= ,(simpl-expr (second eqn)) ,(simpl-expr (third eqn)))]))

(define (simpl-r eqn)
  (let ([eqn* (simpl-l (list (first eqn) (third eqn) (second eqn)))])
    (list (first eqn) (third eqn*) (second eqn*))))

(define (simpl eqn) (simpl-r (simpl-l eqn)))

(define (simpl* eqn)
  (let ([eqn* (simpl eqn)])
    (if (equal? eqn eqn*)
        eqn
        (simpl* eqn*))))

(define (collapse expr)
  (if (list? expr)
      (let ([redl (collapse (second expr))]
            [redr (collapse (third expr))])
        (cond [(and (or (eq? (first expr) '*) (eq? (first expr) '+))
                    (list? redl)
                    (eq? (first redl) (first expr))
                    (list? redr)
                    (eq? (first redr) (first expr)))
               (append (list (first expr))
                       (rest redl)
                       (rest redr))]

              [(and (or (eq? (first expr) '*) (eq? (first expr) '+))
                    (list? redl)
                    (eq? (first redl) (first expr)))
               (append (list (first expr))
                       (rest redl)
                       (list redr))]

              [(and (or (eq? (first expr) '*) (eq? (first expr) '+))
                    (list? redr)
                    (eq? (first redr) (first expr)))
               (append (list (first expr))
                       (list redl)
                       (rest redr))]
              
              [else (list (first expr) redl redr)]))
      expr))

;; (pretty-print eqn)
;; (pretty-print (simpl* eqn))
;; (pretty-print (collapse (simpl* eqn)))

;; after common factor elim
;; '(=
;;   (*
;;    tranche-size
;;    (+
;;     (* scale-denom num-auctions total-raised)
;;     (* auction-index scale-num 180000000 42)))
;;   (*
;;    2
;;    (- (* supply scale-num) (* scale-denom total-tranche-size))
;;    total-raised))
