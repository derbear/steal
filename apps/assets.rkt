#lang racket

(require "lang.rkt")
(require net/base64)

(provide asset-application)

(define next-scratch 0)
(define (new-scratch!)
  (begin
    (let ([c next-scratch])
      (set! next-scratch (+ c 1))
      c)))

(define (valid-address? arg)
  `(= (len ,arg) 32))

(define (asset-valid-configure? key val)
  `(or (= ,val (global ZeroAddress))
       (not (= (app-global-gets ,key) (global ZeroAddress)))))

(define (asset-lget addr key)
  (if (equal? addr '(txn Sender))
      `(app-local-gets 0 ,key)
      `(app-local-gets-acct ,addr ,key)))

(define (asset-lset! addr key val)
  (if (equal? addr '(txn Sender))
      `(app-local-put! 0 ,key ,val)
      `(app-local-put-acct! ,addr ,key ,val)))

(define (asset-frozen? addr)
  `(if (= ,addr (app-global-gets cr))
       (= (app-global-gets fz) 1)
       (= ,(asset-lget addr 'fz) 1)))

;; fails and returns 0 if addr is frozen unless bypass set
(define (asset-modify! addr amt bypass op)
  (let ([ifblock
         `(if (= ,addr (app-global-gets cr))
              (app-global-put! bl (,op (app-global-gets bl) ,amt))
              ,(asset-lset! addr 'bl `(,op ,(asset-lget addr 'bl) ,amt)))])
    (if bypass
        ifblock
        `(begin
           (unless (= ,amt 0)
             (note "cannot modify frozen asset")
             (assert (not ,(asset-frozen? addr)))
             ,ifblock)))))

(define (asset-take-out! addr amt bypass)
  (asset-modify! addr amt bypass '-))
(define (asset-put-in! addr amt bypass)
  (asset-modify! addr amt bypass '+))

;; fails and returns 0 if trying to take out or put into a frozen address unless bypass set or amt is 0
(define (asset-move! snd rcv amt bypass)
  (let ([mem (new-scratch!)])
    `(begin
       (store! ,mem ,amt)
       ,(asset-take-out! snd `(load ,mem) bypass)
       ,(asset-put-in! rcv `(load ,mem) bypass))))

(define asset-application
  ;; TODO inject these?
  `((params (int decimals)
            (int unitname)
            (string assetname)
            (string url)
            (byte base64 metadatahash))

    ;; TODO allow preprocessor to substitute abbrevs.
    (gvars (addr cr) ;; creator, const
           (int tt)  ;; total supply, const
           (int df)  ;; default frozen, const
           (addr mn) ;; manager
           (addr rv) ;; reserve
           (addr fr) ;; frezer
           (addr cl) ;; clawback
           (int bl)  ;; balance, creator
           (int fz)) ;; frozen, creator

    (lvars (int bl)  ;; balance
           (int fz)) ;; frozen

    ;; TODO automatically compute numargs/numaccs from context
    (prog
     (if (= (txn NumAppArgs) 7)
         (with ([args (new-mn new-rv new-fr new-cl init-cr init-tt init-df)])
               (note "asset configuration")
               (if (= (txn ApplicationID) 0)
                   (begin
                     (app-global-put! cr init-cr)
                     (app-global-put! tt (btoi init-tt))
                     (app-global-put! bl (btoi init-tt))
                     (app-global-put! df (btoi init-df)))
                   (assert (and (= (txn Sender) (app-global-gets mn)) ;; reconfigure
                                ,(asset-valid-configure? 'mn 'new-mn)
                                ,(asset-valid-configure? 'rv 'new-rv)
                                ,(asset-valid-configure? 'fr 'new-fr)
                                ,(asset-valid-configure? 'cl 'new-cl))))
               (app-global-put! mn new-mn)
               (app-global-put! rv new-rv)
               (app-global-put! fr new-fr)
               (app-global-put! cl new-cl)
               (and (= (txn NumAccounts) 0)
                    (= (txn OnCompletion) ,NoOp)
                    ,(valid-address? 'new-mn)
                    ,(valid-address? 'new-rv)
                    ,(valid-address? 'new-fr)
                    ,(valid-address? 'new-cl)))
         (begin
           (assert (not (= (txn ApplicationID) 0)))
           (cond
             [(= (txn NumAccounts) 0)
              (note "asset deletion or opt-in")

              (when (= (txn OnCompletion) ,OptIn)
                (note "opting in to implicit zero bl")
                (app-local-put! 0 fz (app-global-gets df)))

              (and (= (txn NumAppArgs) 0)
                   (or (and (= (txn OnCompletion) ,DeleteApplication)
                            (= (txn Sender) (app-global-gets mn))
                            (= (app-global-gets tt) (app-global-gets bl)))
                       (and (= (txn OnCompletion) ,OptIn)
                            (not (= (txn Sender) (app-global-gets cr))))))]

             [(= (txn NumAccounts) 1)
              (note "freeze asset holding")
              (with ([accs (account)]
                     [args (new-fz)])
                    (if (= account (app-global-gets cr))
                        (app-global-put! fz (btoi new-fz))
                        (app-local-put-acct! account fz (btoi new-fz)))
                    (and (= (txn NumAppArgs) 1)
                         (= (txn OnCompletion) ,NoOp)
                         (= (txn Sender) (app-global-gets fr))))]

             [(= (txn NumAppArgs) 2)
              (note "clawback asset")
              (with ([accs (sender receiver)]
                     [args (amount cl-ignored)])
                    ,(asset-move! 'sender 'receiver '(btoi amount) #t)
                    (and (= (txn NumAccounts) 2)
                         (= (txn OnCompletion) ,NoOp)
                         (= (txn Sender) (app-global-gets cl))))]

             [else
              (note "transfer asset")
              (with ([accs (receiver closeto)]
                     [args (amount)])
                    ,(asset-move! '(txn Sender) 'receiver '(btoi amount) #f)
                    (unless (= closeto (global ZeroAddress))
                      ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) 'bl) #f))
                    (and (= (txn NumAppArgs) 1)
                         (= (txn NumAccounts) 2)
                         (if (= (txn OnCompletion) ,NoOp)
                             (= closeto (global ZeroAddress))
                             (and (= (txn OnCompletion) ,CloseOut)
                                  (= ,(asset-lget '(txn Sender) 'bl) 0)
                                  (not (= closeto (global ZeroAddress)))))))]))))

    (onclear (begin
               (app-global-put! bl (+ (app-global-gets bl) (app-local-gets 0 bl)))
               1))))
