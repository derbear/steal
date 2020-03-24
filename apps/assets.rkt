#lang racket

(require "lang.rkt")
(require net/base64)

(provide asset-application)

(define asset-enum 0)
(define (asset-enum-next!)
  (begin
    (let ([c asset-enum])
      (set! asset-enum (+ c 1))
      c)))

(define asset-configure (asset-enum-next!))
(define asset-delete (asset-enum-next!))
(define asset-open (asset-enum-next!))
(define asset-clawback (asset-enum-next!))
(define asset-transfer (asset-enum-next!))
(define asset-freeze (asset-enum-next!))

(define (valid-address? arg)
  `(= (len ,arg) 32))

(define (asset-valid-configure? key val)
  `(or (= ,val (global ZeroAddress))
       (not (= (app-global-get ,key) (global ZeroAddress)))))

(define (asset-lget addr key)
  (if (equal? addr '(txn Sender))
      `(app-local-get 0 0 ,key)
      `(app-local-get-acct ,addr 0 ,key)))

(define (asset-lset! addr key val)
  (if (equal? addr '(txn Sender))
      `(app-local-put! 0 ,key ,val)
      `(app-local-put-acct! ,addr ,key ,val)))

(define (asset-frozen? addr)
  `(or (and (= ,addr creator)
            (= (app-global-get cfrozen) 1))
       (and (not (= ,addr creator))
            (= ,(asset-lget addr 'frozen) 1))))

;; fails and returns 0 if addr is frozen unless bypass set
(define (asset-modify! addr amt bypass op)
  (let ([ifblock
         `(if (= ,addr creator)
              (app-global-put! cbalance (,op (app-global-get cbalance) ,amt))
              ,(asset-lset! addr 'balance `(,op ,(asset-lget addr 'balance) ,amt)))])
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

;; fails and returns 0 if trying to take out or put into a frozen address unless bypass set
(define (asset-move! snd rcv amt bypass)
  `(begin
     ,(asset-take-out! snd amt bypass)
     ,(asset-put-in! rcv amt bypass)))

(define asset-application
  ;; TODO inject these into program text
  `((params (int supply)
            (int decimals)
            (int defaultfrozen)
            (int unitname)
            (string assetname)
            (string url)
            (byte base64 metadatahash)
            (addr creator))

    (gvars (addr manager)
           (addr reserve)
           (addr freezer)
           (addr clawback)
           (int cbalance)
           (int cfrozen))

    (lvars (int balance)
           (int frozen))

    ;; TODO automatically compute numargs/numaccs from context
    (prog
     (with ([args (proc)])
           (cond

             [(= (btoi proc) ,asset-configure)
              (note "asset configuration")
              (with ([args (new-manager new-reserve new-freezer new-clawback)])
                    (assert (and (= (txn NumAppArgs) 5)
                                 (= (txn NumAccounts) 0)
                                 (= (txn OnCompletion) ,NoOp)))
                    (assert (and ,(valid-address? 'new-manager)
                                 ,(valid-address? 'new-reserve)
                                 ,(valid-address? 'new-freezer)
                                 ,(valid-address? 'new-clawback)))

                    (if (= (txn ApplicationID) 0)
                        (assert (= creator (txn Sender))) ;; create
                        (assert (and (= (txn Sender) (app-global-get manager)) ;; reconfigure
                                     ,(asset-valid-configure? 'manager 'new-manager)
                                     ,(asset-valid-configure? 'reserve 'new-reserve)
                                     ,(asset-valid-configure? 'freezer 'new-freezer)
                                     ,(asset-valid-configure? 'clawback 'new-clawback))))

                    (when (= (txn ApplicationID) 0)
                      (app-global-put! cbalance (int TMPL_SUPPLY))
                      (app-global-put! cfrozen 0)) ;; TODO can optimize away
                    (app-updates ((gvars (manager new-manager)
                                         (reserve new-reserve)
                                         (freezer new-freezer)
                                         (clawback new-clawback)))))]

             [(= (btoi proc) ,asset-delete)
              (and (not (= (txn ApplicationID) 0))
                   (= (txn NumAppArgs) 1)
                   (= (txn NumAccounts) 0)
                   (= (txn OnCompletion) ,DeleteApplication)
                   (= (txn Sender) (app-global-get manager))
                   (= supply (app-global-get cbalance)))]

             [(= (btoi proc) ,asset-open)
              (note "open asset holding")
              (assert (and (not (= (txn ApplicationID) 0))
                           (= (txn NumAppArgs) 1)
                           (= (txn NumAccounts) 0)
                           (= (txn OnCompletion) ,OptIn)
                           (not (= (app-opted-in 0) 1)) ;; TODO 0 -> (txn Sender)
                           (not (= (txn Sender) creator))))

              ;; TODO can (conditionally) optimize block away
              (app-updates ((lvars (balance 0)
                                   (frozen defaultfrozen))))]

             [(= (btoi proc) ,asset-clawback)
              (note "clawback asset")
              (with ([accs (sender receiver)]
                     [args (amount)])
                    (assert (and (not (= (txn ApplicationID) 0))
                                 (= (txn NumAppArgs) 2)
                                 (= (txn NumAccounts) 2)
                                 (= (txn OnCompletion) ,NoOp)
                                 (= (txn Sender) clawback)))
                    ,(asset-move! 'sender 'receiver '(btoi amount) #t)
                    1)]

             ;; note: since creator cannot optin, creator cannot close
             [(= (btoi proc) ,asset-transfer)
              (note "transfer asset holding")
              (with ([accs (receiver closeto)]
                     [args (amount)])
                    (assert (and (not (= (txn ApplicationID) 0))
                                 (= (txn NumAppArgs) 2)
                                 (= (txn NumAccounts) 2)
                                 (or (and (= (txn OnCompletion) ,NoOp)
                                          (= closeto (global ZeroAddress)))
                                     (and (= (txn OnCompletion) ,OptIn)
                                          (not (= closeto (global ZeroAddress)))))))
                    ,(asset-move! '(txn Sender) 'receiver '(btoi amount) #f)
                    (unless (= closeto (global ZeroAddress))
                      ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) 'balance) #f))
                    1)]

             [(= (btoi proc) ,asset-freeze)
              (note "freeze asset holding")
              (with ([accs (account)]
                     [args (new-frozen)])
                    (assert (and (not (= (txn ApplicationID) 0))
                                 (= (txn NumAppArgs) 2)
                                 (= (txn NumAccounts) 1)
                                 (= (txn OnCompletion) ,NoOp)
                                 (= (txn Sender) freezer)))
                    (app-updates ((lvars-of account (frozen (btoi new-frozen))))))]

             [else 0])))

    (onclear (app-global-put! cbalance (+ (app-global-get cbalance)
                                            (app-local-get 0 0 balance))))))
