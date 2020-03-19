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

(define (asset-valid-configure? key val)
  `(or (= ,val (global ZeroAddress))
       (not (= (app-read-global ,key) (global ZeroAddress)))))

(define (asset-lget addr key)
  (if (equal? addr '(txn Sender))
      `(app-read-local 0 ,key 0)
      `(app-read-local-acct ,addr ,key 0)))

(define (asset-lset! addr key val)
  (if (equal? addr '(txn Sender))
      `(app-write-local! 0 ,key ,val)
      `(app-write-local-acct! ,addr ,key ,val)))

(define (asset-frozen? addr)
  `(or (and (= ,addr creator)
            (= (app-read-global cfrozen) 1))
       (and (not (= ,addr creator))
            (= ,(asset-lget addr 'frozen) 1))))

;; fails and returns 0 if addr is frozen unless bypass set
(define (asset-modify! addr amt bypass op)
  (let ([ifblock
         `(if (= ,addr creator)
           (app-write-global! cbalance (,op (app-read-global cbalance) ,amt))
           ,(asset-lset! addr 'balance `(,op ,(asset-lget addr 'balance) ,amt)))])
    (if bypass
        ifblock
        `(begin
           (unless (= ,amt 0)
             (when ,(asset-frozen? addr)
               (error "cannot modify frozen asset"))
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
  `((params (int supply)
            (int decimals)
            (int defaultfrozen)
            (int unitname)
            (int assetname)
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
    ;; fn: {implicit env} -> write set
    (prog
     (with ([args (proc)])
           (cond

             ;; TODO check type of proc, args...
             [(= proc ,asset-configure)
              (with ([args (new-manager new-reserve new-freezer new-clawback)])
                    (begin
                      (unless (and (= (txn NumAppArgs) 5)
                                   (= (txn NumAccounts) 0)
                                   (= (txn OnCompletion) ,NoOp)
                                   (or (and (= (txn ApplicationID 0)) ;; create
                                            (= creator (txn Sender)))
                                       (and (not (= (txn ApplicationID) 0)) ;; reconfigure
                                            (= (txn Sender) (app-read-global manager))
                                            ,(asset-valid-configure? 'manager 'new-manager)
                                            ,(asset-valid-configure? 'reserve 'new-reserve)
                                            ,(asset-valid-configure? 'freezer 'new-freezer)
                                            ,(asset-valid-configure? 'clawback 'new-clawback))))
                        (error "configure: bad preconditions"))
                      (when (= (txn ApplicationID 0))
                        (app-write-global! cbalance (int TMPL_SUPPLY)))
                      (app-updates ((gvars (manager new-manager)
                                           (reserve new-reserve)
                                           (freezer new-freezer)
                                           (clawback new-clawback))))))]

             [(= proc ,asset-delete)
              (and (not (= (txn ApplicationID) 0))
                   (= (txn NumAppArgs) 1)
                   (= (txn NumAccounts) 0)
                   (= (txn OnCompletion) ,DeleteApplication)
                   (= (txn Sender) (app-read-global manager))
                   (= supply (app-read-global cbalance)))]

             [(= proc ,asset-open)
              (begin
                (unless (and (not (= (txn ApplicationID) 0))
                             (= (txn NumAppArgs) 1)
                             (= (txn NumAccounts) 0)
                             (= (txn OnCompletion) ,OptIn)
                             (not (= (app-opted-in (txn Sender)) 1))
                             (not (= (txn Sender) creator)))
                  (error "open: bad preconditions"))
                (app-updates ((lvars (balance 0)
                                     (frozen defaultfrozen)))))]

             [(= proc ,asset-clawback)
              (with ([accs (sender receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn NumAppArgs) 2)
                                   (= (txn NumAccounts) 2)
                                   (= (txn OnCompletion) ,NoOp)
                                   (= (txn Sender) clawback))
                        (error "clawback: bad preconditions"))
                      ,(asset-move! 'sender 'receiver 'amount #t)
                      1))]

             ;; note: since creator cannot optin, creator cannot close
             [(= proc ,asset-transfer)
              (with ([accs (receiver closeto)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn NumAppArgs) 2)
                                   (= (txn NumAccounts) 2)
                                   (or (and (= (txn OnCompletion) ,NoOp)
                                            (= closeto (global ZeroAddress)))
                                       (and (= (txn OnCompletion) ,OptIn)
                                            (not (= closeto (global ZeroAddress))))))
                        (error "transfer: bad preconditions"))
                      ,(asset-move! '(txn Sender) 'receiver 'amount #f))
                      (unless (= closeto (global ZeroAddress))
                        ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) 'balance) #f))
                      1)]

             [(= proc ,asset-freeze)
              (with ([accs (account)]
                     [args (frozen)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn NumAppArgs) 2)
                                   (= (txn NumAccounts) 1)
                                   (= (txn OnCompletion) ,NoOp)
                                   (= (txn Sender) freezer))
                        (error "freeze: bad preconditions"))
                      (app-updates ((lvars-of account (frozen frozen))))))]

             [else 0])))

    (onclear (app-write-global! cbalance (+ (app-read-global cbalance)
                                              (app-read-local 0 balance 0))))))
