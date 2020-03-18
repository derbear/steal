#lang racket

(require "lang.rkt")

(provide asset-application)

(define asset-enum 0)
(define (asset-enum-next!)
  (begin
    (let ([c asset-enum])
      (set! asset-enum (+ c 1))
      c)))

(define asset-create (asset-enum-next!))
(define asset-delete (asset-enum-next!))
(define asset-reconfigure (asset-enum-next!))
(define asset-open (asset-enum-next!))
(define asset-freeze (asset-enum-next!))
(define asset-transfer (asset-enum-next!))
(define asset-clawback (asset-enum-next!))
(define asset-close (asset-enum-next!))

(define (asset-lget addr key)
  (if (equal? addr '(txn Sender))
      `(app-read-local 0 ,key 0)
      `(app-read-local-acct ,addr ,key 0)))

(define (asset-lset! addr key val)
  (if (equal? addr '(txn Sender))
      `(app-write-local! 0 ,key ,val)
      `(app-write-local-acct! ,addr ,key ,val)))

;; fails and returns 0 if trying to set a zero address to a nonzero one
(define (asset-gset-if-nonzero! addr val)
  `(begin
     (when (and (= (app-read-global ,addr) (global ZeroAddress))
                (not (= ,val (global ZeroAddress))))
       (error "cannot set zeroed address to nonzero value"))
     (app-write-global! ,addr ,val)))

(define (asset-frozen? addr)
  `(or (and (= ,addr creator)
            (= (app-read-global "cfrozen") 1))
       (and (not (= ,addr creator))
            (= ,(asset-lget addr "frozen") 1))))

;; fails and returns 0 if addr is frozen
(define (asset-take-out! addr amt bypass)
  (if bypass
      `(if (= ,addr creator)
           (app-write-global! "cbalance" (- (app-read-global "cbalance") ,amt))
           ,(asset-lset! addr "balance" `(- ,(asset-lget addr "balance") ,amt)))
      `(begin
         (when ,(asset-frozen? addr)
           (error "cannot put in frozen asset"))
         (if (= ,addr creator)
             (app-write-global! "cbalance" (- (app-read-global "cbalance") ,amt))
             ,(asset-lset! addr "balance" `(- ,(asset-lget addr "balance") ,amt))))))

;; fails and returns 0 if addr is frozen
(define (asset-put-in! addr amt bypass)
  (if bypass
      `(if (= ,addr creator)
           (app-write-global! "cbalance" (+ (app-read-global "cbalance") ,amt))
           ,(asset-lset! addr "balance" `(+ ,(asset-lget addr "balance") ,amt)))
      `(begin
         (when ,(asset-frozen? addr)
           (error "cannot put in frozen asset"))
         (if (= ,addr creator)
             (app-write-global! "cbalance" (+ (app-read-global "cbalance") ,amt))
             ,(asset-lset! addr "balance" `(+ ,(asset-lget addr "balance") ,amt))))))

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
             [(= proc ,asset-create)
              (with ([args (manager reserve freezer clawback)])
                    (begin
                      (unless (and (= (txn ApplicationID) 0)
                                   (= (txn ApplicationNumArgs) 5)
                                   (= (txn ApplicationNumAccs) 0)
                                   (= (txn OnCompletion) ,NoOp)
                                   (= creator (txn Sender)))
                        (error "create: bad preconditions"))
                      (app-updates ((gvars (manager manager)
                                           (reserve reserve)
                                           (freezer freezer)
                                           (clawback clawback)
                                           (cbalance supply))))))]

             [(= proc ,asset-delete)
              (and (not (= (txn ApplicationID) 0))
                   (= (txn ApplicationNumArgs) 1)
                   (= (txn ApplicationNumAccs) 0)
                   (= (txn OnCompletion) ,DeleteApplication)
                   (= (txn Sender) (app-read-global "manager"))
                   (= supply (app-read-global "cbalance")))]

             [(= proc ,asset-reconfigure)
              (with ([args (manager reserve freezer clawback)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 5)
                                   (= (txn ApplicationNumAccs) 0)
                                   (= (txn OnCompletion) ,NoOp)
                                   (= (txn Sender) (app-read-global "manager")))
                        (error "reconfigure: bad preconditions"))

                      ;; note: must return 0 if this returns 0
                      ,(asset-gset-if-nonzero! "manager" 'manager)
                      ,(asset-gset-if-nonzero! "reserve" 'reserve)
                      ,(asset-gset-if-nonzero! "freezer" 'freezer)
                      ,(asset-gset-if-nonzero! "clawback" 'clawback)
                      1))]

             [(= proc ,asset-open)
              (begin
                (unless (and (not (= (txn ApplicationID) 0))
                             (= (txn ApplicationNumArgs) 1)
                             (= (txn ApplicationNumAccs) 0)
                             (= (txn OnCompletion) ,OptIn)
                             (not (= (app-opted-in (txn Sender)) 1))
                             (not (= (txn Sender) creator)))
                  (error "open: bad preconditions"))
                (app-updates ((lvars (balance 0)
                                     (frozen defaultfrozen)))))]

             [(= proc ,asset-freeze)
              (with ([accs (account)]
                     [args (frozen)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (= (txn ApplicationNumAccs) 1)
                                   (= (txn OnCompletion) ,NoOp)
                                   (= (txn Sender) freezer))
                        (error "freeze: bad preconditions"))
                      (app-updates ((lvars-of account (frozen frozen))))))]

             [(= proc ,asset-transfer)
              (with ([accs (receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (= (txn ApplicationNumAccs) 1)
                                   (= (txn OnCompletion) ,NoOp))
                        (error "transfer: bad preconditions"))
                      (when (not (= amount 0))
                        ,(asset-move! '(txn Sender) 'receiver 'amount #f))
                      1))]

             [(= proc ,asset-clawback)
              (with ([accs (sender receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (= (txn ApplicationNumAccs) 2)
                                   (= (txn OnCompletion) ,NoOp)
                                   (= (txn Sender) clawback))
                        (error "clawback: bad preconditions"))
                      (when (not (= amount 0))
                        ,(asset-move! 'sender 'receiver 'amount #t))
                      1))]

             ;; note: since creator cannot optin, creator cannot close
             [(= proc ,asset-close)
              (with ([accs (closeto receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (or (and (= (txn ApplicationNumAccs) 1)
                                            (= (txn ApplicationNumArgs 1)))
                                       (and (= (txn ApplicationNumAccs) 2)
                                            (= (txn ApplicationNumArgs 2))))
                                   (= (txn OnCompletion) ,CloseOut))
                        (error "close: bad preconditions"))
                      (when (= (txn ApplicationNumAccs) 2)
                        ,(asset-move! '(txn Sender) 'receiver 'amount #f))
                      ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) "balance" ) #f)
                      1))]
             [else 0])))

    (onclear (app-write-global! "cbalance" (+ (app-read-global "cbalance")
                                              (app-read-local 0 "balance" 0))))))
