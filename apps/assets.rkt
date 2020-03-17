#lang racket

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
      `(app-lget ,key)
      `(app-lget-acct ,addr ,key)))

(define (asset-lset! addr key val)
  (if (equal? addr '(txn Sender))
      `(app-lset! ,key ,val)
      `(app-lset-acct! ,addr ,key ,val)))

;; fails and returns 0 if trying to set a zero address to a nonzero one
(define (asset-gset-if-nonzero! addr val)
  `(begin
     (when (and (= (app-gget ,addr) (global ZeroAddress))
                (not (= ,val (global ZeroAddress))))
       (error "cannot set zeroed address to nonzero value"))
     (app-gset! ,addr ,val)))

(define (asset-frozen? addr)
  `(or (and (= ,addr creator)
            (= (app-gget cfrozen) 1))
       (and (not (= ,addr creator))
            (= ,(asset-lget addr 'frozen) 1))))

;; fails and returns 0 if addr is frozen
(define (asset-take-out! addr amt)
  `(begin
     (when ,(asset-frozen? addr)
       (error "cannot take out frozen asset"))
     (if (= ,addr creator)
         (app-gset! cbalance (- (app-gget cbalance) ,amt))
         ,(asset-lset! addr 'balance `(- ,(asset-lget addr 'balance) amt)))))

;; fails and returns 0 if addr is frozen
(define (asset-put-in! addr amt)
  `(begin
     (when ,(asset-frozen? addr)
       (error "cannot put in frozen asset"))
     (if (= ,addr creator)
         (app-gset! cbalance (+ (app-gget cbalance) ,amt))
         ,(asset-lset! addr 'balance `(+ ,(asset-lget addr 'balance) amt)))))

;; fails and returns 0 if trying to take out or put into a frozen address
(define (asset-move! snd rcv amt)
  `(begin
     ,(asset-take-out! snd amt)
     ,(asset-put-in! rcv amt)))

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
           (case proc

             ;; TODO check type of proc, args...
             [,asset-create
              (with ([args (manager reserve freezer clawback)])
                    (begin
                      (unless (and (= (txn ApplicationID) 0)
                                   (= (txn ApplicationNumArgs) 5)
                                   (= (txn ApplicationNumAccs) 0)
                                   (= (txn OnCompletion) NoOp)
                                   (= creator (txn Sender)))
                        (error "create: bad preconditions"))
                      (app-updates ((gvars (manager manager)
                                           (reserve reserve)
                                           (freezer freezer)
                                           (clawback clawback)
                                           (cbalance supply))))))]

             [,asset-delete
              (and (not (= (txn ApplicationID) 0))
                   (= (txn ApplicationNumArgs) 1)
                   (= (txn ApplicationNumAccs) 0)
                   (= (txn OnCompletion) DeleteApplication)
                   (= (txn Sender) (app-gget manager))
                   (= supply (app-gget cbalance)))]

             [,asset-reconfigure
              (with ([args (new-manager new-reserve new-freezer new-clawback)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 5)
                                   (= (txn ApplicationNumAccs) 0)
                                   (= (txn OnCompletion) NoOp)
                                   (= (txn Sender) (app-gget manager)))
                        (error "create: bad preconditions"))

                      ;; note: must return 0 if this returns 0
                      ,(asset-gset-if-nonzero! 'manager 'new-manager)
                      ,(asset-gset-if-nonzero! 'reserve 'new-reserve)
                      ,(asset-gset-if-nonzero! 'freezer 'new-freezer)
                      ,(asset-gset-if-nonzero! 'clawback 'new-clawback)
                      1))]

             [,asset-open
              (begin
                (unless (and (not (= (txn ApplicationID) 0))
                             (= (txn ApplicationNumArgs) 1)
                             (= (txn ApplicationNumAccs) 0)
                             (= (txn OnCompletion) OptIn)
                             (not (= (ApplicationOptedIn (txn Sender)) 1))
                             (not (= (txn Sender) creator)))
                  (error "open: bad preconditions"))
                (app-updates ((lvars (balance 0)
                                     (frozen defaultfrozen)))))]

             [,asset-freeze
              (with ([accs (account)]
                     [args (frozen)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (= (txn ApplicationNumAccs) 1)
                                   (= (txn OnCompletion) NoOp)
                                   (= (txn Sender) freezer))
                        (error "freeze: bad preconditions"))
                      (app-updates ((lvars-of account (frozen frozen))))))]

             [,asset-transfer
              (with ([accs (receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (= (txn ApplicationNumAccs) 1)
                                   (= (txn OnCompletion) NoOp))
                        (error "transfer: bad preconditions"))
                      (when (not (= amount 0))
                        ,(asset-move! '(txn Sender) 'receiver 'amount))
                      1))]

             [,asset-clawback
              (with ([accs (sender receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (= (txn ApplicationNumAccs) 2)
                                   (= (txn OnCompletion) NoOp)
                                   (= (txn Sender) clawback))
                        (error "clawback: bad preconditions"))
                      (when (not (= amount 0))
                        ,(asset-move! 'sender 'receiver 'amount))
                      1))]

             ;; note: since creator cannot optin, creator cannot close
             [,asset-close
              (with ([accs (closeto receiver)]
                     [args (amount)])
                    (begin
                      (unless (and (not (= (txn ApplicationID) 0))
                                   (= (txn ApplicationNumArgs) 2)
                                   (or (and (= (txn ApplicationNumAccs) 1)
                                            (= (txn ApplicationNumArgs 1)))
                                       (and (= (txn ApplicationNumAccs) 2)
                                            (= (txn ApplicationNumArgs 2))))
                                   (= (txn OnCompletion) CloseOut))
                        (error "close: bad preconditions"))
                      (if (= (txn ApplicationNumAccs) 1)
                          ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) 'balance))
                          (begin
                            ,(asset-move! '(txn Sender) 'receiver 'amount)
                            ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) 'balance))))
                      1))]
             [else 0])))

    (onclear (app-gset! cbalance (+ cbalance (app-lget 'balance))))))
