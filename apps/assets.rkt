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
  `(if (= ,addr (app-global-gets creator))
       (= (app-global-gets frozen) 1)
       (= ,(asset-lget addr 'frozen) 1)))

;; fails and returns 0 if addr is frozen unless bypass set
(define (asset-modify! addr amt bypass op)
  (let ([ifblock
         `(if (= ,addr (app-global-gets creator))
              (app-global-put! balance (,op (app-global-gets balance) ,amt))
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

;; fails and returns 0 if trying to take out or put into a frozen address unless bypass set or amt is 0
(define (asset-move! snd rcv amt bypass)
  (let ([mem (new-scratch!)])
    `(begin
       (store! ,mem ,amt)
       ,(asset-take-out! snd `(load ,mem) bypass)
       ,(asset-put-in! rcv `(load ,mem) bypass))))

(define creator-help "The asset creator created the asset. The asset creator is always opted in to the asset: its balance and freeze status are stored in global storage at \"creator-balance\" and \"creator-frozen\", respectively. This value is constant.")
(define supply-help "The total supply of the asset when initially created. This value is constant.")
(define default-frozen-help "The default frozen status of any created new account (excepting the creator, whose frozen status is initialized to 0). A value of 1 signifies that the account is frozen and cannot make ordinary transactions. This value is constant.")
(define manager-help "The asset manager can set the manager, reserve, freezer, and clawback addresses if they are nonzero.")
(define reserve-help "The asset reserve for the asset.")
(define freezer-help "The asset freezer can execute the \"freeze\" procedure on accounts.")
(define clawback-help "The asset clawback address can execute the \"clawback\" procedure on accounts.")
(define creator-balance-help "The balance of the \"creator\".")
(define creator-frozen-help "The frozen status of the \"creator\". This value is always initialized to 0.")
(define balance-help "The balance of the account.")
(define frozen-help "The frozen status of the account. This value is initialized to \"default-frozen\" when the account is first created. This value may be modified via the \"freeze\" procedure.")

(define asset-application
  ;; TODO inject these?
  `((params (int decimals)
            (int unitname)
            (string assetname)
            (string url)
            (byte base64 metadatahash))

    (gvars (creator        (addr cr) ,creator-help)        ;; const
           (total-supply   (int tt)  ,supply-help)         ;; const
           (default-frozen (int df)  ,default-frozen-help) ;; const
           (manager        (addr mn) ,manager-help)
           (reserve        (addr rv) ,reserve-help)
           (freezer        (addr fr) ,freezer-help)
           (clawback       (addr cl) ,clawback-help)
           (balance        (int bl)  ,creator-balance-help)
           (frozen         (int fz)  ,creator-frozen-help))

    (lvars (balance (int bl) ,balance-help)
           (frozen  (int fz) ,frozen-help))

    (procs (create NoOp
                   "Create a new asset."
                   (addr manager "The initial asset manager, if nonzero.")
                   (addr reserve "The initial reserve address, if nonzero.")
                   (addr freezer "The initial freezer address, if nonzero.")
                   (addr clawback "The initial clawback address, if nonzero.")
                   (int supply "The total asset supply.")
                   (int default-frozen "The default frozen status for new accounts."))
           (destroy DeleteApplication
                    "Destroy an asset. The asset creator must hold the entire supply. This transaction must be sent by the asset creator.")
           (reconfigure NoOp
                        "Reconfigure an asset's managers. A manager can be set only if it is currently not the zero address. This transaction must be set by the asset's manager."
                        (addr manager "The initial asset manager, if nonzero.")
                        (addr reserve "The initial reserve address, if nonzero.")
                        (addr freezer "The initial freezer address, if nonzero.")
                        (addr clawback "The initial clawback address, if nonzero.")
                        (() int ignored0)
                        (() int ignored1))
           (opt-in OptIn "Opt into an asset. The asset creator is always opted in.")
           (freeze NoOp
                   "Set the frozen value of an account. This transaction must be sent by the asset freezer."
                   (int frozen "The new frozen value of the account.")
                   (account target "The target account whose frozen value to set."))
           (transfer NoOp
                     "Transfer assets to a receiver."
                     (int amount "The amount of the asset to transfer.")
                     (account receiver "The account receiving the assets.")
                     (() account ignored))
           (clawback NoOp
                     "Force the transfer of an asset from some sender to some receiver. This transaction must be sent by the asset clawback account."
                     (int amount "The amount of the asset to transfer.")
                     (() int ignored)
                     (account sender "The account sending the assets.")
                     (account receiver "The account receiving the assets."))
           (close-out CloseOut
                     "Close out all assets to an account, with an optional transfer beforehand."
                     (int amount "An optional amount of the asset to transfer to a receiver before closing out.")
                     (account receiver "An optional account to receive assets before closing out.")
                     (account close-to "The address receiving all remaining assets."))
           (clear ClearState "Clear out all assets from the account."))
    (create-procs create)

    ;; TODO automatically compute numargs/numaccs from context
    (prog
     (if (= (txn NumAppArgs) 6)
         (with ([args (new-manager new-reserve new-freezer new-clawback init-total-supply init-default-frozen)])
               (note "asset configuration")
               (if (= (txn ApplicationID) 0)
                   (begin
                     (app-global-put! creator (txn Sender))
                     (app-global-put! total-supply (btoi init-total-supply))
                     (app-global-put! balance (btoi init-total-supply))
                     (app-global-put! default-frozen (btoi init-default-frozen)))
                   (assert (and (= (txn Sender) (app-global-gets manager)) ;; reconfigure
                                ,(asset-valid-configure? 'manager 'new-manager)
                                ,(asset-valid-configure? 'reserve 'new-reserve)
                                ,(asset-valid-configure? 'freezer 'new-freezer)
                                ,(asset-valid-configure? 'clawback 'new-clawback))))
               (app-global-put! manager new-manager)
               (app-global-put! reserve new-reserve)
               (app-global-put! freezer new-freezer)
               (app-global-put! clawback new-clawback)
               (and (= (txn NumAccounts) 0)
                    (= (txn OnCompletion) ,NoOp)
                    ,(valid-address? 'new-manager)
                    ,(valid-address? 'new-reserve)
                    ,(valid-address? 'new-freezer)
                    ,(valid-address? 'new-clawback)))
         (begin
           (assert (not (= (txn ApplicationID) 0)))
           (cond
             [(= (txn NumAccounts) 0)
              (note "asset deletion or opt-in")

              (when (= (txn OnCompletion) ,OptIn)
                (note "opting in to implicit zero bl")
                (app-local-put! 0 frozen (app-global-gets default-frozen)))

              (and (= (txn NumAppArgs) 0)
                   (or (and (= (txn OnCompletion) ,DeleteApplication)
                            (= (txn Sender) (app-global-gets manager))
                            (= (app-global-gets total-supply) (app-global-gets balance)))
                       (and (= (txn OnCompletion) ,OptIn)
                            (not (= (txn Sender) (app-global-gets creator))))))]

             [(= (txn NumAccounts) 1)
              (note "freeze asset holding")
              (with ([accs (account)]
                     [args (new-frozen)])
                    (if (= account (app-global-gets creator))
                        (app-global-put! frozen (btoi new-frozen))
                        (app-local-put-acct! account frozen (btoi new-frozen)))
                    (and (= (txn NumAppArgs) 1)
                         (= (txn OnCompletion) ,NoOp)
                         (= (txn Sender) (app-global-gets freezer))))]

             [(= (txn NumAppArgs) 2)
              (note "clawback asset")
              (with ([accs (sender receiver)]
                     [args (amount cl-ignored)])
                    ,(asset-move! 'sender 'receiver '(btoi amount) #t)
                    (and (= (txn NumAccounts) 2)
                         (= (txn OnCompletion) ,NoOp)
                         (= (txn Sender) (app-global-gets clawback))))]

             [else
              (note "transfer asset")
              (with ([accs (receiver closeto)]
                     [args (amount)])
                    ,(asset-move! '(txn Sender) 'receiver '(btoi amount) #f)
                    (unless (= closeto (global ZeroAddress))
                      ,(asset-move! '(txn Sender) 'closeto (asset-lget '(txn Sender) 'balance) #f))
                    (and (= (txn NumAppArgs) 1)
                         (= (txn NumAccounts) 2)
                         (if (= (txn OnCompletion) ,NoOp)
                             (= closeto (global ZeroAddress))
                             (and (= (txn OnCompletion) ,CloseOut)
                                  (= ,(asset-lget '(txn Sender) 'balance) 0)
                                  (not (= closeto (global ZeroAddress)))))))]))))

    (onclear (begin
               (app-global-put! balance (+ (app-global-gets balance) (app-local-gets 0 balance)))
               1))))
