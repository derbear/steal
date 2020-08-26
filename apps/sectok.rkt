#lang racket

;; goal app create --app-arg str:test --app-arg int:20000 --creator 7UXNOGEKOO4GFJLK3L56PLEWEEN6ALTC5GG2GVJ66OS5SDZWL66NTDGF3A --global-byteslices 1 --global-ints 23 --local-byteslices 0 --local-ints 7 --approval-prog ~/cleango/steal/sectok_approve.teal --clear-prog ~/cleango/steal/sectok_clear.teal --on-completion OptIn

(require "lang.rkt")

(provide security-token-application)

(define (sectok-lget addr key)
  (if (equal? addr '(txn Sender))
      `(app-local-get 0 ,key)
      `(app-local-get-acct ,addr ,key)))

(define (sectok-lset! addr key val)
  (if (equal? addr '(txn Sender))
      `(app-local-put! 0 ,key ,val)
      `(app-local-put-acct! ,addr ,key ,val)))

(define (sectok-modify! addr amt op)
  (sectok-lset! addr 'balance `(,op ,(sectok-lget addr 'balance) ,amt)))

(define (sectok-take-out! addr amt)
  (sectok-modify! addr amt '-))
(define (sectok-put-in! addr amt)
  (sectok-modify! addr amt '+))

(define (sectok-move! snd rcv amt)
  `(begin
     ,(sectok-take-out! snd amt)
     ,(sectok-put-in! rcv amt)))

;; note: snd and rcv are bytes (must be length 8!)
(define (sectok-rule-key snd rcv)
  `(concat (byte 0x0001) ,snd ,rcv))

;; note: snd and rcv are integers
(define (sectok-xfer-allowed? snd rcv)
  ;; possible underflow is deliberate
  `(< (- (app-global-get ,(sectok-rule-key `(itob ,snd) `(itob ,rcv))) 1)
      (global LatestTimestamp)))

;; separate upgrade key
;; *auditability is the issue for upgrades*
;; around 120 rules for rep.

(define token-params-help "The parameters of the security token. This is a JSON object with the string \"symbol\" attribute, the string \"name\" attribute, and the integer \"decimals\" attribute.")
(define reserve-supply-help "The supply of this security token currently sitting in reserve.")
(define total-supply-help "The total supply of this security token when first created. This value is constant.")
(define paused-help "Whether all trading is paused. No trading occurs unless this is zero.")
(define contract-admin-help "Whether this account is a contract admin. If this is one, this account may upgrade the application code, set the contract and transfer admin status of accounts, destroy the asset if the reserve holds the supply, mint and burn from the supply, pause and unpause all transfers, and freeze and unfreeze accounts. This is automatically set to one for the token creator.")
(define transfer-admin-help "Whether this account is a transfer admin. A transfer admin can set transfer rules between groups, freeze and unfreeze accounts, and set the max balance, transfer group, and timed locks for accounts.")
(define frozen-help "Whether this account is frozen. This account issue and accept transfers unless this is zero.")
(define max-balance-help "The maximum token balance allowed for this account.")
(define lock-until-help "The first timestamp at which this account can issue and accept transfers.")
(define lock-until-group-help "The first timestamp at which accounts in one group can issue transfers to accounts in another group.")
(define transfer-group-help "The transfer group of this account, which affects the set of allowed transfers to and from other groups.")
(define balance-help "The number of tokens held by this account.")

(define security-token-application
  `((gvars (token-params   (string px) ,token-params-help) ;; TODO split up the params
           (reserve-supply (int rv)    ,reserve-supply-help)
           (total-supply   (int tt)    ,total-supply-help)
           (paused         (int ps)    ,paused-help)

           (lock-until-group ((map byte int 40)
                              (prefix "0x0001"))
                             ,lock-until-group-help))

    ;; TODO store these in bitmask?
    ;; TODO support (key-of frozen) -> fz
    (lvars (contract-admin (int cX) ,contract-admin-help)
           (transfer-admin (int tX) ,transfer-admin-help)
           (frozen         (int fz) ,frozen-help)
           (max-balance    (int mb) ,max-balance-help)
           (lock-until     (int tl) ,lock-until-help)
           (transfer-group (int tg) ,transfer-group-help)
           (balance        (int bl) ,balance-help))

    (procs (create OptIn
                   "Create a new security token."
                   (string token-params "The parameters for the token.")
                   (int total-supply "The total supply for the token."))
           (destroy DeleteApplication
                    "Destroy a security token. The entire token supply must be in the reserve. This transaction must be sent by a contract admin.")
           (upgrade UpdateApplication
                    "Update the programs implementing the token. This transaction must be sent by a contract admin.")
           (pause NoOp
                  "Control whether all transfers are disallowed."
                  (int paused "No transfers will be allowed unless this is zero."))
           (set-transfer-rule NoOp
                              "Create, update, or delete a transfer rule. This transaction must be sent by a transfer admin."
                              (int send-group "The sending group the rule applies to.")
                              (int receive-group "The receiving group the rule applies to.")
                              (int lock-until "The first timestamp at which accounts in the sending group may make transfers to accounts in the receiving group."))
           (set-contract-admin NoOp
                               "Configure the contract admin status of an account. This transaction must be sent by a contract admin."
                               ((cX) string ignored)
                               (int status "If this is 1, confer contract admin status on the account; otherwise, revoke contract admin status on the account.")
                               (account target "The account to configure."))
           (set-transfer-admin NoOp
                               "Configure the transfer admin status of an account. This transaction must be sent by a contract admin."
                               ((tX) string ignored)
                               (int status "If this is 1, confer transfer admin status on the account; otherwise, revoke transfer admin status on the account.")
                               (account target "The account to configure."))
           (mint NoOp
                 "Mint funds into an account. This circumvents other restrictions on that account. This transaction must be sent by a contract admin."
                 ((mn) string ignored)
                 (int amount "The number of tokens to mint into the account.")
                 (account target "The account to which to mint to."))
           (burn NoOp
                 "Burn funds from an account. This circumvents other restrictions on that account. This transaction must be sent by a contract admin."
                 ((br) string ignored)
                 (int amount "The number of tokens to burn from the account.")
                 (account target "The account from which to mint from."))
           (freeze NoOp
                   "Freeze or unfreeze an account. This transaction must be sent by either a contract or a transfer admin."
                   ((fz) string ignored)
                   (int frozen "No transfers from this account will be allowed unless this is zero.")
                   (account target "The account which to freeze/unfreeze."))
           (set-max-balance NoOp
                            "Set the maximum balance of an account. This transaction must be sent by a transfer admin."
                            ((mb) string ignored)
                            (int max-balance "The new maximum balance of the account.")
                            (account target "The account to configure."))
           (set-lock-until NoOp
                           "Set the first timestamp at which an account can issue transactions. This transaction must be sent by a transfer admin."
                           ((tl) string ignored)
                           (int lock-until "Transfers from the account will be restricted until this timestamp passes.")
                           (account target "The account to configure."))
           (set-transfer-group NoOp
                               "Set the transfer group an account belongs to. This transaction must be sent by a transfer admin."
                               ((tg) string ignored)
                               (int transfer-group "The new transfer group of the account.")
                               (account target "The account to configure."))
           (opt-in OptIn "Opt into a token. The initial transfer group of all accounts is zero.")
           (clear ClearState "Clear all tokens out of the account.")
           (transfer NoOp
                     "Transfer tokens to a receiver."
                     (int amount "The number of tokens to transfer.")
                     (account receiver "The account receiving the tokens.")))
    (create-procs create)

    (prog
     (cond

       [(= (txn ApplicationID) 0)
        (note "create")
        (with ([args (init-token-params init-supply)])
              (app-local-put! 0 contract-admin 1)
              (app-global-put! token-params init-token-params)
              (app-global-put! reserve-supply (btoi init-supply))
              (app-global-put! total-supply (btoi init-supply))

              (and (= (txn NumAppArgs) 2)
                   (= (txn NumAccounts) 0)
                   (= (txn OnCompletion) ,OptIn)))]

       [(= (txn OnCompletion) ,DeleteApplication)
        (note "destroy")
        (and (= (txn NumAppArgs) 0)
             (= (txn NumAccounts) 0)
             (= (app-local-get 0 contract-admin) 1)
             (= (app-global-get reserve-supply) (app-global-get total-supply)))]

       [(= (txn OnCompletion) ,UpdateApplication)
        (note "upgrade")
        (and (= (txn NumAppArgs) 0)
             (= (txn NumAccounts) 0)
             (= (app-local-get 0 contract-admin) 1))]

       [(= (txn OnCompletion) ,OptIn)
        (note "opt-in")
        (and (= (txn NumAppArgs) 0)
             (= (txn NumAccounts) 0))]

       [else
        (assert (and (= (txn OnCompletion) ,NoOp)
                     (< (txn NumAccounts) 2)))

        (if (= (txn NumAccounts) 0)
            (if (= (txn NumAppArgs) 1)
                (with ([args (new-paused)])
                      (note "pause")
                      (app-global-put! paused (btoi new-paused))
                      (= (app-local-get 0 contract-admin) 1))

                (with ([args (send-group receive-group new-lock-until)])
                      (note "transfer-rule")

                      (if (= (btoi new-lock-until) 0)
                          (app-global-del! ,(sectok-rule-key 'send-group 'receive-group))
                          (app-global-put! ,(sectok-rule-key 'send-group 'receive-group) (btoi new-lock-until)))

                      (and (= (len send-group) 8)
                           (= (len receive-group) 8)
                           (= (txn NumAppArgs) 3)
                           (= (app-local-get 0 transfer-admin) 1))))

            (if (= (txn NumAppArgs) 1)
                 (with ([accs (receiver)]
                        [args (amount)])
                       (note "transfer")
                       ,(sectok-move! '(txn Sender) 'receiver '(btoi amount))
                       (and (= (app-global-get paused) 0)
                            (<= (app-local-get-acct receiver balance) (app-local-get-acct receiver max-balance))
                            (= (app-local-get 0 frozen) 0)
                            (= (app-local-get-acct receiver frozen) 0)
                            (< (app-local-get 0 lock-until) (global LatestTimestamp))
                            (< (app-local-get-acct receiver lock-until) (global LatestTimestamp))
                            ,(sectok-xfer-allowed? '(app-local-get 0 transfer-group)
                                                   '(app-local-get-acct receiver transfer-group))))

                 (with ([accs (target)]
                        [args (key value)])
                       (note "config")
                       (assert (= (txn NumAppArgs) 2))
                       (cond [(or (= key (byte "\"cX\""))
                                  (= key (byte "\"tX\"")))
                              (note "contract/transfer admin")
                              (assert (= (app-local-get 0 contract-admin) 1))
                              (app-local-put-acct! target key (btoi value))
                              1]

                             [(= key (byte "\"mn\""))
                              (note "mint")
                              (app-local-put-acct! target balance (+ (app-local-get-acct target balance) (btoi value)))
                              (app-global-put! reserve-supply (- (app-global-get reserve-supply) (btoi value)))
                              (= (app-local-get 0 contract-admin) 1)]

                             [(= key (byte "\"br\""))
                              (note "burn")
                              (app-local-put-acct! target balance (- (app-local-get-acct target balance) (btoi value)))
                              (app-global-put! reserve-supply (+ (app-global-get reserve-supply) (btoi value)))
                              (= (app-local-get 0 contract-admin) 1)]

                             [else
                              (note "set max balance, lock until, transfer group, freeze")
                              (app-local-put-acct! target key (btoi value))
                              (or (and (= key (byte "\"fz\""))
                                       (or (= (app-local-get 0 contract-admin) 1)
                                           (= (app-local-get 0 transfer-admin) 1)))
                                  (and (= (app-local-get 0 transfer-admin) 1)
                                       (or (= key (byte "\"mb\""))
                                           (= key (byte "\"tl\""))
                                           (= key (byte "\"tg\"")))))]))))]))

    (onclear (app-global-put! reserve-supply
                              (+ (app-global-get reserve-supply)
                                 (app-local-get 0 balance))))))
