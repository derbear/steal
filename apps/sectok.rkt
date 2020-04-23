#lang racket

(require "lang.rkt")

(provide security-token-application)

(define sectok-enum 0)
(define (sectok-enum-next!)
  (begin
    (let ([c sectok-enum])
      (set! sectok-enum (+ c 1))
      c)))

(define sectok-ct-admin (sectok-enum-next!))
(define sectok-xf-admin (sectok-enum-next!))
(define sectok-mint (sectok-enum-next!))
(define sectok-burn (sectok-enum-next!))
(define sectok-freeze (sectok-enum-next!))
(define sectok-max-bal (sectok-enum-next!))
(define sectok-lock-until (sectok-enum-next!))
(define sectok-xfer-group (sectok-enum-next!))

(define (sectok-lget addr key)
  (if (equal? addr '(txn Sender))
      `(app-local-gets 0 ,key)
      `(app-local-gets-acct ,addr ,key)))

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

;; (4160 << 32) + (sender-group << 16) + (receiver-group)
(define (sectok-rule-key snd rcv) `(+ (* 4160 8589934592) (* ,snd 131072) ,rcv))

(define (sectok-xfer-allowed? snd rcv)
  `(< (app-global-gets (itob ,(sectok-rule-key snd rcv)) (txn FirstValid))))

;; separate upgrade key
;; *auditability is the issue for upgrades*
;; around 120 rules for rep.

(define security-token-application
  `((gvars (string params)
           ;; params is a JSON object with keys
           ;; "symbol" (str), "name" (str), and "decimals" (int)

           (addr reserve-admin)
           (int reserve-supply)

           (int total-supply)

           (int paused)

           (map 120 int group-transfer-lock-until))

    (lvars (int contract-admin) ;; TODO store these in bitmask?
           (int transfer-admin)
           (int frozen)
           (int max-balance)
           (int lock-until)
           (int transfer-group)
           (int balance))
    ;; note that creator is the first admin
    (prog
     (cond
       
       [(= (txn ApplicationID) 0)
        (note "create")
        (with ([args (init-params init-reserve-admin init-supply)])
              (app-local-set! 0 contract-admin 1)
              (app-global-set! params init-params)
              (app-global-set! reserve-admin init-reserve-admin)
              (app-global-set! reserve-supply init-supply)
              (app-global-set! total-supply init-supply)

              (and (= (txn NumAppArgs) 3)
                   (= (txn NumAccounts) 0)
                   (= (txn OnCompletion) ,OptIn)))]

       [(= (txn OnCompletion) ,DeleteApplication)
        (note "delete")
        (and (= (txn NumAppArgs) 0)
             (= (txn NumAccounts) 0)
             (= (app-local-gets 0 contract-admin) 1)
             (= (app-global-gets reserve-supply) (app-global-gets total-supply)))]

       [else
        (assert (and (= (txn OnCompletion) ,NoOp)
                     (< (txn NumAccounts) 2)))

        (if (= (txn NumAccounts) 0)
            (if (= (txn NumAppArgs) 1)
                (with ([args (new-paused)])
                      (note "pause")
                      (app-global-set! paused (btoi new-paused))
                      (= (app-local-gets 0 contract-admin) 1))

                (with ([args (pair lock-until)])
                      (note "transfer-rule")
                      (app-global-set! pair lock-until)
                      (and (= (txn NumAppArgs) 2)
                           (= (app-local-gets 0 transfer-admin) 1))))

            (if (= (txn NumAppArgs) 1)
                 (with ([accs (receiver)]
                        [args (amount)])
                       (note "transfer")
                       ,(sectok-move! '(txn Sender) 'receiver '(btoi amount))
                       (and (= (app-global-gets paused) 0)
                            (<= (app-local-gets receiver balance) (app-local-gets receiver max-balance))
                            (= (app-local-gets 0 frozen) 0)
                            (= (app-local-gets receiver frozen) 0)
                            (< (app-local-gets 0 lock-until) (txn FirstValid))
                            (< (app-local-gets receiver lock-until) (txn FirstValid))
                            ,(sectok-xfer-allowed? '(app-local-gets 0 transfer-group)
                                                   '(app-local-gets receiver transfer-group))))

                 (with ([accs (target)]
                        [args (key value)])
                       (note "config")
                       (assert (= (txn NumAppArgs) 2))
                       (cond [(or (= key contract-admin)
                                  (= key transfer-admin))
                              (note "contract/transfer admin")
                              (app-local-set! target key (btoi value))
                              (= (app-local-gets 0 contract-admin) 1)]

                             [(= key mint)
                              (note "mint")
                              (app-local-set! target balance (+ (app-local-gets target balance) (btoi value)))
                              (app-global-set! reserve-supply (- (app-global-gets reserve-supply) (btoi value)))
                              (= (app-local-gets 0 contract-admin) 1)]

                             [(= key burn)
                              (note "burn")
                              (app-local-set! target balance (- (app-local-gets target balance) (btoi value)))
                              (app-global-set! reserve-supply (+ (app-global-gets reserve-supply) (btoi value)))
                              (= (app-local-gets 0 contract-admin) 1)]

                             [else
                              (note "max balance, lock until, transfer group, frozen")
                              (app-local-set! target key (btoi value))
                              (or (and (= key frozen)
                                       (or (= (app-global-gets 0 contract-admin) 1)
                                           (= (app-global-gets 0 transfer-admin) 1)))
                                  (and (= (app-global-gets 0 transfer-admin) 1)
                                       (or (= key max-balance)
                                           (= key lock-until)
                                           (= key transfer-group))))]))))]))

    (onclear (app-global-set! reserve-supply
                              (+ (app-global-gets reserve-supply)
                                 (app-local-gets 0 balance))))))


             
