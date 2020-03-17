#lang racket

(require racket/pretty)

(provide asset-application)

(define (app-get sym app) (rest (assoc sym app)))
(define (app-params app) (app-get 'params app))
(define (app-gvars app) (app-get 'gvars app))
(define (app-lvars app) (app-get 'lvars app))
(define (app-prog app) (app-get 'prog app))
(define (app-onclear app) (app-get 'onclear app))

(define (app-int? var) (eq? (first var) 'int))
(define (app-blob? var) (not (app-int? var)))

(define (app-blobs vars) (length (filter app-blob? vars)))
(define (app-ints vars) (length (filter app-int? vars)))

(define (app-schema app)
  `((global-state-blobs ,(app-blobs (app-gvars app)))
    (global-state-ints ,(app-ints (app-gvars app)))
    (local-state-blobs ,(app-blobs (app-lvars app)))
    (local-state-ints ,(app-ints (app-lvars app)))))

(define (app-program app) (app-program-helper (first (app-prog app)) (app-params app) '() '()))
(define (app-clear-program app) (app-program-helper (first (app-onclear app)) (app-params app) '() '()))

(define (with-sym sym ast)
  (if (assoc sym (second ast))
      (second (assoc sym (second ast)))
      '()))

(define (with-args ast) (with-sym 'args ast))
(define (with-accs ast) (with-sym 'accs ast))

(define (app-params-assoc params sym)
  (cond [(null? params) #f]
        [(eq? (last (first params)) sym) (first params)]
        [else (app-params-assoc (rest params) sym)]))

(define (app-rewrite-param spec)
  (cond [(= (length spec) 0) (raise "app-rewrite-param")] ;; TODO add err
        [(= (length spec) 2) (cons (first spec) (app-rewrite-param (rest spec)))]
        [else (list (string->symbol (string-append "TMPL_" (string-upcase (symbol->string (first spec))))))]))

(define (app-rewrite-arg n)
  `(txn ApplicationArg ,n))

(define (app-rewrite-acc n)
  `(txn ApplicationAcc ,n))

(define (app-acc-rewrite ast)
  (third ast))

(define (app-arg-index args sym)
  (app-rewrite-arg (- (length args) (length (memq sym args)))))

(define (app-acc-index accs sym)
  (app-rewrite-acc (- (length accs) (length (memq sym accs)))))

(define (app-rewrite-subst params args accs sym)
  (cond [(memq sym args) (app-arg-index args sym)]
        [(memq sym accs) (app-acc-index accs sym)]
        [(app-params-assoc params sym) (app-rewrite-param (app-params-assoc params sym))]
        [else sym]))

(define (app-program-helper ast params local-args local-accs)
  (cond [(null? ast) ast]
        [(number? ast) ast]
        [(string? ast) ast]
        [(symbol? ast) (app-rewrite-subst params local-args local-accs ast)]
        [(eq? (first ast) 'with)
         (app-program-helper (third ast)
                             params
                             (append local-args (with-args ast))
                             (append local-accs (with-accs ast)))]
        [(eq? (first ast) 'app-updates)
         (app-rewrite-updates params
                              local-args
                              local-accs
                              (second ast))]
        [(or (eq? (first ast) 'app-lget-acct)
             (eq? (first ast) 'app-lset-acct!))
         (cons (first ast)
               (cons (app-acc-rewrite (app-program-helper (second ast) params local-args local-accs))
                     (app-program-helper (rest (rest ast)) params local-args local-accs)))]

        [else (cons (app-program-helper (first ast) params local-args local-accs)
                    (app-program-helper (rest ast) params local-args local-accs))]))

(define (app-compile-var-updates global? params args accs acct)
  (lambda (ast) ;; (key value)
    (let ([key (first ast)]
          [value (second ast)])
      (append (if global?
                  '(app-gset!)
                  (if (not (null? acct))
                      `(app-lset-acct! ,(app-acc-rewrite (app-program-helper acct params args accs)))
                      '(app-lset!)))
              (list key
                    (app-program-helper value params args accs))))))

(define (maybe fn)
  (lambda (l)
    (if (and l (not (null? l)))
        (fn l)
        '())))

(define maybe-rest (maybe rest))
(define maybe-first (maybe first))
(define maybe-second (maybe second))

(define (app-rewrite-updates params args accs ast)
  (append '(begin)
          (map (app-compile-var-updates #t params args accs '()) (maybe-rest (assoc 'gvars ast)))
          (map (app-compile-var-updates #f params args accs '()) (maybe-rest (assoc 'lvars ast)))
          (map (app-compile-var-updates #f params args accs (maybe-second (assoc 'lvars-of ast))) (maybe-rest (maybe-rest (assoc 'lvars-of ast))))
          '(1)))

;; TODO enforce that these numbers are distinct
(define asset-create 0)
(define asset-delete 1)
(define asset-reconfigure 2)
(define asset-open 3)
(define asset-freeze 4)
(define asset-transfer 5)
(define asset-clawback 6)
(define asset-close 7)

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
                      (app-updates ((lvars-of account (frozen frozen))))
                      1))]

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

(define (stealc-flatten-begin-helper ast)
  (cond [(null? ast) ast]
        [(number? ast) ast]
        [(string? ast) ast]
        [(symbol? ast) ast]
        [else
         (if (and (list? (first ast))
                  (> (length (first ast)) 0)
                  (eq? (first (first ast)) 'begin))
             (append (stealc-flatten-begin (rest (first ast))) (stealc-flatten-begin-helper (rest ast)))
             (cons (stealc-flatten-begin (first ast)) (stealc-flatten-begin-helper (rest ast))))]))



(define (stealc-flatten-begin ast)
  (cond [(null? ast) ast]
        [(number? ast) ast]
        [(string? ast) ast]
        [(symbol? ast) ast]
        [else
         (if (eq? (first ast) 'begin)
             (let ([flattened (stealc-flatten-begin-helper (rest ast))])
               (if (equal? flattened (rest ast))
                   (cons 'begin (rest ast))
                 (stealc-flatten-begin (cons 'begin flattened))))
             (cons (stealc-flatten-begin (first ast)) (stealc-flatten-begin (rest ast))))]))

(pretty-print (app-schema asset-application))
(pretty-print (stealc-flatten-begin (app-program asset-application)))
(pretty-print (stealc-flatten-begin (app-clear-program asset-application)))
