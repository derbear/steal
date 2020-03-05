#lang racket

(require racket/pretty)

(provide asset-application)

(define (app-get sym app) (rest (assoc sym app)))
(define (app-params app) (app-get 'params app))
(define (app-gvars app) (app-get 'gvars app))
(define (app-lvars app) (app-get 'lvars app))
(define (app-prog app) (app-get 'prog app))
;; (define (app-fns app) (app-get 'fns app))

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
  (cond [(null? ast) '()]
        [(number? ast) ast]
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
           (addr clawback))

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
                    (if (and (= (txn ApplicationID) 0)
                             (= (txn ApplicationNumArgs) 5)
                             (= (txn ApplicationNumAccs) 0)
                             (= (txn OnCompletion) OptIn)
                             (= creator (txn Sender)))
                        (app-updates ((gvars (manager manager)
                                             (reserve reserve)
                                             (freezer freezer)
                                             (clawback clawback))
                                      (lvars (balance supply))))
                        0))]

             [,asset-delete
              ;; note: manager nonzero check left out
              (and (not (= (txn ApplicationID) 0))
                   (= (txn ApplicationNumArgs) 1)
                   (= (txn ApplicationNumAccs) 0)
                   (= (txn OnCompletion) DeleteApplication)
                   (= (txn Sender) (app-gget manager)))]
                   ;; (= supply (app-lget-acct creator balance)))] ;; TODO syntactically invalid to use creator here
                   ;;                                              ;; wait for opcode

             [,asset-reconfigure
              (with ([args (new-manager reserve freezer clawback)])
                    (if (and (not (= (txn ApplicationID) 0))
                             (= (txn ApplicationNumArgs) 5)
                             (= (txn ApplicationNumAccs) 0)
                             (= (txn OnCompletion) NoOp)
                             (= (txn Sender) (app-gget manager)))
                        ;; TODO cannot set 0 field to be nonzero
                        (app-updates ((gvars (manager new-manager)
                                             (reserve reserve)
                                             (freezer freezer)
                                             (clawback clawback))))
                        0))]

             ;; TODO make sure clawback cannot allocate
             [,asset-open
              (if (and (not (= (txn ApplicationID) 0))
                       (= (txn ApplicationNumArgs) 1)
                       (= (txn ApplicationNumAccs) 0)
                       (= (txn OnCompletion) OptIn))
                  ;; TODO noop if already opted in...
                  (app-updates ((lvars (balance 0)
                                       (frozen defaultfrozen))))
                  0)]

             [,asset-freeze
              (with ([accs (account)]
                     [args (frozen)])
                    (if (and (not (= (txn ApplicationID) 0))
                             (= (txn ApplicationNumArgs) 2)
                             (= (txn ApplicationNumAccs) 1)
                             (= (txn OnCompletion) NoOp)
                             (= (txn Sender) freezer))
                        (app-updates ((lvars-of account (frozen frozen))))
                        0))]

             [,asset-transfer
              (with ([accs (receiver)]
                     [args (amount)])
                    (if (and (not (= (txn ApplicationID) 0))
                             (= (txn ApplicationNumArgs) 2)
                             (= (txn ApplicationNumAccs) 1)
                             (= (txn OnCompletion) NoOp))
                        (cond [(= amount 0) 1]
                              [(= (app-get frozen) 1) 0]
                              [else
                               (begin
                                 (app-lset! balance (- (app-lget balance) amount))
                                 (app-lset-acct! receiver balance (+ (app-lget-acct receiver balance) amount))
                                 1)])
                        0))]

             [,asset-clawback
              (with ([accs (sender receiver)]
                     [args (amount)])
                    (if (and (not (= (txn ApplicationID) 0))
                             (= (txn ApplicationNumArgs) 2)
                             (= (txn ApplicationNumAccs) 2)
                             (= (txn OnCompletion) NoOp)
                             (= (txn Sender) clawback))
                        (cond [(= amount 0) 1]
                              [else
                               (begin
                                 (app-lset-acct! sender balance (- (app-lget-acct sender balance) amount))
                                 (app-lset-acct! receiver balance (+ (app-lget-acct receiver balance) amount))
                                 1)])
                        0))]

             ;; TODO can creator closeout????
             ;; [,asset-close
             ;;  (with ([accs (closeto receiver)]
             ;;         [args (amount)])
             ;;        (if (and (not (= (txn ApplicationID) 0))
             ;;                 (= (txn ApplicationNumArgs) 2)
             ;;                 (or (= (txn ApplicationNumAccs) 1) (= (txn ApplicationNumAccs) 2))
             ;;                 (= (txn OnCompletion) NoOp)
             ;;                 (= (txn Sender) clawback))
             ;;            (cond [(= (txn ApplicationNumAccs) 2)
             ;;                   (begin
             ;;                     (app-lset-acct! receiver balance (+ (app-lget-acct creator balance) (app-lget balance)))
             ;;                     (app-lset! balance 0)
             ;;                     1)]
             ;;                  [(= (txn Ap

             ;;            0))]
                                 
             [else 0])))))

     ;; (transfer app-call)
     ;; (clawback app-call)
     ;; (close app-closeout) ;; TODO prohibit creator from closing out
     ;; (clear app-clear))))

;; (app-schema asset-application)
(pretty-print (app-program asset-application))

;;;;;

;; (define (asset-settable old new)
;;   `(or (not (= ,old (global ZeroAddress)))
;;        (= ,new (global ZeroAddress))))

;; (app-bind! create
;;            'app-create
;;            '(args (manager reserve freezer clawback))
;;            `((manager manager)
;;              (reserve reserve)
;;              (freezer freezer)
;;              (clawback clawback))) ;; TODO write supply to lvars

;; (app-bind! reconfig
;;            'app-call
;;            '(args (manager reserve freezer clawback))
;;            `(if (and (= (txn Sender) (app-get manager))
;;                      ,(asset-settable '(app-gget reserve) 'reserve)
;;                      ,(asset-settable '(app-gget freezer) 'freezer)
;;                      ,(asset-settable '(app-gget clawback)) 'clawback)
;;                 ((gvars (manager manager)
;;                         (reserve reserve)
;;                         (freezer freezer)
;;                         (clawback clawback)))
;;                 fail!))

;; (app-bind! destroy 'app-delete ())

;; (app-bind! open 'app-open ())

;; ;; note: make sure closeout check passes/fails for creator closeout to self

;; TODO prohibit creator from holding asset

;; (app-bind! transfer
;;            'app-call
;;            '(addrs receiver)
;;            `(if (not ,(asset-frozen))
;;                 ,(asset-move '(txn Sender) 'receiver)
;;                 fail!))

;;;;;

;; (define app-fn-kinds
;;   '(app-create app-delete app-update app-optin app-closeout app-clear app-call))

;; (define (app-fn-kind appfn)
;;   (second appfn))

;; (define (app-get-sym-fns sym app)
;;   (filter (lambda (fn) (eq? (app-fn-kind fn) sym)) (app-fns app)))

;; (define (app-get-sym-fn-1 sym app)
;;   (let ([fns (app-get-sym-fns sym app)])
;;     (if (not (= (length fns) 1))
;;         (raise (format "app-get-sym-fn-1: sym ~v appeared ~v times" sym (length fns)))
;;         (rest (rest (first fns))))))

;; (define (app-create-fn app) (app-get-sym-fn-1 'app-create app))
;; (define (app-delete-fn app) (app-get-sym-fn-1 'app-delete app))
;; (define (app-update-fn app) (app-get-sym-fn-1 'app-update app))
;; (define (app-optin-fn app) (app-get-sym-fn-1 'app-optin app))
;; (define (app-closeout-fn app) (app-get-sym-fn-1 'app-closeout app))
;; (define (app-clear-fn app) (app-get-sym-fn-1 'app-clear app))
;; (define (app-call-fns app) (app-get-sym-fns 'app-call app))

;; (define (app-rewrite-fn-helper-first approval? params args ast)
;;   (app-rewrite-fn-helper approval? params args ast))

;; (define (app-rewrite-fn-helper-rest approval? params args ast)
;;   (app-rewrite-fn-helper approval? params args ast))

;; (define (app-compile-var-updates global? params args acct)
;;   (lambda (ast) ;; (key value)
;;     (let ([key (first ast)]
;;           [value (second ast)])
;;       (append (if global?
;;                   '(app-gset!)
;;                   (if (not (null? acct))
;;                       `(app-lset-acct! ,(app-rewrite-fn-helper #f params args acct))
;;                       '(app-lset!)))
;;               (list key
;;                     (app-rewrite-fn-helper #f params args value))))))

;; (define (maybe fn)
;;   (lambda (l)
;;     (if (and l (not (null? l)))
;;         (fn l)
;;         '())))
        
;; (define maybe-rest (maybe rest))
;; (define maybe-first (maybe first))
;; (define maybe-second (maybe second))

;; (define (app-compile-updates params args ast)
;;   (append '(begin)
;;           (map (app-compile-var-updates #t params args '()) (maybe-rest (assoc 'gvars ast)))
;;           (map (app-compile-var-updates #f params args '()) (maybe-rest (assoc 'lvars ast)))
;;           (map (app-compile-var-updates #f params args (maybe-second (assoc 'lvars-of ast))) (maybe-rest (maybe-rest (assoc 'lvars-of ast))))
;;           '(1)))

;; (define (app-rewrite-updates approval? params args ast)
;;   (if approval?
;;       1
;;       (app-compile-updates params args ast)))

;; (define (app-params-assoc params sym)
;;   (cond [(null? params) #f]
;;         [(eq? (last (first params)) sym) (first params)]
;;         [else (app-params-assoc (rest params) sym)]))

;; (define (app-rewrite-arg n)
;;   `(txn ApplicationArg ,n))

;; (define (app-rewrite-tmpl spec)
;;   (cond [(= (length spec) 0) (raise "app-rewrite-tmpl")] ;; TODO add err
;;         [(= (length spec) 2) (cons (first spec) (app-rewrite-tmpl (rest spec)))]
;;         [else (list (string->symbol (string-append "TMPL_" (string-upcase (symbol->string (first spec))))))]))

;; (define (app-arg-index args sym)
;;   (app-rewrite-arg (- (length args) (length (memq sym args)))))

;; (define (app-rewrite-subst params args sym)
;;   (cond [(memq sym args) (app-arg-index args sym)]
;;         [(app-params-assoc params sym) (app-rewrite-tmpl (app-params-assoc params sym))]
;;         [else sym]))

;; (define (app-rewrite-fn-helper approval? params args ast)
;;   (cond [(number? ast) ast]
;;         [(symbol? ast) (app-rewrite-subst params args ast)]
;;         [(null? ast) ast]
;;         [(eq? (first ast) 'app-updates) (app-rewrite-updates approval? params args (second ast))]
;;         [else (cons (app-rewrite-fn-helper-first approval? params args (first ast))
;;                     (app-rewrite-fn-helper-rest approval? params args (rest ast)))]))

;; (define (app-rewrite-fn approval? params fn)
;;   (let ([args (first fn)]
;;         [body (second fn)])
;;     (app-rewrite-fn-helper approval? params args body)))

;; (define (app-programs-helper approval? app)
;;   (define (sel l) (rest (rest (first (rest l)))))
;;   `(cond [(= (txn OnCompletion) CreateApplication)
;;           ,(app-rewrite-fn approval? (app-params app) (app-create-fn app))]
;;          [(= (txn OnCompletion) DeleteApplication)
;;           ,(app-rewrite-fn approval? (app-params app) (app-delete-fn app))]
;;          [(= (txn OnCompletion) OptIn)
;;           ,(app-rewrite-fn approval? (app-params app) (app-optin-fn app))]
;;          [else
;;           ,(app-rewrite-fn approval? (app-params app) (sel (app-call-fns app)))])) ;; TODO handle app calls correctly

;; (define (app-programs app)
;;   (list (list 'approval (app-programs-helper #t app))
;;         (list 'state-update (app-programs-helper #f app))))

