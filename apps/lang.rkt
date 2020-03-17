#lang racket

(provide app-schema)
(provide app-program)
(provide app-clear-program)

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
