#lang racket

(require net/base64)

(provide app-schema)
(provide app-program)
(provide app-clear-program)

(provide NoOp)
(provide OptIn)
(provide CloseOut)
(provide ClearState)
(provide UpdateApplication)
(provide DeleteApplication)

(define app-on-completion-enum 0)
(define (app-on-completion-enum-next!)
  (begin
    (let ([c app-on-completion-enum])
      (set! app-on-completion-enum (+ c 1))
      c)))

(define NoOp (app-on-completion-enum-next!))
(define OptIn (app-on-completion-enum-next!))
(define CloseOut (app-on-completion-enum-next!))
(define ClearState (app-on-completion-enum-next!))
(define UpdateApplication (app-on-completion-enum-next!))
(define DeleteApplication (app-on-completion-enum-next!))

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

(define (app-program app) (app-program-helper (first (app-prog app)) (app-params app) (app-gvars app) (app-lvars app) '() '()))
(define (app-clear-program app) (app-program-helper (first (app-onclear app)) (app-params app) (app-gvars app) (app-lvars app) '() '()))

(define (with-sym sym ast)
  (if (assoc sym (second ast))
      (second (assoc sym (second ast)))
      '()))

(define (with-args ast) (with-sym 'args ast))
(define (with-accs ast) (with-sym 'accs ast))

(define (app-spec-assoc params sym)
  (cond [(null? params) #f]
        [(eq? (last (first params)) sym) (first params)]
        [else (app-spec-assoc (rest params) sym)]))

(define (app-rewrite-param spec)
  (when (not (or (= (length spec) 2) (= (length spec) 3)))
    (raise "app-rewrite-param")) ;; TODO add err
  (append (take spec (- (length spec) 1))
          (list (string->symbol (string-append "TMPL_" (string-upcase (symbol->string (last spec))))))))

(define (symbol->base64 sym)
  (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 (symbol->string sym)) "")))

(define (app-rewrite-gvar spec)
  (when (not (or (= (length spec) 2) (= (length spec) 3)))
    (raise "app-rewrite-gvar")) ;; TODO add err
  `(byte base64 ,(symbol->base64 (last spec))))

(define (app-rewrite-lvar spec)
  (when (not (or (= (length spec) 2) (= (length spec) 3)))
    (raise "app-rewrite-lvar")) ;; TODO add err
  `(byte base64 ,(symbol->base64 (last spec))))

(define (app-rewrite-arg n)
  `(txna ApplicationArgs ,n))

(define (app-rewrite-acc n)
  `(txna Accounts ,(+ n 1)))

(define (app-acc-rewrite ast)
  (third ast))

(define (app-arg-index args sym)
  (app-rewrite-arg (- (length args) (length (memq sym args)))))

(define (app-acc-index accs sym)
  (app-rewrite-acc (- (length accs) (length (memq sym accs)))))

(define (app-rewrite-subst params gvars lvars args accs sym)
  (cond [(memq sym args) (app-arg-index args sym)]
        [(memq sym accs) (app-acc-index accs sym)]
        [(app-spec-assoc params sym) (app-rewrite-param (app-spec-assoc params sym))]
        [(app-spec-assoc gvars sym) (app-rewrite-gvar (app-spec-assoc gvars sym))]
        [(app-spec-assoc lvars sym) (app-rewrite-lvar (app-spec-assoc lvars sym))]
        [else sym]))

(define (app-program-helper ast params global-vars local-vars local-args local-accs)
  (cond [(null? ast) ast]
        [(number? ast) ast]
        [(string? ast) ast]
        [(symbol? ast) (app-rewrite-subst params global-vars local-vars local-args local-accs ast)]
        [(eq? (first ast) 'with)
         (append '(begin)
            (app-program-helper (rest (rest ast))
                                params
                                global-vars
                                local-vars
                                (append local-args (with-args ast))
                                (append local-accs (with-accs ast))))]
        [(eq? (first ast) 'app-updates)
         (app-rewrite-updates params
                              global-vars
                              local-vars
                              local-args
                              local-accs
                              (second ast))]
        [(or (eq? (first ast) 'app-local-get-acct)
             (eq? (first ast) 'app-local-gets-acct)
             (eq? (first ast) 'app-local-put-acct!))
         (cons (string->symbol (string-replace (symbol->string (first ast))
                                               "-acct"
                                               ""))
                                
               (cons (app-acc-rewrite (app-program-helper (second ast) params global-vars local-vars local-args local-accs))
                     (app-program-helper (rest (rest ast)) params global-vars local-vars local-args local-accs)))]

        [else (cons (app-program-helper (first ast) params global-vars local-vars local-args local-accs)
                    (app-program-helper (rest ast) params global-vars local-vars local-args local-accs))]))

(define (app-compile-var-updates global? params gvars lvars args accs acct)
  (lambda (ast) ;; (key value)
    (let ([key (first ast)]
          [value (second ast)])
      (append (if global?
                  '(app-global-put!)
                  (if (not (null? acct))
                      `(app-local-put! ,(app-acc-rewrite (app-program-helper acct params gvars lvars args accs)))
                      '(app-local-put! 0)))
              (list (app-program-helper key params gvars lvars args accs)
                    (app-program-helper value params gvars lvars args accs))))))

(define (maybe fn)
  (lambda (l)
    (if (and l (not (null? l)))
        (fn l)
        '())))

(define maybe-rest (maybe rest))
(define maybe-first (maybe first))
(define maybe-second (maybe second))

(define (app-rewrite-updates params gvars lvars args accs ast)
  (append '(begin)
          (map (app-compile-var-updates #t params gvars lvars args accs '()) (maybe-rest (assoc 'gvars ast)))
          (map (app-compile-var-updates #f params gvars lvars args accs '()) (maybe-rest (assoc 'lvars ast)))
          (map (app-compile-var-updates #f params gvars lvars args accs (maybe-second (assoc 'lvars-of ast))) (maybe-rest (maybe-rest (assoc 'lvars-of ast))))
          '(1)))
