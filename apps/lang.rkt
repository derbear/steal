#lang racket

(require racket/trace)

(require json)
(require net/base64)

(provide app-schema)
(provide app-header)
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

(define (app-get sym app)
  (if (assoc sym app)
      (rest (assoc sym app))
      '()))
(define (app-vars sym app)
  (app-get sym app))

(define (app-params app) (app-get 'params app))
(define (app-prog app) (app-get 'prog app))
(define (app-onclear app) (app-get 'onclear app))

(define (app-gvars app) (app-vars 'gvars app))
(define (app-lvars app) (app-vars 'lvars app))

(define (app-int? var) (eq? (first (second var)) 'int))
(define (app-blob? var) (and (symbol? (first (second var))) (not (app-int? var))))

(define (app-blobs vars) (length (filter app-blob? vars)))
(define (app-ints vars) (length (filter app-int? vars)))

(define (app-varblob-size var)
  (if (and (list? (first (second var)))
           (not (eq? (first (first (second var))) 'int)))
      (last (first (second var)))
      0))
(define (app-varint-size var)
  (if (and (list? (first (second var)))
           (eq? (first (first (second var))) 'int))
      (last (first (second var)))
      0))

(define (app-varblobs vars) (apply + (map app-varblob-size vars)))
(define (app-varints vars) (apply + (map app-varint-size vars)))

(define (app-gvars-varint app) (app-varints (app-get 'gvars app)))
(define (app-gvars-varblob app) (app-varblobs (app-get 'gvars app)))
(define (app-lvars-varint app) (app-varints (app-get 'lvars app)))
(define (app-lvars-varblob app) (app-varblobs (app-get 'lvars app)))

(define (app-schema app)
  `((global-state-blobs ,(+ (app-gvars-varblob app) (app-blobs (app-gvars app))))
    (global-state-ints ,(+ (app-gvars-varint app) (app-ints (app-gvars app))))
    (local-state-blobs ,(+ (app-lvars-varblob app) (app-blobs (app-lvars app))))
    (local-state-ints ,(+ (app-lvars-varint app) (app-ints (app-lvars app))))))

(define (app-header app)
  ;; (pretty-print (app-header-jexpr app))
  ;; (app-header-jexpr app))
  (jsexpr->string (app-header-jexpr app)))

(define (app-header-jexpr app)
  (hash 'execute (app-header-execute app)
        'query (app-header-queries app)))

(define (app-header-execute app)
  (make-immutable-hash (map (app-header-proc (app-get 'create-procs app)) (app-get 'procs app))))

(define (app-header-proc create-procs)
  (lambda (proc)
    (cons (first proc)
          (apply hash (append (list 'on-completion (symbol->string (second proc))
                                    'help (third proc)
                                    'create (not (not (member (first proc) create-procs))))
                              (app-header-args (rest (rest (rest proc)))))))))

(define (app-header-args proc)
  (if (null? proc)
      '(args () accounts () foreign ())
      (let ([rem (app-header-args (rest proc))])
        (cond
          [(null? (first (first proc)))
           (app-header-arg rem (rest (first proc)) #t #f)]
          [(list? (first (first proc)))
           (app-header-arg rem (rest (first proc)) #t (first (first (first proc))))]
          [else (app-header-arg rem (first proc) #f #f)]))))

(define (app-header-arg rem arg pseudo default)
  (let ([rem-args (second rem)]
        [rem-accounts (fourth rem)]
        [rem-foreign (sixth rem)]
        [hash-base (list 'name (symbol->string (second arg))
                         'help (if (>= (length arg) 3) (third arg) "")
                         'pseudo pseudo)])
    (let ([hash-base (if default
                         (append hash-base (list 'default (symbol->string default)))
                         hash-base)])
      (case (first arg)
        [(account)
         (list 'args rem-args
               'accounts
               (cons (apply hash hash-base)
                     rem-accounts)
               'foreign rem-foreign)]
        [(foreign) 'TODO]
        [else
         (list 'args
               (cons (apply hash
                            (append hash-base
                                    (list 'kind (symbol->string (first arg)))))
                     rem-args)
               'accounts rem-accounts 'foreign rem-foreign)]))))

(define (app-header-queries app)
  (hash 'global (apply hash (apply append (map app-header-var (app-gvars app))))
        'local (apply hash (apply append (map app-header-var (app-lvars app))))))

(define (app-header-var vars)
  (if (list? (first (second vars)))
      (list (first vars)
            (hash 'map (apply hash (append (list 'kind
                                                 (symbol->string (second (first (second vars)))))
                                           (last (second vars))))
                  'size (last (first (second vars)))
                  'kind (symbol->string (third (first (second vars))))
                  'help (last vars)))
      (list (first vars)
            (hash 'key (symbol->string (second (second vars)))
                  'kind (symbol->string (first (second vars)))
                  'help (last vars)))))

(define (app-program app) (app-program-helper (first (app-prog app)) (app-params app) (app-gvars app) (app-lvars app) '() '()))
(define (app-clear-program app) (app-program-helper (first (app-onclear app)) (app-params app) (app-gvars app) (app-lvars app) '() '()))

(define (with-sym sym ast)
  (if (assoc sym (second ast))
      (second (assoc sym (second ast)))
      '()))

(define (with-args ast) (with-sym 'args ast))
(define (with-accs ast) (with-sym 'accs ast))

(define (app-vars-assoc vars sym)
  (cond [(null? vars) #f]
        [(eq? (first (first vars)) sym) (first vars)]
        [else (app-vars-assoc (rest vars) sym)]))

(define (app-rewrite-gvar spec)
  (when (not (or (= (length spec) 2) (= (length spec) 3)))
    (raise "app-rewrite-gvar")) ;; TODO add err
  `(byte base64 ,(symbol->base64 (second (second spec)))))

(define (app-rewrite-lvar spec)
  (when (not (or (= (length spec) 2) (= (length spec) 3)))
    (raise "app-rewrite-lvar")) ;; TODO add err
  `(byte base64 ,(symbol->base64 (second (second spec)))))

;; (trace app-vars-assoc)
;; (trace app-rewrite-gvar)
;; (trace app-rewrite-lvar)

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
        [(app-vars-assoc gvars sym) (app-rewrite-gvar (app-vars-assoc gvars sym))]
        [(app-vars-assoc lvars sym) (app-rewrite-lvar (app-vars-assoc lvars sym))]
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
