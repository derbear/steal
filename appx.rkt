#lang racket

;; printf "\nschema\n" && racket appx.rkt sectok && printf "\nclear\n" && racket appx.rkt -c sectok | tee sectok_clear.teal && printf "\napproval\n" && racket appx.rkt -p sectok | tee sectok_approve.teal

(require racket/cmdline)

(require "stealc.rkt")

(require "apps/lang.rkt")
(require "apps/assets.rkt")
(require "apps/sectok.rkt")

(require racket/pretty)

(define show-prog (make-parameter #f))
(define show-clear (make-parameter #f))

(define app-name
  (command-line
   #:once-any
   [("-p" "--prog") "Show approval program"
                    (show-prog #t)]
   [("-c" "--clr") "Show clear program"
                   (show-clear #t)]
   #:args (app)
   app))

(define args
  '((TMPL_CREATOR "Q732YJSTN4PVI4IHJ7DZTMFYM3MKXRFWBNJ3X7JKKAW2GH4O5KTLVA3S6E")
    (TMPL_SUPPLY 10000000)
    (TMPL_DEFAULTFROZEN 1)))

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

(case app-name
  [("asa" "asset" "assets")
   (cond [(show-prog) (displayln (stealc (stealc-bind (app-program asset-application) args)))]
         [(show-clear) (displayln (stealc (stealc-bind (app-clear-program asset-application) args)))]
         [else (pretty-print (app-schema asset-application))])]
  [("sectok" "security-token" "security-tokens")
   (cond [(show-prog) (displayln (stealc (stealc-bind (app-program security-token-application) args)))]
         [(show-clear) (displayln (stealc (stealc-bind (app-clear-program security-token-application) args)))]
         [else (pretty-print (app-schema security-token-application))])]
  [else (raise (format "unknown app name ~a" app-name))])
   
;; (displayln (stealc (app-program security-token-application)))
;; (pretty-print (app-program security-token-application))

;; ;; (display "\n")
;; ;; (displayln "::::::::::::::::::")
;; ;; (displayln "::::: assets :::::")
;; ;; (displayln "::::::::::::::::::")
;; ;; (display "\n")
;; ;; (displayln "schema:")

;; (displayln (stealc (stealc-bind (app-program asset-application) args)))

;; (cond [(show-prog) (displayln (stealc (stealc-bind (app-program asset-application) args)))]
;;       [(show-clear) (displayln (stealc (stealc-bind (app-clear-program asset-application) args)))]
;;       [else (pretty-print (app-schema asset-application))])

;; ;; (pretty-print (app-schema asset-application))
;; ;; (displayln "program:")

;; ;; (displayln "clear program:")
;; ;; (displayln (stealc (stealc-bind (app-clear-program asset-application) args)))
;; ;; (display "\n")

;; ;; (displayln (app-schema asset-application))
;; ;; (pretty-print (stealc-flatten-begin (app-program asset-application)))

;; ;; (pretty-print (app-clear-program asset-application))
