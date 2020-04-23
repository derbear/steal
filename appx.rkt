#lang racket

;; printf "\nschema\n" && racket appx.rkt && printf "\nclear\n" && racket appx.rkt -c | tee appclear.teal && printf "\napproval\n" && racket appx.rkt -p | tee appprog.teal

(require racket/cmdline)

(require "stealc.rkt")

(require "apps/lang.rkt")
(require "apps/assets.rkt")
(require "apps/sectok.rkt")

(require racket/pretty)

(define show-prog (make-parameter #f))
(define show-clear (make-parameter #f))

(define cmd-options
  (command-line
   #:once-any
   [("-p" "--prog") "Show approval program"
                    (show-prog #t)]
   [("-c" "--clr") "Show clear program"
                   (show-clear #t)]))

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

(displayln (stealc (app-program security-token-application)))
(pretty-print (app-program security-token-application))

;; (cond [(show-prog) (displayln (stealc (stealc-bind (app-program asset-application) args)))]
;;       [(show-clear) (displayln (stealc (stealc-bind (app-clear-program asset-application) args)))]
;;       [else (pretty-print (app-schema asset-application))])

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
