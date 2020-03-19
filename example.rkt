#lang racket

(require "stealc.rkt")

(require "examples/atomicswap.rkt")
(require "examples/feedynamic.rkt")
(require "examples/split.rkt")
(require "examples/feeproxy.rkt")
(require "examples/periodicpayment.rkt")
(require "examples/limitorder.rkt")

(require "apps/lang.rkt")
(require "apps/assets.rkt")

(require racket/pretty)

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

;; (displayln (stealc
;;             '(begin
;;                a
;;                (d e f)
;;                g)))

;; (displayln (stealc
;;             '(cond
;;                [else d e f])))

;; (displayln (stealc
;;             '(cond
;;                [(= a 0) b g]
;;                [(= a 1) c]
;;                [else d e f])))

;; (displayln (stealc
;;             '(begin
;;                (unless (= a 0)
;;                  (x a b)
;;                  s
;;                  y)
;;                (x c a))))

;; (displayln (stealc
;;             '(begin
;;                (when (= a 0)
;;                  (x a b)
;;                  s
;;                  y)
;;                (x c a))))
               
;; (displayln (stealc
;;             '(begin
;;                (when (= a 0)
;;                  (error "asdf")
;;                  s
;;                  y)
;;                (x c a))))


;; TODO
;; fast assertions (use bnz + err)

;; (displayln (app-schema asset-application))
(displayln (stealc (app-program asset-application)))
(pretty-print (stealc-flatten-begin (app-program asset-application)))
;; (displayln (stealc (app-clear-program asset-application)))
;; (pretty-print (app-clear-program asset-application))

(define args
  '((TMPL_FEE 100000)
    (TMPL_AMT 1000000)
    (TMPL_MINPAY 1000000)
    (TMPL_MINTRD 1000000)
    (TMPL_RATN 2)
    (TMPL_RATD 5)
    (TMPL_RCV "YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ")
    (TMPL_AUTH "YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ")
    (TMPL_OWN "CVMUT7RKA3XBHQVVTGBV5EKC7M7ZSCHZMHQQ3MCOCWSKWBH7PVIQ43YGGY")
    (TMPL_CLS "CVMUT7RKA3XBHQVVTGBV5EKC7M7ZSCHZMHQQ3MCOCWSKWBH7PVIQ43YGGY")
    (TMPL_RCV1 "YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ")
    (TMPL_RCV2 "CVMUT7RKA3XBHQVVTGBV5EKC7M7ZSCHZMHQQ3MCOCWSKWBH7PVIQ43YGGY")
    (TMPL_FV 100000)
    (TMPL_LV 100000)
    (TMPL_DUR 500)
    (TMPL_PERIOD 10000)
    (TMPL_TIMEOUT 300000)
    (TMPL_EXPIRE 300000)
    (TMPL_HASHFN sha256)
    (TMPL_X "uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkk=")
    (TMPL_HASHIMG "uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkk=")
    (TMPL_ASSET 42)))

(display "\n")
(display "atomic swap\n:::::\n\n")
(display (stealc (stealc-bind atomicswap args)))
(display "\n")

(display "\n")
(display "dynamic fee\n:::::\n\n")
(display (stealc (stealc-bind feedynamic args)))
(display "\n")

(display "\n")
(display "periodic payment\n:::::\n\n")
(display (stealc (stealc-bind periodicpayment args)))
(display "\n")

(display "\n")
(display "periodic payment (escrow)\n:::::\n\n")
(display (stealc (stealc-bind periodicpayment-escrow args)))
(display "\n")

(display "\n")
(display "split\n:::::\n\n")
(display (stealc (stealc-bind split args)))
(display "\n")

(display "\n")
(display "delegate keyreg\n:::::\n\n")
(display (stealc (stealc-bind feeproxykeyreg args)))
(display "\n")

;; (display "\n")
;; (display "delegate keyreg (revocable)\n:::::\n\n")
;; (display (stealc (stealc-bind feeproxykeyreg-revocable args)))
;; (display "\n")

(display "\n")
(display "limit order\n:::::\n\n")
(display (stealc (stealc-bind limitorder args)))
(display "\n")

;; (display "\n")
;; (display "limit order (fill)\n:::::\n\n")
;; (display (stealc (stealc-bind limitorder-fill args)))
;; (display "\n")
