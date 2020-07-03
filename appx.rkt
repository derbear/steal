#lang racket

;; printf "\nschema\n" && racket appx.rkt asa && printf "\nheader\n" && racket appx.rkt -i asa | jq | tee asa.json && printf "\nclear\n" && racket appx.rkt -c asa | tee asa_clear.teal && printf "\napproval\n" && racket appx.rkt -p asa | tee asa_approve.teal

;; printf "\nschema\n" && racket appx.rkt sectok && printf "\nheader\n" && racket appx.rkt -i sectok | jq | tee sectok.json && printf "\nclear\n" && racket appx.rkt -c sectok | tee sectok_clear.teal && printf "\napproval\n" && racket appx.rkt -p sectok | tee sectok_approve.teal

;; TODO add sovauc-escrow here
;; TODO header removed; reinsert later?

;; printf "\nschema\n" && racket appx.rkt sovauc && printf "\nescrow\n" && racket appx.rkt -e sovauc | tee sovauc_escrow.teal && printf "\nclear\n" && racket appx.rkt -c sovauc | tee sovauc_clear.teal && printf "\napproval\n" && racket appx.rkt -p sovauc | tee sovauc_approve.teal

(require racket/cmdline)

(require "stealc.rkt")

(require "apps/lang.rkt")
(require "apps/assets.rkt")
(require "apps/sectok.rkt")
(require "apps/sovauc.rkt")

(require racket/pretty)

(define show-prog (make-parameter #f))
(define show-clear (make-parameter #f))
(define show-hdr (make-parameter #f))
(define show-escrow (make-parameter #f))

(define app-name
  (command-line
   #:once-any
   [("-p" "--prog") "Show approval program"
                    (show-prog #t)]
   [("-c" "--clr") "Show clear program"
                   (show-clear #t)]
   [("-e" "--escrow") "Show escrow file (if exists)"
                   (show-escrow #t)]
   [("-i" "--hdr") "Show header interface file"
                   (show-hdr #t)]
   #:args (app)
   app))

;; suffix: 0x2087f7ac26536f1f5471074fc799b0b866d8abc4b60b53bbfd2a502da31f8eeaa6311022123120320312103111231231112412111031122512103115320312103116210412311022121031112312103113320312103115320312103120320312103301102412103301182817121037011a00241210113116240c311621050d1031203203121033020033030012103303003304001210330110241210330118281712103301052106121037011a00210612103302102212103302112312103302122512103302152912103303102212103303112412103303122512103303152912103304102105121033040825121033040929121011
(define args
  '((TMPL_CREATOR "Q732YJSTN4PVI4IHJ7DZTMFYM3MKXRFWBNJ3X7JKKAW2GH4O5KTLVA3S6E")
    (TMPL_SUPPLY 10000000)
    (TMPL_DEFAULTFROZEN 1)
    (TMPL_OWNER "Q732YJSTN4PVI4IHJ7DZTMFYM3MKXRFWBNJ3X7JKKAW2GH4O5KTLVA3S6E")))
    ;; (TMPL_APPID 0x0000000000001040)))
    ;; (TMPL_EPREFIX "0x02200704030500020107260208")
    ;; (TMPL_ESUFFIXH "0x019b6ff68c9b836788e8a760acaad12fcaef621a6fb1f1bb28740b418685c3d4")))

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
         [(show-hdr) (displayln (app-header asset-application))]
         [(show-escrow) (raise "no escrow for assets")]
         [else (pretty-print (app-schema asset-application))])]
  [("sectok" "security-token" "security-tokens")
   (cond [(show-prog) (displayln (stealc (stealc-bind (app-program security-token-application) args)))]
         [(show-clear) (displayln (stealc (stealc-bind (app-clear-program security-token-application) args)))]
         [(show-hdr) (displayln (stealc (stealc-bind (app-header security-token-application) args)))]
         [(show-escrow) (raise "no escrow for security token")]
         [else (pretty-print (app-schema security-token-application))])]
  [("sovauc" "auction")
   (cond [(show-prog) (displayln (stealc (stealc-bind (app-program sovauc-application) args)))]
         [(show-clear) (displayln (stealc (stealc-bind (app-clear-program sovauc-application) args)))]
         [(show-hdr) (displayln (stealc (stealc-bind (app-header sovauc-application) args)))]
         [(show-escrow) (displayln (stealc (stealc-bind sovauc-escrow args)))]
         [else (pretty-print (app-schema sovauc-application))])]
  [else (raise (format "unknown app name ~a" app-name))])

;;;;;;;;;;;;;;;;


   
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
