#lang racket

(require racket/cmdline)

(require "stealc.rkt")

(require "examples/atomicswap.rkt")
(require "examples/feedynamic.rkt")
(require "examples/split.rkt")
(require "examples/feeproxy.rkt")
(require "examples/periodicpayment.rkt")
(require "examples/limitorder.rkt")

(define dirname (make-parameter ""))

(define output-dir
  (command-line
   #:once-each
   [("-d" "--directory") n "Output directory" (dirname n)]))

(define log-exn (lambda (exn) (printf "drop exception ~a~n" exn)))

(with-handlers ([exn:fail:filesystem:exists? log-exn])
  (make-directory (dirname)))

(define (comment-out str)
  (string-join (map (lambda (s) (if (string=? s "") "//" (string-append "// " s)))
                    (string-split str "\n"))
               "\n"))

(define templates
  `(["atomic-swap" ,atomicswap ,atomicswap-doc]
    ["dynamic-fee" ,feedynamic ,feedynamic-doc]
    ;; ["periodic-payment" ,periodicpayment]
    ["periodic-payment-escrow" ,periodicpayment-escrow ,periodicpayment-escrow-doc]
    ["split" ,split ,split-doc]
    ["delegate-key-registration" ,feeproxykeyreg ,feeproxykeyreg-doc]
    ["limit-order" ,limitorder ,limitorder-doc]))
    ;; ["limit-order-fill" ,limitorder-fill]))

(define template-ext ".teal.tmpl")

(for-each (lambda (x)
            (let ([fname (first x)]
                  [template-data (second x)]
                  [template-doc (third x)])
              (call-with-output-file
                (build-path (dirname) (string-append fname template-ext))
                #:exists 'truncate
                (lambda (out)
                  (displayln (comment-out template-doc) out)
                  (display (stealc template-data) out)))))
            templates)
