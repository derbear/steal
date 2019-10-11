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

(define templates
  `(["atomic-swap" ,atomicswap]
    ["dynamic-fee" ,feedynamic]
    ["periodic-payment" ,periodicpayment]
    ["periodic-payment-escrow" ,periodicpayment-escrow]
    ["split" ,split]
    ["delegate-key-registration" ,feeproxykeyreg]
    ["limit-order" ,limitorder]))
    ;; ["limit-order-fill" ,limitorder-fill]))

(define template-ext ".teal.tmpl")

(for-each (lambda (x)
            (let ([fname (first x)]
                  [template-data (second x)])
              (call-with-output-file
                (build-path (dirname) (string-append fname template-ext))
                #:exists 'truncate
                (lambda (out) (display (stealc template-data) out)))))
            templates)
