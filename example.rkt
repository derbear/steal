#lang racket

(require "stealc.rkt")

(require "examples/atomicswap.rkt")
(require "examples/feedynamic.rkt")
(require "examples/split.rkt")
(require "examples/feeproxy.rkt")
(require "examples/periodicpayment.rkt")
(require "examples/limitorder.rkt")


(define args
  '((Alice "YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ")
    (Bob "CVMUT7RKA3XBHQVVTGBV5EKC7M7ZSCHZMHQQ3MCOCWSKWBH7PVIQ43YGGY")
    (hash sha256)
    (Image "uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkk=")
    (DerekCoin "uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkm4VUSGMGmSkg==")))

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

(display "\n")
(display "limit order\n:::::\n\n")
(display (stealc (stealc-bind limitorder args)))
(display "\n")

(display "\n")
(display "limit order (fill)\n:::::\n\n")
(display (stealc (stealc-bind limitorder-fill args)))
(display "\n")
