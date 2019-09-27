#lang racket

(require racket/pretty)

(provide periodicpayment)
(provide periodicpayment-escrow)

;; TODO consider asset counters...

(define periodicpayment-core
  '(and (= (txn TypeEnum) 0)
        (< (txn Fee) (* 10 (global MinTxnFee)))
        (= (% (txn FirstValid) 100000) 0)
        (= (txn LastValid) (+ 500 (txn FirstValid)))
        (= (txn Lease) (byte base32 PERIODICPAYMENT))))

(define periodicpayment-transfer
  '(and (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Receiver) (addr Alice))
        (= (txn Amount) 100000)))

(define periodicpayment-close
  '(and (= (txn CloseRemainderTo) (addr Alice))
        (= (txn Receiver) (global ZeroAddress))
        (< (txn SenderBalance) 100000)
        (= (txn Amount) 0)))

;; This is delegate logic.
(define periodicpayment
  `(and ,periodicpayment-core
        ,periodicpayment-transfer))

;; This is an escrow.
(define periodicpayment-escrow
  `(and ,periodicpayment-core
        (or ,periodicpayment-transfer
            ,periodicpayment-close)))
