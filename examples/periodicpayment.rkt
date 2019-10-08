#lang racket

(require racket/pretty)

(provide periodicpayment)
(provide periodicpayment-escrow)

;; TODO consider asset counters...

;; TODO substitute with SenderBalance deprecation

(define periodicpayment-core
  '(and (= (txn TypeEnum) 1)
        (< (txn Fee) (int TMPL_FEE))
        (= (% (txn FirstValid) (int TMPL_PERIOD)) 0)
        (= (txn LastValid) (+ (int TMPL_DUR) (txn FirstValid)))
        (= (txn Lease) (byte base64 TMPL_X))))

(define periodicpayment-transfer
  '(and (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Receiver) (addr TMPL_RCV))
        (= (txn Amount) (int TMPL_AMT))))

(define periodicpayment-close
  '(and (= (txn CloseRemainderTo) (addr TMPL_RCV))
        (= (txn Receiver) (global ZeroAddress))
        (< (txn SenderBalance) (int TMPL_AMT))
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
