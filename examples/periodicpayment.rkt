#lang racket

(provide periodicpayment)
(provide periodicpayment-escrow)

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
        (> (txn FirstValid) (int TMPL_TIMEOUT))
        (= (txn Amount) 0)))

;; Allows the periodic withdrawal of funds to some account.
;; This is delegate logic.
;;
;; This allows TMPL_RCV to withdraw TMPL_AMT every
;; TMPL_PERIOD rounds for TMPL_DUR after every multiple
;; of TMPL_PERIOD.
(define periodicpayment
  `(and ,periodicpayment-core
        ,periodicpayment-transfer))

;; Allows the periodic withdrawal of funds to some account.
;; This is an escrow.
;;
;; This allows TMPL_RCV to withdraw TMPL_AMT every
;; TMPL_PERIOD rounds for TMPL_DUR after every multiple
;; of TMPL_PERIOD.
;;
;; After TMPL_TIMEOUT, all remaining funds in the escrow
;; are available to TMPL_RCV.
(define periodicpayment-escrow
  `(and ,periodicpayment-core
        (or ,periodicpayment-transfer
            ,periodicpayment-close)))
