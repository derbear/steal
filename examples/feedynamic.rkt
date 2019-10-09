#lang racket

(provide feedynamic)

;; Implements a payment transaction with an undetermined fee.
;; This is delegate logic.
;;
;; This must be present on the first of two transactions.
;;
;; The first transaction must be from this account.
;; The Lease is mandatory!
;;
;; The second transaction must send money to this account.
;; It must send an amount equal to txn.Fee.
(define feedynamic
  '(and (= (global GroupSize) 2)

        (= (txn GroupIndex) 0)
        (= (txn TypeEnum) 1)
        (= (txn Receiver) (addr TMPL_RCV))
        (= (txn CloseRemainderTo) (addr TMPL_CLS))
        (= (txn Amount) (int TMPL_AMT))
        (= (txn FirstValid) (int TMPL_FV))
        (= (txn LastValid) (int TMPL_LV))
        (= (txn Lease) (byte base64 TMPL_X))

        (= (gtxn 1 TypeEnum) 1)
        (= (gtxn 1 Receiver) (txn Sender))
        (= (gtxn 1 Amount) (txn Fee))))
