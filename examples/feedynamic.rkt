#lang racket

(provide feedynamic)

;; This is delegate logic.
(define feedynamic
  '(and (= (global GroupSize) 2)

        (= (txn GroupIndex) 0)
        (= (txn TypeEnum) 1)
        (= (txn Receiver) (addr TMPL_RCV))
        (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Amount) TMPL_AMT)
        (= (txn FirstValid) TMPL_FV)
        (= (txn LastValid) TMPL_LV)
        (= (txn Lease) (byte base32 TMPL_X))

        (= (gtxn 1 TypeEnum) 1)
        (= (gtxn 1 Receiver) (txn Sender))
        (= (gtxn 1 Amount) (txn Fee))))
