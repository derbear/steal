#lang racket

(provide feedynamic)

;; This is delegate logic.
(define feedynamic
  '(and (= (global GroupSize) 2)

        (= (txn GroupIndex) 0)
        (= (txn TypeEnum) 1)
        (= (txn Sender) (addr Alice))
        (= (txn Receiver) (addr Bob))
        (= (txn CloseRemainderTo) (addr Bob))
        (= (txn Amount) 900000)
        (= (txn FirstValid) 100000)
        (= (txn LastValid) 100500)
        (= (txn Lease) (byte base32 DYNAMICFEE))

        (= (gtxn 1 TypeEnum) 1)
        (= (gtxn 1 Receiver) (addr Alice))
        (= (gtxn 1 Amount) (txn Fee))))
