#lang racket

(provide split)

(define split-core
  '(and (= (global GroupSize) 2)
        (= (txn TypeEnum) 1)
        (= (gtxn 0 Sender) (gtxn 1 Sender))))

(define split-transfer
  '(and (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr Alice))
        (= (gtxn 1 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 1 Receiver) (addr Bob))

        (< (gtxn 0 Fee) (+ (global MinTxnFee) (/ (gtxn 0 Amount) 100)))
        (< (gtxn 1 Fee) (+ (global MinTxnFee) (/ (gtxn 1 Amount) 100)))

        (= (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 60) 100))

        ;; prevent drainage via small fees; loss due to imprecision
        (> (gtxn 0 Amount) 10000)))

(define split-close
  '(and (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr Alice))
        (= (gtxn 1 CloseRemainderTo) (addr Bob))
        (= (gtxn 1 Receiver) (global ZeroAddress))

        (< (gtxn 0 Fee) (+ (global MinTxnFee) (/ (gtxn 0 Amount) 100)))
        (< (gtxn 1 Fee) (+ (global MinTxnFee) (/ (gtxn 1 SenderBalance) 100)))

        (= (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 SenderBalance)) 60) 100))))

;; This is an escrow.
(define split
  `(and ,split-core
        (or ,split-transfer
            ,split-close)))
