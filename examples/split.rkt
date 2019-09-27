#lang racket

(provide split)

;; TODO add gtxn size
;; TODO add escape condition OR implement sender balance lookup

(define split-core
  '(and (= (global GroupSize) 2)
        (= (txn TypeEnum 0))
        (= (gtxn 0 Sender) (gtxn 1 Sender))))

(define split-transfer
  '(and (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr Alice))
        (= (gtxn 1 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 1 Receiver) (addr Bob))

        (< (gtxn 0 Fee) (+ (global MinTxnFee) (/ (gtxn 0 Amount) 100)))
        (< (gtxn 1 Fee) (+ (global MinTxnFee) (/ (gtxn 1 Amount) 100)))

        (> (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 59) 100))
        (< (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 61) 100))
        (> (gtxn 1 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 39) 100))
        (< (gtxn 1 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 41) 100))))

(define split-close
  '(and (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr Alice))
        (= (gtxn 1 CloseRemainderTo) (addr Bob))
        (= (gtxn 1 Receiver) (global ZeroAddress))

        (< (gtxn 0 Fee) (+ (global MinTxnFee) (/ (gtxn 0 Amount) 100)))
        (< (gtxn 1 Fee) (+ (global MinTxnFee) (/ (gtxn 1 SenderBalance) 100)))

        (> (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 SenderBalance)) 59) 100))
        (< (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 SenderBalance)) 61) 100))
        (> (gtxn 1 SenderBalance) (/ (* (+ (gtxn 0 Amount) (gtxn 1 SenderBalance)) 39) 100))
        (< (gtxn 1 SenderBalance) (/ (* (+ (gtxn 0 Amount) (gtxn 1 SenderBalance)) 41) 100))))

;; This is an escrow.
(define split
  `(and ,split-core
        (or ,split-transfer
            ,split-close)))
