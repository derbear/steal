#lang racket

(provide split)

;; TODO substitute with SenderBalance deprecation

(define split-core
  '(and (= (global GroupSize) 2)
        (= (txn TypeEnum) 1)
        (= (gtxn 0 Sender) (gtxn 1 Sender))))

(define split-transfer
  '(and (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr TMPL_RCV1))
        (= (gtxn 1 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 1 Receiver) (addr TMPL_RCV2))

        (< (gtxn 0 Fee) (int TMPL_FEE))
        (< (gtxn 1 Fee) (int TMPL_FEE))

        (= (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) (int TMPL_RATN)) (int TMPL_RATD)))

        ;; prevent drainage via small fees; loss due to imprecision
        (> (gtxn 0 Amount) (int TMPL_MINPAY))))

(define split-close
  '(and (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr TMPL_RCV1))
        (= (gtxn 1 CloseRemainderTo) (addr TMPL_RCV2))
        (= (gtxn 1 Receiver) (global ZeroAddress))

        (< (gtxn 0 Fee) (int TMPL_FEE))
        (< (gtxn 1 Fee) (int TMPL_FEE))

        (= (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 SenderBalance)) (int TMPL_RATN)) (int TMPL_RATD)))))

;; This is an escrow.
(define split
  `(and ,split-core
        (or ,split-transfer
            ,split-close)))
