#lang racket

(provide split)

;; TODO add gtxn size
(define split
  '(&& (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
       (= (gtxn 0 Receiver) (addr Alice))
       (= (gtxn 1 CloseRemainderTo) (global ZeroAddress))
       (= (gtxn 1 Receiver) (addr Bob))
       (< (gtxn 0 Fee) (/ (gtxn 0 Amount) 100))
       (< (gtxn 1 Fee) (/ (gtxn 1 Amount) 100))
       (> (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 60) 100))
       (< (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 61) 100))
       (> (gtxn 1 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 40) 100))
       (< (gtxn 1 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) 41) 100))))
