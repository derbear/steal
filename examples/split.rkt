#lang racket

(provide split)

(define split
  '(&& (= (gtxn 1 CloseRemainderTo) (addr "Alice"))
       (= (gtxn 1 Receiver) (addr "Alice"))
       (= (gtxn 2 CloseRemainderTo) (addr "Bob"))
       (= (gtxn 2 Receiver) (addr "Bob"))
       (< (gtxn 1 Fee) (/ (gtxn 1 Amount) 100))
       (< (gtxn 2 Fee) (/ (gtxn 2 Amount) 100))
       (> (gtxn 1 Amount) (/ (* (+ (gtxn 1 Amount) (gtxn 2 Amount)) 60) 100))
       (< (gtxn 1 Amount) (/ (* (+ (gtxn 1 Amount) (gtxn 2 Amount)) 61) 100))
       (> (gtxn 2 Amount) (/ (* (+ (gtxn 1 Amount) (gtxn 2 Amount)) 40) 100))
       (< (gtxn 2 Amount) (/ (* (+ (gtxn 1 Amount) (gtxn 2 Amount)) 41) 100))))
