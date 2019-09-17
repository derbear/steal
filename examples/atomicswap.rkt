#lang racket

(provide atomicswap)

(define atomicswap
  '(&& (< (txn Fee) (/ (txn Amount) 33))
       (|| (&& (= (txn CloseRemainderTo) (addr "Alice"))
               (= (txn Receiver) (addr "Alice"))
               (= (hash arg0) (byte base64 "X")))
           (&& (= (txn CloseRemainderTo) (addr "Bob"))
               (= (txn Receiver) (addr "Bob"))
               (> (global Round) 3000)))))
