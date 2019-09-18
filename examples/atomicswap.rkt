#lang racket

(provide atomicswap)

(define atomicswap
  '(&& (< (txn Fee) (/ (txn Amount) 33))
       (|| (&& (= (txn CloseRemainderTo) (addr Alice))
               (= (txn Receiver) (addr Alice))
               (= (hash arg_0) (byte base64 Image)))
           (&& (= (txn CloseRemainderTo) (addr Bob))
               (= (txn Receiver) (addr Bob))
               (> (global Round) 3000)))))
