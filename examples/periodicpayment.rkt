(define periodicpayment
  '(&& (= (txn closeremainderto) "Zero")
       (= (txn receiver) "Alice")
       (= (txn amount) 100,000)
       (< (txn fee) 10,000)
       (= (% (txn firstvalid 100,000)))
       (= 500 (- (txn firstvalid) (txn lastvalid)))
       (= (len (txn note)) 0)))
