(define [feecompensate con ntxn addrs fees]
  (if (null? addrs)
      con
      (let [con1 (append con
                         `(&& (= (gtxn (+ 1 ,ntxn) receiver) (first addrs))
                              (= (gtxn (+ 1 ,ntxn) fee) (first fees))))]
        (feecompensate con1 (+ 1 ntxn) (rest addrs) (rest fees)))))

(define [feeauction con ntxn price]
  (append con
          `(&& (= (gtxn (+ 1 ,ntxn) amount price)))))
