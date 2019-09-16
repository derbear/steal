(define limitorder-ask
  '(&& (= (gtxn 1 closeremainderto) "Alice")
       (= (gtxn 1 receiver) "Alice")
       (= (gtxn 2 closeremainderto) "Bob")
       (= (gtxn 2 receiver) "Bob")
       (< (gtxn 1 fee) 10,000)
       (< (gtxn 2 fee) 10,000)
       (< (* 3 (gtxn 1 amount)) (* 5 (gtxn-asset 2 "DerekCoin")))))

(define limitorder-bid
  '(&& (= (gtxn 1 closeremainderto) "Alice")
       (= (gtxn 1 receiver) "Alice")
       (= (gtxn 2 closeremainderto) "Bob")
       (= (gtxn 2 receiver) "Bob")
       (< (gtxn 1 fee) 10,000)
       (< (gtxn 2 fee) 10,000)
       (> (* 2 (gtxn 1 amount)) (* 5 (gtxn-asset 2 "DerekCoin")))))
