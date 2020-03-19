#lang racket

(require racket/trace)

(provide stealc)
(provide stealc-bind)

(define [stealc prog]
  (string-join (map stealc-postprocess (stealc-lines (stealc-preprocess prog))) "\n"))

(define [stealc-lines prog]
  (if (list? prog)
      (stealc-lines-special prog)
      (list (stealc-arg prog))))

(define next-label-number 0)
(define (gen-label! pfx)
  (let ([num next-label-number])
    (set! next-label-number (+ 1 next-label-number))
    (string-append pfx (number->string num))))

(define [stealc-lines-if prog]
  (let ([then-entry (gen-label! "if")]
        [otherwise-entry (gen-label! "if_end")])
    (append (stealc-lines (second prog)) ; test body
            (list (string-append "bnz " then-entry))
            (stealc-lines (fourth prog)) ; else body
            (list "int 1")
            (list (string-append "bnz " otherwise-entry))
            (list (string-append then-entry ":"))
            (stealc-lines (third prog)) ; then body
            (list (string-append otherwise-entry ":")))))
  
(define [stealc-lines-begin prog]
  (if (null? prog)
      prog
      (append (stealc-lines (first prog)) (stealc-lines-begin (rest prog)))))

(define [stealc-lines-cond prog]
  (let ([end-label (gen-label! "cond_end")])
    (append (stealc-lines-cond-recur prog end-label #t)
            (list (string-append end-label ":")))))

(define [stealc-lines-cond-recur prog end-label first?]
  (cond [(null? prog) prog]
        [(eq? (first (first prog)) 'else)
         (append (stealc-lines-begin (rest (first prog)))
                 (if first?
                     '()
                     (list "int 1"
                           (string-append "bnz " end-label))))]
        [else
         (let ([test (first (first prog))]
               [body (rest (first prog))]
               [body-label (gen-label! "cond")])
           (append (stealc-lines test)
                   (list (string-append "bnz " body-label))
                   (stealc-lines-cond-recur (rest prog) end-label #f)
                   (list (string-append body-label ":"))
                   (stealc-lines-begin body)
                   (if first?
                       '()
                       (list (string-append "bnz " end-label)))
                   ))]))

(define [stealc-lines-unless prog]
  (let ([test (first prog)]
        [body (rest prog)]
        [after-label (gen-label! "unless")])
    (append (stealc-lines test)
            (list (string-append "bnz " after-label))
            (stealc-lines-begin body)
            (list (string-append after-label ":")))))

(define [stealc-lines-when prog]
  (let ([test (first prog)]
        [body (rest prog)]
        [after-label (gen-label! "when")])
    (append (stealc-lines test)
            (list "!")
            (list (string-append "bnz " after-label))
            (stealc-lines-begin body)
            (list (string-append after-label ":")))))

(define [stealc-lines-error prog]
  (append (list (string-append "// " (if (null? prog) "" (first prog))))
          (stealc-lines '(/ 1 0))))

(define [stealc-stateful-op? op]
  (or (eq? op 'app-opted-in)
      (eq? op 'app-read-global)
      (eq? op 'app-read-local)
      (eq? op 'app-write-global!)
      (eq? op 'app-write-local!)))

(define [stealc-lines-stateful prog]
  (let ([op (cond [(eq? (first prog) 'app-opted-in) 'app_opted_in]
                  [(eq? (first prog) 'app-read-global) 'app_read_global]
                  [(eq? (first prog) 'app-read-local) 'app_read_local]
                  [(eq? (first prog) 'app-write-global!) 'app_write_global]
                  [(eq? (first prog) 'app-write-local!) 'app_write_local])])
    (if (or (eq? (first prog) 'app-read-global)
            (eq? (first prog) 'app-read-local))
        (stealc-lines `(begin ,(cons op (rest prog)) (pop)))
        (stealc-lines (cons op (rest prog))))))

(define [stealc-lines-special prog]
  (cond [(eq? (first prog) 'if) (stealc-lines-if prog)]
        [(eq? (first prog) 'begin) (stealc-lines-begin (rest prog))]
        [(eq? (first prog) 'cond) (stealc-lines-cond (rest prog))]
        [(eq? (first prog) 'unless) (stealc-lines-unless (rest prog))]
        [(eq? (first prog) 'when) (stealc-lines-when (rest prog))]
        [(eq? (first prog) 'error) (stealc-lines-error (rest prog))]
        [(stealc-stateful-op? (first prog)) (stealc-lines-stateful prog)]
        [else
         (let ([lines (append (stealc-lines-recur (rest prog))
                              (list (stealc-op (first prog))))])
           (stealc-flatten prog lines))]))

(define [stealc-lines-recur prog]
  (cond [(null? prog) prog]
        [(list? prog)
         (append (stealc-lines (first prog))
                 (stealc-lines-recur (rest prog)))]
        [else (list (stealc-arg prog))]))

(define [stealc-op op]
  (cond [(eq? op '||) "||"]
        [(eq? op '=) "=="]
        [(eq? op 'and) "&&"]
        [(eq? op 'or) "||"]
        [(eq? op 'not) "!"]
        [else (symbol->string op)]))

(define [stealc-arg arg]
  (cond [(symbol? arg) (symbol->string arg)]
        [(number? arg) (number->string arg)]
        [else arg]))

(define [stealc-flatten prog lines]
  (if (list? lines)
      (cond [(eq? (first prog) 'txn) (stealc-flatten-txn lines)]
            [(eq? (first prog) 'gtxn) (stealc-flatten-gtxn lines)]
            [(eq? (first prog) 'txna) (stealc-flatten-txna lines)]
            [(eq? (first prog) 'addr) (stealc-flatten-addr lines)]
            [(eq? (first prog) 'global) (stealc-flatten-global lines)]
            [(eq? (first prog) 'byte) (stealc-flatten-byte lines)]
            [(eq? (first prog) 'int) (stealc-flatten-int lines)]
            [else lines])
      lines))

(define [stealc-flatten-txn lines]
  (list (string-join (list (second lines) (first lines)) " ")))

(define [stealc-flatten-txna lines]
  (list (string-join (list (third lines) (first lines) (second lines)) " ")))

(define [stealc-flatten-addr lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-global lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-int lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-byte lines]
  (list (string-join (list (third lines) (first lines) (second lines)) " ")))

(define [stealc-flatten-gtxn lines]
  (stealc-flatten-byte lines))

(define [stealc-preprocess prog]
  (if (list? prog)
      (stealc-preprocess-expand prog)
      prog))

(define [stealc-preprocess-expand prog]
  (if (null? prog)
      prog
      (cond [(eq? (first prog) '&&) (stealc-preprocess-distribute prog)]
            [(eq? (first prog) '||) (stealc-preprocess-distribute prog)]
            [(eq? (first prog) 'and) (stealc-preprocess-distribute prog)]
            [(eq? (first prog) 'or) (stealc-preprocess-distribute prog)]
            [else (cons (stealc-preprocess (first prog)) (map stealc-preprocess (rest prog)))])))

(define [stealc-preprocess-distribute prog]
  (cond [(null? (rest prog)) (rest prog)]
        [(null? (rest (rest (rest prog)))) (list (first prog) (stealc-preprocess (second prog)) (stealc-preprocess (third prog)))]
        [else (let ([expr (dq prog)])
                (list (first prog)
                      (stealc-preprocess (second expr))
                      (stealc-preprocess (first expr))))]))

(define [stealc-postprocess line]
  (if (eq? (string->number line) #f)
      line
      (string-append "int " line)))

(define [stealc-bind prog args]
  (if (and (list? prog) (not (null? prog)))
      (cond [(null? (first prog)) prog]
            [(list? (first prog)) (cons (stealc-bind (first prog) args) (stealc-bind (rest prog) args))]
            [(symbol? (first prog))
             (if (assoc (first prog) args)
                 (cons (second (assoc (first prog) args)) (stealc-bind (rest prog) args))
                 (cons (first prog) (stealc-bind (rest prog) args)))]
            [else prog])
      prog))

(define [dq l]
  (let ([lrev (reverse l)])
    (list (first lrev) (reverse (rest lrev)))))
