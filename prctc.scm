(define numlist
  (lambda (n)
    (if (= n 1)
        '(1)
        (cons n
              (numlist (- n 1))
              ))))


(define myappend
  (lambda (ls)
    (if (null? ls)
        '()
        (append (car ls)
                (myappend (cdr ls))
                ))))


(define times
  (lambda (n ls)
    (map
     (lambda (x) (* n x))
      ls)
    ))


(define rms
  (lambda (data)
    (sqrt (/
           (apply + (map (lambda (x) (* x x)) data))
           (length data)))
))    
