(define kakeizu
  (read
     (open-input-file "kakeizu")))

(define myappend
  (lambda (ls)
    (if (null? ls)
        '()
        (append (car ls)
                (myappend (cdr ls))
                ))))

;ex2-1
(define get-depth
  (lambda (list dpt)
    (cond ((= 1 dpt) (cons (car list) '()))
          (else (map (lambda (t) (map-tree2 f t-1)) TREE))
          )))