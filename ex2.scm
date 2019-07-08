(define kakeizu
  (read
     (open-input-file "kakeizu")))

(define myappend
  (lambda (ls)
    (apply myappend ls)
                ))

;ex2-1
(define get-depth
  (lambda (list dpt)
    (cond ((= 1 dpt) (map car (cdr list)))
          (else (apply append (map (lambda (t) (get-depth t (- dpt 1))) (cdr list))))
          )))

;ex2-2