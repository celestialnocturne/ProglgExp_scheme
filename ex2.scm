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
(define search
  (lambda (list name dpt)
    (cond ((equal? name (car list)) dpt)
          ((null? (cdr list)) 0)
          (else  (apply + (map (lambda (t) (search t name (+ dpt 1))) (cdr list))))
          )))

(define get-cousin
  (lambda (list name)
    (get-depth list (search list name 0))
    ))


;ex2-3
(define get-path
  (lambda (list name)
    (cond ((equal? name (car list)) `(,name))
          ((< 1 (search (cdr list) name 0)) `(,(car list)))
          ((null? (cdr list)) '() )
          
          (else  (map (lambda (t) (get-path t name)) (cdr list)))
          )))
