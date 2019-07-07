;ex1-1
(define TREE '(1 (2 (3 4)) 6 (7 8 9)) )

(define map-tree
  (lambda (f TREE)
    (cond ((null? TREE) '())
          ((pair? TREE)   (cons (map-tree f (car TREE)) (map-tree f (cdr TREE))))
          (else        (f TREE))
          )))

;ex1-2
(define map-tree2
  (lambda (f TREE)
    (cond ((null? TREE) '())
          ((pair? TREE)  (map (lambda (t) (map-tree2 f t)) TREE))
          (else        (f TREE))
          )))

