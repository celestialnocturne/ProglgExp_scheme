(define myappend
   (lambda (ls)
      (apply append ls)
   ))


;exp3-1 
(define diff
  (lambda (f)
    (cond ((number? f) 0)
          ((equal? 'x f) 1)
          ((symbol? f) f)
          ((equal? (or '+ '-) (car f)) (map diff f))
          ((equal? '- (car f)) (map diff f))
          ((equal? '* (car f)) `(+
                                  (* ,(cadr f) ,(diff (caddr f)))
                                  (* ,(diff (cadr f)) ,(caddr f))))
          
          ((equal? '** (car f)) `(* ,(caddr f)
                                    (* ,(diff (cadr f))
                                        (** ,(cadr f)
                                            ,(- (caddr f) 1)))))
                                       
          (else f)
          )))

;exp3-2
(define ** expt)

(define tt
  (lambda (f dot)
    (let ((dfa 1) (fa 1) (a dot))
      (+ (* dfa (- x a)) fa))
    ))

(define tangent
  (lambda (f dot)
    (let 
         ((fa  ((eval `(lambda (x) ,f) (interaction-environment))dot))
          (dfa ((eval `(lambda (x) ,(diff f)) (interaction-environment))dot))
          (a dot)
         )

         (if (> 0 (- fa (* a dfa))) `(- (* ,dfa  x) ,(- (- fa (* a dfa))))
                                   `(+ (* ,dfa  x) ,(- fa (* a dfa)))
           ))
   ))


;exp3-3 
(define diff2
  (lambda (f d)
    (cond ((number? f) 0)
          ((equal? d f) 1)
          ((and (symbol? f) (not (equal? f (or '+ '- '* '**)))) 0)
          ((symbol? f) f)
          ((equal? (or '+ '-) (car f)) (map (lambda (f) (diff2 f d)) f))
          ((equal? '* (car f)) `(+
                                  (* ,(cadr f) ,(diff2 (caddr f) d))
                                  (* ,(diff2 (cadr f) d) ,(caddr f))))
          
          ((equal? '** (car f)) `(* ,(caddr f)
                                    (* ,(diff2 (cadr f) d)
                                        (** ,(cadr f)
                                            ,(- (caddr f) 1)))))
                                       
          (else 0)
          )))


;exp3-4
(define simple+
  (lambda (lst)
    (let ((nzl (crnzl+ lst)))
    (cond ((null? nzl) 0)
          ((null? (cdr nzl)) (car nzl))
          (else (cons '+ nzl)))
    )))

(define simple-
  (lambda (lst)
    (let ((nzl (crnzl+ (cdr lst))))
    (cond ((null? nzl) (car lst))
          (else (cons '- (cons (car lst) nzl)))
    ))))

(define simple*
  (lambda (lst)
    (let ((p (car lst)) (q (cadr lst)))
    (cond ((or (equal? p 0) (equal? q 0)) 0)
          ((equal? p 1) q)
          ((equal? q 1) p)
          (else (cons '* lst))
          )
      )
    )
  )

(define simple**
  (lambda (lst)
    (let ((p (car lst)) (q (cadr lst)))
    (cond ((equal? q 0) 1)
          ((equal? q 1) p)
          (else (cons '** lst))
          )
      )
    )
  )

(define crnzl+
  (lambda (lst)
    (let ((nzl (map (lambda (t) (if (equal? t 0) '() `(,t))) lst))
         )
      (myappend nzl))
    ))

;(define crnzl-
;  (lambda (lst)
;   (let ((nzl (map (lambda (t) (if (equal? t 0) '() `(,t))) (cdr lst)))
;         )
;     (myappend nzl))
;   ))


(define simple
  (lambda (f)
    (cond ((or (number? f) (symbol? f)) f)
          ((equal? '+ (car f)) (simple+ (map simple (cdr f))))
          ((equal? '- (car f)) (simple- (map simple (cdr f))))
          ((equal? '* (car f)) (simple* (map simple (cdr f))))
          ((equal? '** (car f)) (simple**(map simple (cdr f))))
          (else f))
    ))