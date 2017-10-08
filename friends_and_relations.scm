(load "shadows.scm")
(define (set? l)
  (cond ((null? l) #t)
        ((member? (car l) (cdr l)) #f)
        (else (set? (cdr l)))))

(define (makeset l)
  (cond ((null? l) '())
        (else (cons (car l)
                    (makeset
                      (multi-rember (car l) (cdr l)))))))

(define (subset? s1 s2)
  (if (null? s1)
      #t
      (and (member? (car s1) s2)
           (subset? (cdr s1) s2))))

(define (eqset? s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))
