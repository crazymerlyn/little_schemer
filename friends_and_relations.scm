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
