(load "stars.scm")
(define (numbered? l)
  (cond ((atom? l) (number? l))
        (else (and (numbered? (car l))
                   (numbered? (caddr l))))))

(define (value nexp)
  (cond ((atom? nexp) nexp)
        ((eq? (cadr nexp) '+)
         (add (value (car nexp))
              (value (caddr nexp))))
        ((eq? (cadr nexp) '*)
         (mul (value (car nexp))
              (value (caddr nexp))))
        (else
         (pow (value (car nexp))
              (value (caddr nexp))))))
