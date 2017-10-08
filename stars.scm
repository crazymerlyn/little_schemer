(load "base.scm")
(define (rember* a l)
  (cond ((null? l) '())
        ((eq? a (car l)) (rember* a (cdr l)))
        ((not (atom? (car l))) (cons (rember* a (car l))
                                     (rember* a (cdr l))))
        (else (cons (car l) (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond
           ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
           (else (cons (car l) (insertR* new old (cdr l))))))
        (else (cons (insertR* new old (car l))
                    (insertR* new old (cdr l))))))

(load "numbers.scm")
(define (occur* a l)
  (cond ((null? l) 0)
        ((atom? (car l)) (if (eq? a (car l))
                             (add1 (occur* a (cdr l)))
                             (occur* a (cdr l))))
        (else (add (occur* a (car l))
                   (occur* a (cdr l))))))

(define (subst* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? old (car l))
             (cons new (subst* new old (cdr l)))
             (cons (car l) (subst* new old (cdr l)))))
        (else (cons (subst* new old (car l))
                    (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (if (eq? old (car l))
             (cons new (cons old (insertL* new old (cdr l))))
             (cons (car l) (insertL* new old (cdr l)))))
        (else (cons (insertL* new old (car l))
                    (insertL* new old (cdr l))))))
