(load "base.scm")
(define (rember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else (cons (car l) (rember-f test? a (cdr l)))))))

(define (insert-f test? merge)
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((test? old (car lat)) (merge new old (cdr lat)))
          (else (cons (car lat) ((insertL-f test? merge) new old (cdr lat)))))))

(define (insertL-f test?)
  (insert-f test? (lambda (new old lat)
                    (cons new (cons old lat)))))

(define (insertR-f test?)
  (insert-f test? (lambda (new old lat)
                    (cons old (cons new lat)))))

(define (subst-f test?)
  (insert-f test? (lambda (new old lat)
                    (cons new lat))))
