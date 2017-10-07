(define (add1 num)
  (+ num 1))

(define (sub1 num)
  (- num 1))

(define (add a b)
  (if (zero? b) a (add (add1 a) (sub1 b))))

(define (sub a b)
  (if (zero? b) a (sub (sub1 a) (sub1 b))))


(define (addtup tup)
  (if (null? tup)
      0
      (add (car tup) (addtup (cdr tup)))))

(define (mul n m)
  (if (zero? m) 0 (add n (mul n (sub1 m)))))


(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (add (car tup1) (car tup2))
                    (tup+ (cdr tup1) (cdr tup2))))))
