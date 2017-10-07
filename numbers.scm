(define (add1 num)
  (+ num 1))

(define (sub1 num)
  (- num 1))

(define (add a b)
  (if (zero? b) a (add (add1 a) (sub1 b))))

(define (sub a b)
  (if (zero? b) a (sub (sub1 a) (sub1 b))))
