(load "base.scm")

(define (looking a lat)
  (keep-looking a (pick 0 lat) lat))

(define (keep-looking needle to-check lat)
  (cond ((eq? needle to-check) #t)
        ((number? to-check)
         (keep-looking needle (pick to-check lat) lat))
        (else #f)))
