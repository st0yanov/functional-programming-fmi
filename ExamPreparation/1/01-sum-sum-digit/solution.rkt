#lang racket

(define (sum-sum-digit a b k)
  (define (sum-digits x)
    (define (sum-digits-iter n sum)
      (if (= n 0)
        sum
        (sum-digits-iter (quotient n 10) (+ sum (remainder n 10)))
      )
    )
    (sum-digits-iter x 0)
  )

  (define (sum-range a b n sum)
    (if (and (> n (- a 1)) (< n (+ b 1)))
      (if (= (remainder (sum-digits n) k) 0)
        (sum-range a b (+ n 1) (+ sum n))
        (sum-range a b (+ n 1) sum)
      )
      sum
    )
  )

  (sum-range a b a 0)
)

(sum-sum-digit 2 10 2)
(sum-sum-digit 3 11 2)
