#lang racket

(define (divisors-count n)
  (define (divisors-count-iter i count)
    (if (or (< i n) (= i n))
      (if (= (remainder n i) 0)
        (divisors-count-iter (+ i 1) (+ count 1))
        (divisors-count-iter (+ i 1) count)
      )
      count
    )
  )
  (divisors-count-iter 1 0)
)

; Let's find the number of divisors of the numbers 2 and 10.
; Expected result: 2 and 4
; (divisors-count 2)
; (divisors-count 10)
