#lang racket

(define (prime? n)
  (define square-root-of-n (sqrt n))

  (define (prime-iter divisor)
    (cond
      [(> divisor square-root-of-n) #t]
      [(= (remainder n divisor) 0) #f]
      [else (prime-iter (+ divisor 1))]
    )
  )

  (if (or (= n 1) (< n 1))
    #f
    (prime-iter 2)
  )
)

(define (sum-prime-numbers n k)
  (define (helper i counter sum)
    (if (< counter n)
      (if (prime? i)
        (helper (+ i 1) (+ counter 1) (+ sum i))
        (helper (+ i 1) counter sum)
      )
      sum
    )
  )
  (helper (+ k 1) 0 0)
)

; Let's find the sum of the first 5 prime numbers.
; Expected result: 28
; (sum-prime-numbers 5 1)
