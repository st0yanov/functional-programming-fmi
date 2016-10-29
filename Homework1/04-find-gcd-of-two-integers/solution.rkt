#lang racket

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))
  )
)

; Let's find some GCDs to check the corectness of the function.
; Expected results: 5, 5, 10, 5, 10, 15
; (gcd 5 10)
; (gcd 10 5)
; (gcd 20 10)
; (gcd 20 5)
; (gcd 50 30)
; (gcd 30 45)
