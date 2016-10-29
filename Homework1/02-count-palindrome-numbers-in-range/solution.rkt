#lang racket

(define (reverse-number n)
  (define (reverse-number-iter n result)
    (if (= n 0)
      result
      (reverse-number-iter (quotient n 10) (+ (* result 10) (remainder n 10)))
    )
  )
  (reverse-number-iter n 0)
)

(define (palindrome? n)
  (= n (reverse-number n))
)

(define (count-palindromes-in-range a b)
  (define (count-palindromes-in-range-iter i count)
    (if (< i b)
      (if (palindrome? i)
        (count-palindromes-in-range-iter (+ i 1) (+ count 1))
        (count-palindromes-in-range-iter (+ i 1) count)
      )
      count
    )
  )

  (cond
    [(and (> a 0) (> b 0) (< a b)) (count-palindromes-in-range-iter a 0)]
    [else #f]
  )
)

; Let's find the number of palindromes between 1 and 100.
; Expected result: 18
; (count-palindromes-in-range 1 100)
