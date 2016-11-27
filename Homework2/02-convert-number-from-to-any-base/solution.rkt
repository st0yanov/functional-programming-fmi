#lang racket

(define (convert x k n)
  (define (to-digits number base digits)
    (if (> number 0)
      (to-digits (quotient number base) base (append digits (list (remainder number base))))
      (reverse digits)
    )
  )

  (define (from-digits digits base result)
    (if (> (length digits) 0)
      (from-digits (rest digits) base (+ (* result base) (first digits)))
      result
    )
  )

  (from-digits (to-digits (from-digits (to-digits x 10 '()) k 0) n '()) 10 0)
)

; Tests

; Decimal to Binary
(convert 69 10 2)  ;→ 1000101
(convert 123 10 2) ;→ 1111011

; Decimal to Octal
(convert 69 10 8)  ;→ 105
(convert 123 10 8) ;→ 173

; Octal to Binary
(convert 105 8 2)  ;→ 1000101
(convert 173 8 2)  ;→ 1111011

; Octal to Decimal
(convert 105 8 10)  ;→ 69
(convert 173 8 10)  ;→ 123

; Binary to Octal
(convert 1000101 2 8)  ;→ 105
(convert 1111011 2 8) ;→ 173

; Binary to Decimal
(convert 1000101 2 10)  ;→ 69
(convert 1111011 2 10)  ;→ 123

; Binary to Binary
(convert 1000101 2 2)  ;→ 1000101

; Octal to Octal
(convert 105 8 8)  ;→ 105

; Decimal to Decimal
(convert 69 10 10)  ;→ 69
