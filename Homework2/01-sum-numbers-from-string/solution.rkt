#lang racket

(define (sum-numbers str)
  (define (calculate-number lst total i)
    (if (> (length lst) 0)
      (calculate-number (rest lst) (+ total (* (first lst) i)) (* i 10))
      total
    )
  )

  (define (sum-numbers-iter str pos numbers total)
    (if (> pos 0)
      (if (string->number (substring str (- pos 1) pos))
        (sum-numbers-iter str (- pos 1) (append numbers (list (string->number (substring str (- pos 1) pos)))) total)
        (sum-numbers-iter str (- pos 1) '() (+ total (calculate-number numbers 0 1)))
      )
      total
    )
  )

  (sum-numbers-iter str (string-length str) '() 0)
)

(sum-numbers "a123b2c56") ;→ 181
(sum-numbers "a1b2c3") ;→ 6
