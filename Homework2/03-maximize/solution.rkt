#lang racket

(define (maximize list)
  (define (maximize-iter list x highest)
    (if (> (length list) 0)
      (maximize-iter (rest list) x (if (> (abs ((first list) x)) highest) (abs ((first list) x)) highest))
      highest
    )
  )

  (lambda (x) (maximize-iter list x (abs ((first list) x))))
)

((maximize (list (lambda (x) (- x 10)) (lambda (x) (- x 5)))) 5) ;→ 5
((maximize (list (lambda (x) (- x 10)) (lambda (x) (- x 5)))) 9) ;→ 4
