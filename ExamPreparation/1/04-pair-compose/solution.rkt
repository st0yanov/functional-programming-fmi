#lang racket

(define (pair-compose fs)
  (lambda (x)
    (cond
      [(= (length fs) 0) 0]
      [(= (length fs) 1) ((first fs) x)]
      [else
        (+ ((first fs) ((second fs) x)) ((pair-compose (rest (rest fs))) x))
      ]
    )
  )
)

(define (add1 x) (+ x 1))
(define (add2 x) (+ x 2))
(define (mult2 x) (* x 2))

((pair-compose (list add1 add2)) 1) ;; -> 4
((pair-compose (list add1 mult2 mult2 add2)) 1) ;;(+ (add1 (mult2 1)) (mult2 (add2 1))) -> (+ 3 6) -> 9
((pair-compose (list add1 mult2 mult2 add2 add2)) 1) ;; -> 12
