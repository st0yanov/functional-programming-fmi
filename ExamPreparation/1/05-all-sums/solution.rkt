#lang racket

(define (all-sums lst)
  (define (all-sums-raw wlist)
    (cond
      [(empty? wlist) '()]
      [(= (length wlist) 1) (list 0 (first wlist))]
      [else
        (
          let ([all-sums-rest (all-sums-raw (rest wlist))])
          (append all-sums-rest (map (lambda (x) (+ x (first wlist))) all-sums-rest))
        )
      ]
    )
  )

  (all-sums-raw lst)
)

(all-sums '(1 2 3))
