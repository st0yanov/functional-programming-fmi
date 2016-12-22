#lang racket

; (triangular? ‘((1 2 3) (0 5 6) (0 0 9))) #t
; 1   2   3
; 0   5   6
; 0   0   9

; (triangular? ‘((0 2 3) (0 0 6) (1 0 0))) #f
; 0   2   3
; 0   0   6
; 1   0   0

(define (triangular? mat)
  (define (row-zeros? row pos current-pos)
    (if (< current-pos pos)
      (if (= (list-ref row current-pos) 0)
        (row-zeros? row pos (+ current-pos 1))
        #f
      )
      #t
    )
  )

  (define (triangular?-iter matrix rows-count result)
    (if result
      (if (< rows-count 3)
        (triangular?-iter (rest matrix) (+ rows-count 1) (row-zeros? (first matrix) rows-count 0))
        result
      )
      #f
    )
  )

  (triangular?-iter mat 0 #t)
)

(triangular? '((1 2 3) (0 5 6) (0 0 9)))
(triangular? '((0 2 3) (0 0 6) (1 0 0)))
