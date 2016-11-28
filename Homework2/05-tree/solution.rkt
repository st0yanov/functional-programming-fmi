#lang racket

(define (empty-tree? tree)
  (empty? tree)
)

(define (get-node tree)
  (first tree)
)

(define (get-left tree)
  (second tree)
)

(define (get-right tree)
  (third tree)
)

(define (make-edge n m)
  (cons n m)
)

(define (edges-on-level tree level)
  (define (edges-on-level-iter tree level i)
    (cond
      [(empty-tree? tree) '()]
      [(and (empty-tree? (get-left tree)) (empty-tree? (get-right tree)))]
      [(and (= level i) (empty-tree? (get-left tree))) (list (make-edge (get-node tree) (get-node (get-right tree))))]
      [(and (= level i) (empty-tree? (get-right tree))) (list (make-edge (get-node tree) (get-node (get-left tree))))]
      [(= level i) (list (make-edge (get-node tree) (get-node (get-left tree))) (make-edge (get-node tree) (get-node (get-right tree))))]
      [else (list (edges-on-level-iter (get-left tree) level (add1 i)) (edges-on-level-iter (get-right tree) level (add1 i)))]
    )
  )
  (edges-on-level-iter tree level 1)
)

(edges-on-level '(1 (2 (4 () ()) (5 () (7 () ()))) (3 (6 (8 () ()) (9 () ())) ())) 2)
