#lang racket

; (max-ordered-sublist ‘(1 5 2 4 6 8 3 4 1)) → ‘(2 4 6 8)

(define (max-ordered-sublist lst)
  (define (subsequent-length list length max-length)
    (if (or (empty? list) (= length max-length))
      length
      (if (< (first list) (first (rest list)))
        (subsequent-length (rest list) (+ length 1) max-length)
        length
      )
    )
  )

  (define (max-ordered-sublist-iter list i length max-length max-list)
    (if (< i length)
      (max-ordered-sublist-iter (rest list) (+ i 1) length
        (if (< max-length (subsequent-length list 0 (length list))) (subsequent-length list 0 (length list)) max-length)
        (if (> (subsequent-length list 0 (length list)) (length max-list)) list max-list)
      )
      max-list
    )
  )

  (max-ordered-sublist-iter lst 0 (length lst) 0 '())
)

(max-ordered-sublist '(1 5 2 4 6 8 3 4 1))
