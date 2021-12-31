#lang racket
(define (countIncreases values)
  (if (>= 1 (length values))
     0
     (if (< (first values) (first (rest values)))
        (+ 1 (countIncreases (rest values)))
        (countIncreases (rest values)))))

(define numbers
  (call-with-input-file "numbers.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

(countIncreases numbers)
