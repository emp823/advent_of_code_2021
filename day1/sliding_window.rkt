#lang racket
(define (countSlidingIncreases values)
  (if (>= 1 (length values))
      0
      (if (< ((+ (take values 3)) (+ (take (rest values) 3)))
          (+ 1 (countSlidingIncreases (rest values)))
          (countSlidingIncreases (rest values)))))

(define numbers
  (call-with-input-file "numbers.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

(countSlidingIncreases numbers)
