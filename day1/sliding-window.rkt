#lang racket
(define (countSlidingIncreases values)
  (if (>= 3 (length values))
     0
     (if (< (sumWindow values) (sumWindow (rest values)))
        (+ 1 (countSlidingIncreases (rest values)))
        (countSlidingIncreases (rest values)))))

(define numbers
  (call-with-input-file "numbers.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

(define (sumWindow list)
  (apply + (take list 3)))

(countSlidingIncreases numbers)
