#lang racket

(define binary-numbers
  (call-with-input-file "binary.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (map string (string->list line))))))

(define column-length
  (length (first binary-numbers)))

(define rows-length
  (length binary-numbers))

(define mid-point
  (/ rows-length 2))

;;; the numbers in a given column, sorted ex. (000111)
(define (sorted-column index)
  (sort
   (map (lambda (line)
         (string->number (list-ref line index)))
       binary-numbers)
   <))

;;; columns, sorted
(define columns
  (map sorted-column (range column-length)))

;;; is 0 or 1 most common in the column?
(define (most-common-bit column)
  (cond
    [(> (index-of column 1) mid-point) 1]
    [else 0]))

;;; opposite of most common
(define (least-common-bit column)
  (cond
    [(> (index-of column 1) mid-point) 0]
    [else 1]))

;;; combine the most common bits to make a number, ex. 10011
(define (combined-bit proc)
  (string->number
    (string-join
      (map ~a (map number->string (map proc columns))) "")))

(define (bin->dec n)
  (if (zero? n)
      n
      (+ (modulo n 10) (* 2 (bin->dec (quotient n 10))))))

(define gamma-rate
  (bin->dec (combined-bit most-common-bit)))

(define epsilon-rate
  (bin->dec (combined-bit least-common-bit)))

(define power-consumption (* gamma-rate epsilon-rate))

power-consumption
