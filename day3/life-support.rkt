#lang racket

(define binary-strings
  (call-with-input-file "binary.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (map string (string->list line))))))

(define binary-numbers
  (map (lambda (row) (map string->number row)) binary-strings))

(define column-length
  (length (first binary-strings)))

(define rows-length
  (length binary-strings))

(define mid-point
  (/ rows-length 2))

;;; the numbers in a given column, sorted ex. (000111)
(define (sorted-column index)
  (sort
   (map (lambda (line)
          (string->number (list-ref line index)))
       binary-strings)
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

(define most-common-bits
  (map most-common-bit columns))

(define least-common-bits
  (map least-common-bit columns))

(define (filter-rows rows [acc 0])
  (if (= 1 (length rows))
     (first rows)
     (filter-rows (filter (lambda (row)
                            (equal? (list-ref row acc) (list-ref most-common-bits acc)))
                         rows)
                 (+ acc 1))))

;;; combine the most common bits to make a number, ex. 10011
(define (combined-bit bits)
  (string->number
   (string-join
    (map ~a (map number->string bits)) "")))

(define oxygen-row
  (filter-rows binary-numbers))

(define oxygen-bit
  (combined-bit oxygen-row))

(define (bin->dec n)
  (if (zero? n)
     n
     (+ (modulo n 10) (* 2 (bin->dec (quotient n 10))))))

(define oxygen-rating
  (bin->dec oxygen-bit))
