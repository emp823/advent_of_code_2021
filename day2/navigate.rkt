#lang racket

(define instructions
  (call-with-input-file "moves.txt"
    (lambda (moves)
      (for/list ([row (in-lines moves)])
        (define match (regexp-match #rx"([^ ]+) (.+)" row))
        (define direction (string->symbol (cadr match)))
        (define steps (string->number (caddr match)))
        `(,direction ,steps)))))

(define (sumValues lst)
  (apply + (map cadr lst)))

(define (filterDir instr dir)
  (filter (lambda (pair) (eq? (car pair) dir)) instr))

(define fwd (sumValues (filterDir instructions 'forward)))
(define down (sumValues (filterDir instructions 'down)))
(define up (sumValues (filterDir instructions 'up)))
  
(define horiz fwd)
(define depth (- down up))

(* horiz depth)
