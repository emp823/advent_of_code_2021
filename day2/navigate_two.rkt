#lang racket

(define instructions
  (call-with-input-file "moves.txt"
    (lambda (moves)
      (for/list ([row (in-lines moves)])
        (define match (regexp-match #rx"([^ ]+) (.+)" row))
        (define direction (string->symbol (cadr match)))
        (define steps (string->number (caddr match)))
        `(,direction ,steps)))))

(define (sum-values lst)
  (apply + (map cadr lst)))

(struct sub-position (aim depth horiz))

(define (match-move command position)
  (match command
    [(list 'forward n) (move-forward n position)]
    [(list 'up n) (move-up n position)]
    [(list 'down n) (move-down n position)]))

(define (move-down n position)
  (struct-copy sub-position position
               [aim (+ (sub-position-aim position) n)]))

(define (move-up n position)
  (struct-copy sub-position position
               [aim (- (sub-position-aim position) n)]))

(define (move-forward n position)
  (struct-copy sub-position position
               [horiz (+ (sub-position-horiz position) n)]
               [depth (* (sub-position-aim position) n)]))

(define (navigate)
  (for/fold ([p (sub-position 0 0 0)]
            #:result (* (sub-position-horiz p)
                        (sub-position-depth p)))
            ([move (in-list instructions)])
    (match-move move p)))

(navigate)
