#lang racket
(define-syntax for-each
  (syntax-rules (tmp)
               ((tmp proc l)
                (map proc l))))

(display (for-each (lambda (x) (* x x)) '(1 2 3 4)))
(newline)
(display (for-each l p))
