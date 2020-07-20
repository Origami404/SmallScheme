#lang r5rs

(display (cddr '(2 3 . 4)))
(newline)
(display (cddr '(2 3 4)))
(newline)
(display (cons 4 '()))
(newline)
(display (cdr '(3 . ...)))

(newline)
(newline)

(display (list 2 3 . 5))
(newline)
(display (list 2 3 4))
(newline)
(display '(cons 2 3))
(newline)