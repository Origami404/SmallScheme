#lang r5rs
(define (f x y . z)
  (display x)
  (newline)
  (display y)
  (newline)
  (display z)
  (newline))

(f 2 3 4 5)

(define (g x y . z)
  (display x)
  (newline)
  (display y)
  (newline)
  (display z)
  (newline))

(g 2 3 4 5)