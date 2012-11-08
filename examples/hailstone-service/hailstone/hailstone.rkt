#lang racket
(provide hailstone)

(define (even? n) (= 0 (modulo n 2)))
(define (odd? n) (not (even? n)))

(define (f n) (cond ((even? n) (/ n 2))
((odd? n) (+ 1 (* 3 n)))))

(define (hailstone n) (cond ((= 1 n) (list 1))
(else (cons n (hailstone (f n))))))

