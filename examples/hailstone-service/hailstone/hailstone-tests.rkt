#lang racket
(require rackunit rackunit/text-ui "hailstone.rkt")
(provide execute-tests)

(define tests (test-suite "Hailstone unit tests"
			  (test-case "values"
				     (check-equal? '(1) (hailstone 1))
				     (check-equal? '(2 1) (hailstone 2))
				     (check-equal? '(4 2 1) (hailstone 4))
				     (check-equal? '(5 16 8 4 2 1) (hailstone 5))
				     (check-equal? '(9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1) (hailstone 9)))))

(define (execute-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))