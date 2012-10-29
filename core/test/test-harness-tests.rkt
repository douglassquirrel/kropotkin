#lang racket
(require rackunit rackunit/text-ui "../lib/test-harness.rkt")
(provide execute-test-harness-tests)

(define tests (test-suite "test-harness unit tests"
			  (test-case "no tests"
				     (check-true (execute-tests empty)))))

(define (execute-test-harness-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))