#lang racket
(require rackunit rackunit/text-ui "catalog.rkt")
(provide execute-tests)

(define tests (test-suite "Publisher catalog tests"
			  (test-case "add data" (check-not-exn (lambda () (add-to-catalog #:name "stations"
											  #:creation-datetime "2012-01-01 00:00:00"
											  #:data #"Ashford, Westenghanger, Sandling"))))))


(define (execute-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))