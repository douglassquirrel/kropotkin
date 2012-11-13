#lang racket
(require rackunit rackunit/text-ui "catalog.rkt")
(provide execute-tests)

(define tests (test-suite "Publisher catalog tests"
			  (test-case "add" (check-not-exn (lambda () (add-to-catalog #:name "stations"
										     #:creation-datetime "2012-01-01 00:00:00"
										     #:contents #"Ashford, Westenghanger, Sandling"))))
			  (test-case "add and retrieve" 
				     (add-to-catalog #:name "colours"
						     #:creation-datetime "2012-02-02 00:00:00"
						     #:contents #"red, black")
				     (check-equal? #"red, black" (get-latest-with-name "colours")))))


(define (execute-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))