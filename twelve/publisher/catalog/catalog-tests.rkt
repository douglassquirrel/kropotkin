#lang racket
(require rackunit rackunit/text-ui "catalog.rkt")
(provide execute-catalog-tests)

(define tests (test-suite "Publisher catalog tests"
			  (test-case "add" (check-not-exn (lambda () (add-to-catalog #:name "stations"
										     #:creation-datetime "2012-01-01 00:00:00"
										     #:contents #"Ashford, Westenghanger, Sandling"))))
			  (test-case "add and retrieve one item" 
				     (add-to-catalog #:name "colours"
						     #:creation-datetime "2012-02-02 00:00:00"
						     #:contents #"red, black")
				     (check-equal? (get-latest-with-name "colours") #"red, black"))
			  (test-case "add and retrieve two items" 
				     (add-to-catalog #:name "colours"
						     #:creation-datetime "2012-02-03 00:00:11"
						     #:contents #"red, black")
				     (add-to-catalog #:name "dogs"
						     #:creation-datetime "2012-02-03 00:02:22"
						     #:contents #"Labrador, poodle")
				     (check-equal? (get-latest-with-name "colours") #"red, black")
				     (check-equal? (get-latest-with-name "dogs")    #"Labrador, poodle"))))


(define (execute-catalog-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))