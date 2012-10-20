#lang racket
(require rackunit rackunit/text-ui "../lib/fetch.rkt")
(provide execute-fetch-tests)

(define tests 
  (test-suite "Fetch tests" 
	      (test-case "faked" 
			 (check-equal? 1 1))))
  			 
(define (execute-fetch-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
