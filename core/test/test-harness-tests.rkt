#lang racket
(require rackunit rackunit/text-ui "../lib/test-harness.rkt")
(provide execute-test-harness-tests)

(define tests (test-suite "test-harness unit tests"
			  (test-case "no tests"
				     (check-true (execute-tests empty) "Should pass if no tests"))
			  (test-case "one passing test"
				     (check-true (execute-tests (test-suite "one passing test" 
									    (test-case "passes" (check-true #t))))))
			  (test-case "multiple passing tests"
				     (check-true (execute-tests (test-suite "multiple passing tests" 
									    (test-case "passes"            (check-true #t))
									    (test-case "passes again"      (check-false #f))
									    (test-case "yet again, passes" (check-equal? 1 1))))))
			  (test-case "one failing test"
				     (check-false (execute-tests (test-suite "one failing test" 
									    (test-case "fails" (check-true #f))))))
			  (test-case "mixed - some pass, some fail"
				     (check-false (execute-tests (test-suite "mixed" 
						  			    (test-case "passes"            (check-true #t))
									    (test-case "fails"             (check-false #t))
									    (test-case "passes again"      (check-equal? 1 1))))))))

(define (execute-test-harness-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))