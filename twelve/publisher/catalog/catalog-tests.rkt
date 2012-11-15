#lang racket
(require rackunit rackunit/text-ui "catalog.rkt")
(provide execute-catalog-tests)

(random-seed 314159265)
(define (add-test-data-to-catalog name [creation-datetime "2012-01-01 00:00:00"])
  (let ((random-data (format "~a" (random 1000000))))
    (add-to-catalog #:name name #:creation-datetime creation-datetime #:contents random-data)
    random-data))

(define tests (test-suite "Publisher catalog tests"
			  (test-case "add" (check-not-exn (lambda () (add-test-data-to-catalog "test-add"))))
			  (test-case "add and retrieve one item" 
				     (let ((data (add-test-data-to-catalog "test-one-add")))
				       (check-equal? (get-latest-with-name "test-one-add") data)))
			  (test-case "add and retrieve two items" 
				     (let ((data-1 (add-test-data-to-catalog "test-two-add-1"))
				           (data-2 (add-test-data-to-catalog "test-two-add-2")))
				       (check-equal? (get-latest-with-name "test-two-add-1") data-1)
				       (check-equal? (get-latest-with-name "test-two-add-2") data-2)))))

(define (execute-catalog-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))