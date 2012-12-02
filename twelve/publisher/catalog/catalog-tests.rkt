#lang racket
(require rackunit rackunit/text-ui "catalog.rkt")
(provide execute-catalog-tests)

(define CATALOG_DIR (make-temporary-file "twelve-tmp-~a" 'directory))
(define CATALOG_FILE (path->string (build-path CATALOG_DIR "test-catalog.s3")))
(define cat (make-catalog CATALOG_FILE))

(random-seed 314159265)
(define (add-test-data-to-catalog name [creation-datetime "2012-01-01 00:00:00"])
  (let ((random-data (string->bytes/utf-8 (format "~a" (random 1000000)))))
    (add-to-catalog #:catalog cat #:name name #:creation-datetime creation-datetime #:contents random-data)
    random-data))

(define tests (test-suite "Publisher catalog tests"
			  (test-case "add" (check-not-exn (lambda () (add-test-data-to-catalog "test-add"))))
			  (test-case "add and retrieve one item" 
				     (let ((data (add-test-data-to-catalog "test-one-add")))
				       (check-equal? (get-latest-with-name cat "test-one-add") data)))
			  (test-case "retrieve nonexistent item"
				     (check-false (get-latest-with-name cat "does not exist")))
			  (test-case "add and retrieve two items" 
				     (let ((data-1 (add-test-data-to-catalog "test-two-add-1"))
				           (data-2 (add-test-data-to-catalog "test-two-add-2")))
				       (check-equal? (get-latest-with-name cat "test-two-add-1") data-1)
				       (check-equal? (get-latest-with-name cat "test-two-add-2") data-2)))
			  (test-case "add multiple items with same name in any order, always retrieve latest"
				     (add-test-data-to-catalog "test-multiple-same" "2012-01-01 01:01:01")
				     (let ((most-recent-data (add-test-data-to-catalog "test-multiple-same" "2012-02-02 02:02:02")))
				       (check-equal? (get-latest-with-name cat "test-multiple-same") most-recent-data)
				       (add-test-data-to-catalog "test-multiple-same" "2012-01-03 03:03:03")
				       (check-equal? (get-latest-with-name cat "test-multiple-same") most-recent-data)))
			  (test-case "add and retrieve from in-memory catalog"
				     (let ((memory-cat (make-memory-catalog)))
				       (add-to-catalog #:catalog memory-cat #:name "test-memory-add" #:creation-datetime "2012-03-03 03:03:03"
						       #:contents #"memory")
				       (check-equal? (get-latest-with-name memory-cat "test-memory-add") #"memory")))))

(define (execute-catalog-tests)
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))