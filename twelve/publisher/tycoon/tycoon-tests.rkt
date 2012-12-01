#lang racket
(require rackunit rackunit/text-ui net/url)
(require "tycoon.rkt" "library/catalog.rkt" "http-checks.rkt")
(provide execute-tests)

(cond ((not (getenv "TEST_PORT")) (error "Environment variable TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "TEST_PORT")))
(define ROOT_URL              (string->url (format "http://localhost:~a"                    TEST_PORT)))
(define NONEXISTENT_ENTRY_URL (string->url (format "http://localhost:~a/doesnotexist.tar"   TEST_PORT)))
(define VALID_ENTRY_URL       (string->url (format "http://localhost:~a/test-component.tar" TEST_PORT)))

(define tests (test-suite "Publisher tycoon tests"
			  (test-case "Server should be responsive"                     (check-http-server-responding? ROOT_URL))
			  (test-case "Should get 404 response for nonexistent entry"   (check-response-code NONEXISTENT_ENTRY_URL 404))
			  (test-case "Should get 200 response for valid entry"         (check-response-code VALID_ENTRY_URL       200))
			  (test-case "Should get octet-stream content for valid entry" (check-content-type  VALID_ENTRY_URL       "application/octet-stream"))
			  (test-case "Should get correct content for valid entry"      (check-content       VALID_ENTRY_URL       #"test-component"))))

(define (execute-tests)
  (let ((cat (make-memory-catalog)))
    (add-to-catalog #:catalog cat #:name "test-component.tar" #:creation-datetime "2012-03-03 03:03:03" #:contents #"test-component")
    (let ((test-server-thread (thread (lambda () (start-server TEST_PORT cat))))
	  (number-of-failures (run-tests tests 'verbose)))
      (kill-thread test-server-thread)
      (= 0 number-of-failures))))
