#lang racket
(require rackunit rackunit/text-ui net/url)
(require "tycoon.rkt" "http-checks.rkt")
(provide execute-tests)

(cond ((not (getenv "TEST_PORT")) (error "Environment variable TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "TEST_PORT")))
(define ROOT_URL              (string->url (format "http://localhost:~a"                  TEST_PORT)))
(define NONEXISTENT_ENTRY_URL (string->url (format "http://localhost:~a/doesnotexist.tar" TEST_PORT)))

(define tests (test-suite "Publisher tycoon tests"
			  (test-case "Server should be responsive"
				     (check-http-server-responding? ROOT_URL))
			  (test-case "Should get 404 response for nonexistent entry" 
				     (check-response-code NONEXISTENT_ENTRY_URL 404))))

(define (execute-tests)
  (let ((test-server-thread (thread (lambda () (start-server TEST_PORT))))
	(number-of-failures (run-tests tests 'verbose)))
      (kill-thread test-server-thread)
      (= 0 number-of-failures)))
