#lang racket
(require rackunit rackunit/text-ui net/url)
(require "clerk.rkt")
(provide execute-clerk-tests)

(cond ((not (getenv "TEST_CLERK_PORT")) (error "Environment variable TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "TEST_CLERK_PORT")))
(define ROOT_URL (string->url (format "http://localhost:~a" TEST_PORT)))

(define (http-response? url [within-milliseconds 0])
  (let* ((read-byte (with-handlers ((exn:fail? (lambda (e) eof))) (call/input-url url get-pure-port read-byte)))
	 (listening (not (eof-object? read-byte))))
    (cond ((or listening (<= within-milliseconds 0)) listening)
	  (else                                      (sleep 0.1)
				                     (http-response? url (- within-milliseconds 100))))))

(define-simple-check (check-http-server-responding? url) (http-response? url 2000))

(define tests (test-suite "Courier clerk tests"
			  (test-case "Clerk should be responsive" (check-http-server-responding? ROOT_URL))))

(define (execute-clerk-tests)
  (let ((test-clerk-thread (thread (lambda () (start-clerk TEST_PORT))))
	(number-of-failures (run-tests tests 'verbose)))
    (kill-thread test-clerk-thread)
    (= 0 number-of-failures)))
