#lang racket
(require rackunit rackunit/text-ui net/url)
(require "http-server-tools.rkt" "http-checks.rkt")
(provide execute-http-server-tools-tests)

(cond ((not (getenv "HTTP_SERVER_TOOLS_TEST_PORT")) (error "Environment variable HTTP_SERVER_TOOLS_TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "HTTP_SERVER_TOOLS_TEST_PORT")))
(define root-url (string->url (format "http://localhost:~a" TEST_PORT)))

(define tests (test-suite "HTTP server tools tests"
			  (test-case "Server responds" (check-http-server-responding? root-url))))

(define (execute-http-server-tools-tests)
  (let* ((test-server-thread (thread (lambda () (serve-bytes-as-file #"test-contents" "test.txt" TEST_PORT))))
	 (number-of-failures (run-tests tests 'verbose)))
    (kill-thread test-server-thread)
    (= 0 number-of-failures)))
