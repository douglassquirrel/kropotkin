#lang racket
(require rackunit rackunit/text-ui net/url "tycoon.rkt")
(provide execute-tests)

(cond ((not (getenv "TEST_PORT")) (error "Environment variable TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "TEST_PORT")))

(define RESPONSE_CODE_RE #px"HTTP/[^ ]* (\\d*) ")
(define NONEXISTENT_ENTRY_URL (string->url (format "http://localhost:~a/doesnotexist.tar" TEST_PORT)))

(define (headers url) (call/input-url url get-impure-port purify-port))
(define (response-code url) 
  (let ((result (regexp-match RESPONSE_CODE_RE (headers url))))
    (cond ((false? result) (error "No response code for request to " url))
	  (else            (string->number (second result))))))

(define (test-http-response #:from port #:within-milliseconds [within-milliseconds 0])
  (let* ((url (string->url (format "http://localhost:~a" port)))
	 (read-byte (with-handlers ((exn:fail? (lambda (e) eof))) (call/input-url url get-pure-port read-byte)))
	 (listening (not (eof-object? read-byte))))
    (cond ((or listening (<= within-milliseconds 0)) listening)
	  (else                                      (sleep 0.1)
				                     (test-http-response #:from port #:within-milliseconds (- within-milliseconds 100))))))

(define tests (test-suite "Publisher tycoon tests"
			  (test-case "404 response for nonexistent entry" 
				     (check-eq? (response-code NONEXISTENT_ENTRY_URL) 404))))

(define (execute-tests)
  (let ((test-server-thread (thread (lambda () (start-server TEST_PORT)))))
    (cond ((not (test-http-response #:from TEST_PORT #:within-milliseconds 2000)) (error "Failed to start server")))

    (let ((number-of-failures (run-tests tests 'verbose)))
      (kill-thread test-server-thread)
      (= 0 number-of-failures))))
