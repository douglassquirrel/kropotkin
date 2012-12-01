#lang racket
(require rackunit rackunit/text-ui net/url "tycoon.rkt")
(provide execute-tests)

(cond ((not (getenv "TEST_PORT")) (error "Environment variable TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "TEST_PORT")))
(define ROOT_URL              (string->url (format "http://localhost:~a"                  TEST_PORT)))
(define NONEXISTENT_ENTRY_URL (string->url (format "http://localhost:~a/doesnotexist.tar" TEST_PORT)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define RESPONSE_CODE_RE #px"HTTP/[^ ]* (\\d*) ")

(define (headers url) (call/input-url url get-impure-port purify-port))
(define (response-code url) 
  (let ((result (regexp-match RESPONSE_CODE_RE (headers url))))
    (cond ((false? result) (error "No response code for request to " url))
	  (else            (string->number (second result))))))

(define (http-response? url [within-milliseconds 0])
  (let* ((read-byte (with-handlers ((exn:fail? (lambda (e) eof))) (call/input-url url get-pure-port read-byte)))
	 (listening (not (eof-object? read-byte))))
    (cond ((or listening (<= within-milliseconds 0)) listening)
	  (else                                      (sleep 0.1)
				                     (http-response? url (- within-milliseconds 100))))))
(define-simple-check (check-http-server-responding? url) (http-response? url 2000))
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tests (test-suite "Publisher tycoon tests"
			  (test-case "Server responding"
				     (check-http-server-responding? ROOT_URL))
			  (test-case "404 response for nonexistent entry" 
				     (check-eq? (response-code NONEXISTENT_ENTRY_URL) 404))))

(define (execute-tests)
  (let ((test-server-thread (thread (lambda () (start-server TEST_PORT))))
	(number-of-failures (run-tests tests 'verbose)))
      (kill-thread test-server-thread)
      (= 0 number-of-failures)))
