#lang racket
(require rackunit net/url)
(provide check-response-code check-http-server-responding?)

(define RESPONSE_CODE_RE #px"HTTP/[^ ]* (\\d*) ")

(define (headers url) (call/input-url url get-impure-port purify-port))
(define (response-code url) 
  (let ((result (regexp-match RESPONSE_CODE_RE (headers url))))
    (cond ((false? result) (error "No response code for request to " url))
	  (else            (string->number (second result))))))
(define-simple-check (check-response-code url expected) (= (response-code url) expected))

(define (http-response? url [within-milliseconds 0])
  (let* ((read-byte (with-handlers ((exn:fail? (lambda (e) eof))) (call/input-url url get-pure-port read-byte)))
	 (listening (not (eof-object? read-byte))))
    (cond ((or listening (<= within-milliseconds 0)) listening)
	  (else                                      (sleep 0.1)
				                     (http-response? url (- within-milliseconds 100))))))
(define-simple-check (check-http-server-responding? url) (http-response? url 2000))