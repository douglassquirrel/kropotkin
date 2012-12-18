#lang racket
(require rackunit net/url net/head)
(provide check-http-server-responding? check-response-code check-content-type check-content)

(define RESPONSE_CODE_RE #px"HTTP/[^ ]* (\\d*) ")

(define (headers url) (call/input-url url get-impure-port purify-port))
(define (content url) (call/input-url url get-pure-port port->bytes))

(define (http-response? url [within-milliseconds 0])
  (let* ((read-byte (with-handlers ((exn:fail? (lambda (e) eof))) (call/input-url url get-pure-port read-byte)))
	 (listening (not (eof-object? read-byte))))
    (cond ((or listening (<= within-milliseconds 0)) listening)
	  (else                                      (sleep 0.1)
				                     (http-response? url (- within-milliseconds 100))))))
(define (response-code url) 
  (let ((result (regexp-match RESPONSE_CODE_RE (headers url))))
    (cond (result (string->number (second result)))
	  (else   0))))
(define (content-type url)
  (let ((result (extract-field "Content-Type" (headers url))))
    (cond (result result)
	  (else   ""))))

(define-simple-check (check-http-server-responding? url) (http-response? url 2000))
(define-simple-check (check-response-code url expected) (=      (response-code url) expected))
(define-simple-check (check-content-type  url expected) (equal? (content-type url)  expected))
(define-simple-check (check-content       url expected) (equal? (content url)       expected))
