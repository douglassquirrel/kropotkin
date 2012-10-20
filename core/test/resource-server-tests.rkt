#lang racket
(require net/url rackunit rackunit/text-ui "../lib/resource-server.rkt")
(provide execute-resource-server-tests)

(define PORT (string->number (getenv "TEST_PORT")))
(cond ((not PORT) (error "Must supply TEST_PORT environment variable")))
(define test-content #"run-resources test file")
(define (create-temporary-file content) 
  (define temporary-file (make-temporary-file))
  (call-with-output-file* temporary-file #:exists 'replace (lambda (out) (display content out)))  
  (path->string temporary-file))

(define tests 
  (test-suite "resource server tests" 
	      (test-case "serves file" 
			 (define test-file (create-temporary-file test-content))
			 (define URL (string->url (format "http://localhost:~a" PORT)))
			 (define server-thread (thread (lambda () (deploy-resource-server test-file PORT))))
			 (sleep 2)
			 (let ((downloaded-bytes (call/input-url URL get-pure-port port->bytes)))
			   (kill-thread server-thread)
			   (check-equal? test-content downloaded-bytes)))))
  			 
(define (execute-resource-server-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
