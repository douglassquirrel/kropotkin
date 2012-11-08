#lang racket
(require net/url rackunit rackunit/text-ui "../lib/run-resource.rkt")
(provide execute-run-resource-tests)

(define ran-tests #f)
(define (execute-tests) (set! ran-tests #t))
(define PORT (string->number (getenv "TEST_PORT")))
(cond ((not PORT) (error "Must supply TEST_PORT environment variable")))
(define test-content #"run-resources test file")
(define (create-temporary-file content) 
  (define temporary-file (make-temporary-file))
  (call-with-output-file* temporary-file #:exists 'replace (lambda (out) (display content out)))  
  (path->string temporary-file))

(define tests 
  (test-suite "run-resource tests" 
	      (test-case "serves file" 
			 (define test-file (create-temporary-file test-content))
			 (define URL (string->url (format "http://localhost:~a" PORT)))
			 (define server-thread (thread (lambda () (run-resource test-file execute-tests PORT))))
			 (sleep 2)
			 (let ((downloaded-bytes (call/input-url URL get-pure-port port->bytes)))
			   (kill-thread server-thread)
			   (check-equal? test-content downloaded-bytes)))))
  			 
(define (execute-run-resource-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
