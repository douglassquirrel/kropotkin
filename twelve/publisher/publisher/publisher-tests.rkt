#lang racket
(require rackunit rackunit/text-ui net/url "publisher.rkt")
(provide execute-tests)

(cond ((not (getenv "TEST_PORT")) (error "Environment variable TEST_PORT not set")))
(define TEST_PORT (string->number (getenv "TEST_PORT")))

(define (getting-http-response-from port)
  (let* ((url (string->url (format "http://localhost:~a" port)))
	 (read-byte (with-handlers ((exn:fail? (lambda (e) eof))) (call/input-url url get-pure-port read-byte))))
    (not (eof-object? read-byte))))

(define tests (test-suite "Publisher publisher tests"
			  (test-case "fails" (check-eq? 'a 'b))))

(define (execute-tests)
  (let ((test-server-thread (thread (lambda () (start-server TEST_PORT)))))
    (cond ((not (getting-http-response-from TEST_PORT)) (error "Failed to start server")))

    (let ((number-of-failures (run-tests tests 'verbose)))
      (kill-thread test-server-thread)
      (= 0 number-of-failures))))

