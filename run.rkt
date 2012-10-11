#!/usr/bin/racket
#lang racket
(require file/md5 net/url)
(require "lib/engine.rkt" "lib/resource-server.rkt" 
	 "test/engine-tests.rkt" "test/complications-tests.rkt" "test/thread-monitor-tests.rkt")

(define TARGET (getenv "TARGET"))
(cond ((not TARGET) (error "Need to specify target using the TARGET environment variable")))
(printf "Running with target ~a\n" TARGET)
(define target-file (format "lib/~a.rkt" TARGET))
(define execute-tests (cond ((equal? TARGET "engine")         execute-engine-tests)
			    ((equal? TARGET "complications")  execute-complications-tests)
			    ((equal? TARGET "thread-monitor") execute-thread-monitor-tests)
			    (else                            (error "Unrecognised target" TARGET))))

(define deploy (make-thread-side-effect (lambda () (deploy-resource-server target-file 8080))))

(define file-bytes (call-with-input-file target-file port->bytes))
(define (check-server) 
  (define http-bytes (call/input-url (string->url "http://localhost:8080") get-pure-port port->bytes))
  (cond ((equal? file-bytes http-bytes) (displayln "Serving file correctly")     #t)
	(else                           (displayln "Not serving file correctly") #f)))

(define (state-machine state)
  (cond ((eq? state 'init)                                   (values 'unit            empty))
	((eq? state 'unit)            (cond ((execute-tests) (values 'deploy          empty))
				      (else                  (values 'exit            empty))))
	((eq? state 'deploy)                                 (values 'wait-for-deploy deploy))
	((eq? state 'wait-for-deploy) (sleep 5)              (values 'monitor         empty))
	((eq? state 'monitor)         (cond ((check-server)  (values 'monitor         (make-wait-side-effect 5)))
				      (else                  (values 'exit            empty))))))

(define engine (make-engine state-machine))
(start engine)