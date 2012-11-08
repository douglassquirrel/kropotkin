#!/usr/bin/racket
#lang racket
(require net/url "engine.rkt" "resource-server.rkt")
(provide run-resource)

(define (run-resource target-file execute-tests port)
  (define URL (string->url (format "http://localhost:~a" port)))

  (define deploy (make-thread-side-effect (lambda () (deploy-resource-server target-file port))))

  (define file-bytes (call-with-input-file target-file port->bytes))
  (define (check-server) 
    (define http-bytes (call/input-url URL get-pure-port port->bytes))
    (cond ((equal? file-bytes http-bytes) (displayln "Serving file correctly")     #t)
	  (else                           (displayln "Not serving file correctly") #f)))

  (define (state-machine state)
    (cond ((eq? state 'init)                                   (values 'unit            empty))
	  ((eq? state 'unit)            (cond ((execute-tests) (values 'deploy          empty))
					      (else            (values 'exit            empty))))
	  ((eq? state 'deploy)                                 (values 'wait-for-deploy deploy))
	  ((eq? state 'wait-for-deploy) (sleep 5)              (values 'monitor         empty))
	  ((eq? state 'monitor)         (cond ((check-server)  (values 'monitor         (make-wait-side-effect 30)))
					      (else            (values 'exit            empty))))))

  (define engine (make-engine state-machine))
  (start engine))