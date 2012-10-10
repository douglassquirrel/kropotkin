#!/usr/bin/racket
#lang racket
(require file/md5 net/url)
(require "lib/engine.rkt" "lib/resource-server.rkt")
(require "test/complications-tests.rkt")

(define deploy (make-thread-side-effect (lambda () (deploy-resource-server "lib/complications.rkt" 8080))))

(define file-bytes (call-with-input-file "lib/complications.rkt" port->bytes))
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

(displayln "Starting engine")
(start engine)
(displayln "Engine finished")


