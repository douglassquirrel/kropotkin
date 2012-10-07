#!/usr/bin/racket
#lang racket
(require "lib/engine.rkt" "src/unit-tests.rkt" "lib/resource-server.rkt")

(define deploy (make-thread-side-effect (lambda () (deploy-resource-server "hailstone.rkt"))))
(define (check-server) (displayln "Checking server") #t)  

(define (state-machine state)
  (cond ((eq? state 'init)                            (values 'unit    empty))
	((eq? state 'unit)    (cond ((run-unit-tests) (values 'deploy  empty))
				    (else             (values 'exit    empty))))
	((eq? state 'deploy)                          (values 'monitor deploy))
	((eq? state 'monitor) (cond ((check-server)   (values 'monitor (make-wait-side-effect 5)))
				    (else             (values 'exit    empty))))))

(define engine (make-engine state-machine))

(displayln "Starting engine")
(start engine)
(displayln "Engine finished")


