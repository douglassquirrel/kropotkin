#lang racket
(require rackunit rackunit/text-ui "../lib/engine.rkt")
(provide execute-engine-tests)

(define log "")
(define (make-publish-side-effect message)
  (lambda () (set! log (string-append log "\n" message))))

(define tests 
  (test-suite "Engine tests" 
	      (test-case "states" 
			 (define (state-machine state)
			   (cond ((eq? state 'init)   (values 'step-1 (make-publish-side-effect "init")))
				 ((eq? state 'step-1) (values 'step-2 (make-publish-side-effect "step-1")))
				 ((eq? state 'step-2) (values 'exit   (make-publish-side-effect "step-2")))))
			 (define engine (make-engine state-machine))
			 (start engine)
			 (check-equal? "\ninit\nstep-1\nstep-2" log))))
  			 
(define (execute-engine-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
