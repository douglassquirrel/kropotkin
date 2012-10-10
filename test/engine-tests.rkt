#lang racket
(require rackunit rackunit/text-ui "../lib/engine.rkt")
(provide execute-engine-tests)

(define (make-step-side-effect step-name)
  (make-thread-side-effect (lambda () (printf "Test: entering step ~a\n" step-name))))

(define tests 
  (test-suite "Engine tests" 
	      (test-case "states" 
			 (define (state-machine state)
			   (cond ((eq? state 'init)   (values 'step-1 (make-step-side-effect "init")))
				 ((eq? state 'step-1) (values 'step-2 (make-step-side-effect "step-1")))
				 ((eq? state 'step-2) (values 'exit   (make-step-side-effect "step-2")))))
			 (define engine (make-engine state-machine))
			 (start engine)
			 (check-equal? 1 1))))
  			 
(define (execute-engine-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
