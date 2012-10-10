#lang racket
(require rackunit rackunit/text-ui "../lib/complications.rkt")
(provide execute-tests)

(define (milliseconds-to-run proc args)
  (let-values 
   (((ignored1 ignored2 milliseconds-elapsed ignored3) (time-apply proc args))) 
   milliseconds-elapsed))

(define tests 
  (test-suite "Complications tests" 
	      (test-case "thread" 
			 (let* ((thread-descriptor empty)
				(complication (make-thread-side-effect (lambda () (set! thread-descriptor (current-thread))))))
			   (apply-complications (list complication))
			   (check-not-equal? (current-thread) thread-descriptor)))
	      (test-case "wait"
			 (define complication (make-wait-side-effect 5))
			 (define milliseconds-to-run-complication (milliseconds-to-run apply-complications (list complication)))
			 (check-= 5000 milliseconds-to-run-complication 200 "Should wait approximately 5 seconds"))))
  			 
(define (execute-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
