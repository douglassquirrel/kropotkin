#lang racket
(require rackunit rackunit/text-ui "../lib/thread-monitor.rkt")
(provide execute-thread-monitor-tests)

(define ticker-finished-naturally #f)
(define (ticker number spacing monitor)
  (sleep spacing)
  (monitor 'tick)
  (cond ((> number 0) (ticker (- number 1) spacing monitor))
	(else         (set! ticker-finished-naturally #t))))

(define tests 
  (test-suite "Thread monitor tests" 
	      (test-case "Monitor without kill" 
			 (set! ticker-finished-naturally #f)
			 (let* ((monitor          empty)
				(monitored-thread (thread (lambda () (ticker 2 2 monitor)))))
			   (set! monitor (make-thread-monitor monitored-thread 4))
			   (monitor 'start)
			   (thread-wait monitored-thread)
			   (monitor 'stop)
			   (check-true ticker-finished-naturally)))))
  			 
(define (execute-thread-monitor-tests) 
  (let ((number-of-failures (run-tests tests 'verbose)))
    (= 0 number-of-failures)))
