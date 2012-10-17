#lang racket
(provide make-thread-monitor)

(define (make-thread-monitor monitored-thread wait-seconds)
  (let ((tick-time      (current-seconds))
	(keep-running   #t))
    (define (kill-monitored-thread)
	     (printf "No tick in the last ~a seconds - shutting down monitored thread\n" wait-seconds)
	     (kill-thread monitored-thread))
    (define (run-monitor [last-observed-tick 0])
      (sleep wait-seconds)
      (cond ((not keep-running) (displayln "Thread monitor stopping"))
	    ((= last-observed-tick tick-time) (kill-monitored-thread))
	    (else (displayln "Monitor OK") (run-monitor tick-time))))
    (lambda (command) 
      (cond ((eq? command 'start) (thread (lambda () (run-monitor))))
	    ((eq? command 'tick)  (set! tick-time (current-seconds)))
	    ((eq? command 'stop)  (set! keep-running #f))
	    (else (error "Unrecognised command to thread monitor"))))))




