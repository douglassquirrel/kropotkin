#lang racket
(provide start-server)

(define (start-server port [notify-thread #f])
  (printf "Starting publisher on port ~a\n" port)
  (sleep 1)
  (displayln "Publisher started")
  (cond ((not (false? notify-thread)) 
	 (displayln "Notifying re successful start") 
	 (thread-send notify-thread 'started #f)))) 