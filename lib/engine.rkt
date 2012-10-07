#lang racket
(require "thread-monitor.rkt" "complications.rkt")
(provide (except-out (all-from-out "complications.rkt") apply-complications))
(provide make-engine make-functional-engine start)

(define (make-wheel gearing)
  (lambda (state) 
    (let-values (((new-state complications) (gearing state)))
      (apply-complications complications)
      new-state)))

(define (mainspring wheel monitor [state 'init])
  (printf "Tick, state ~a\n" state)
  (cond ((not (eq? state 'exit))
	 (monitor 'tick)
	 (mainspring wheel monitor (wheel state)))))

(define (make-engine gearing [monitor-seconds 60])
  (define wheel (make-wheel gearing))
  (define monitor (make-thread-monitor (current-thread) monitor-seconds))
  (list gearing wheel monitor))

(define (make-functional-engine functional-gearing [monitor-seconds 60])
  (define (gearing state) (values (functional-gearing state) empty))
  (make-engine gearing monitor-seconds))

(define (start engine) 
  (let-values (((gearing wheel monitor)
		(values (car engine) (cadr engine) (caddr engine))))
	      (monitor 'start)
	      (mainspring wheel monitor)
	      (monitor 'stop)))