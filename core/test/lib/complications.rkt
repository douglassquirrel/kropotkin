#lang racket
(provide apply-complications 
	 (rename-out (make-wait-complication   make-wait-side-effect))
	 (rename-out (make-thread-complication make-thread-side-effect)))

(define (make-wait-complication seconds) (lambda () (sleep seconds)))
(define (make-thread-complication thunk) (lambda () (thread thunk)))

(define (apply-complication complication) (apply complication empty))
(define (apply-complications complications)
  (cond ((procedure? complications) (apply-complication complications))
	((list? complications)      (for-each apply-complication complications))
	(else (error "Side effects neither a list nor a single procedure"))))

