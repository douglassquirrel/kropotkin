#lang racket
(require rackunit rackunit/text-ui)
(provide execute-tests)

(define (execute-tests tests [mode 'verbose])
  (cond ((empty? tests) #t)
	(else
	 (let ((number-of-failures (run-tests tests)))
	   (= 0 number-of-failures)))))
