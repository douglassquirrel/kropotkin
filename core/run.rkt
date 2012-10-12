#!/usr/bin/racket
#lang racket
(require "lib/run-resource.rkt" 
	 "test/engine-tests.rkt" "test/complications-tests.rkt" "test/thread-monitor-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(define TARGET (getenv "TARGET"))
(cond ((not TARGET) (error "Need to specify target using the TARGET environment variable")))
(printf "Running with target ~a\n" TARGET)
(define target-file (format "lib/~a.rkt" TARGET))
(define execute-tests (cond ((equal? TARGET "engine")         execute-engine-tests)
			    ((equal? TARGET "complications")  execute-complications-tests)
			    ((equal? TARGET "thread-monitor") execute-thread-monitor-tests)
			    (else                            (error "Unrecognised target" TARGET))))

(define PORT (string->number (getenv "PORT")))
(cond ((not PORT) (error "Need to specify HTTP port using the PORT environment variable")))
(printf "Running with port ~a\n" PORT)

(run-resource target-file execute-tests PORT)