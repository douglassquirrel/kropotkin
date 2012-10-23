#!/usr/bin/racket
#lang racket
(require "lib/run-resource.rkt" 
	 "test/engine-tests.rkt" "test/complications-tests.rkt" "test/thread-monitor-tests.rkt" 
	 "test/run-resource-tests.rkt" "test/resource-server-tests.rkt" "test/fetch-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(define TARGET (getenv "TARGET"))
(cond ((not TARGET) (error "Need to specify target using the TARGET environment variable")))
(printf "Running with target ~a\n" TARGET)
(define target-file (format "lib/~a.rkt" TARGET))
(define execute-tests (cond ((equal? TARGET "run-resource")    execute-run-resource-tests)
			    ((equal? TARGET "engine")          execute-engine-tests)
			    ((equal? TARGET "complications")   execute-complications-tests)
			    ((equal? TARGET "thread-monitor")  execute-thread-monitor-tests)
			    ((equal? TARGET "resource-server") execute-resource-server-tests)
			    ((equal? TARGET "fetch")           execute-fetch-tests)
			    (else                             (error "Unrecognised target" TARGET))))

(define PORT (string->number (getenv "PORT")))
(cond ((not PORT) (error "Need to specify HTTP port using the PORT environment variable")))
(printf "Running with port ~a\n" PORT)

(run-resource target-file execute-tests PORT)
