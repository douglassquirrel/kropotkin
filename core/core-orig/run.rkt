#!/usr/bin/racket
#lang racket
(require "lib/run-resource.rkt" 
	 "test/engine-tests.rkt" "test/complications-tests.rkt" "test/thread-monitor-tests.rkt" 
	 "test/run-resource-tests.rkt" "test/resource-server-tests.rkt" "test/test-harness-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(define OUTPUT_DIR (getenv "OUTPUT_DIR"))
(cond ((not OUTPUT_DIR) (error "Need to specify output directory using the OUTPUT_DIR environment variable")))
(printf "Running with output directory ~a\n" OUTPUT_DIR)

(define TARGET (getenv "TARGET"))
(cond ((not TARGET) (error "Need to specify target using the TARGET environment variable")))
(printf "Running with target ~a\n" TARGET)

(define filename (format "~a.rkt" TARGET))
(define target-file (path->string (build-path "lib" filename)))
(define output-file (format "~a/~a.rkt" OUTPUT_DIR filename))
(define run-tests (cond ((equal? TARGET "run-resource")    execute-run-resource-tests)
			    ((equal? TARGET "engine")          execute-engine-tests)
			    ((equal? TARGET "complications")   execute-complications-tests)
			    ((equal? TARGET "thread-monitor")  execute-thread-monitor-tests)
			    ((equal? TARGET "resource-server") execute-resource-server-tests)
			    ((equal? TARGET "test-harness")    execute-test-harness-tests)
			    (else                             (error "Unrecognised target" TARGET))))

(cond ((run-tests) (printf "Tests passed, deploying ~a\n" TARGET)
                   (copy-file target-file output-file))
      (else        (printf "Tests failed, not deploying ~a\n" TARGET)))
