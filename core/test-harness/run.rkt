#!/usr/bin/racket
#lang racket
(require "test-harness-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)
(define OUTPUT_DIR (getenv "OUTPUT_DIR"))

(cond ((execute-test-harness-tests) (copy-file "test-harness.rkt" (build-path OUTPUT_DIR "test-harness.rkt")))
      (else                         (displayln "Tests failed, not deploying")))


