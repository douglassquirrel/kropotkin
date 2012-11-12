#!/usr/bin/racket
#lang racket
(require "hailstone-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)
(define OUTPUT_DIR (getenv "OUTPUT_DIR"))

(cond ((execute-tests) (copy-file "hailstone.rkt" (build-path OUTPUT_DIR "hailstone.rkt")))
      (else            (displayln "Tests failed, not deploying")))


