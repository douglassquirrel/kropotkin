#!/usr/bin/racket
#lang racket
(require "secretary.rkt" "secretary-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)
(define INPUT_DIR   (getenv "INPUT_DIR"))
(define PUBLISH_DIR (getenv "PUBLISH_DIR"))

(cond ((execute-tests) (run-secretary))
      (else            (displayln "Tests failed, not deploying")))


