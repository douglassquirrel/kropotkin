#!/usr/bin/racket
#lang racket
(require "secretary-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((execute-secretary-tests) (displayln "Tests succeeded"))
      (else                    (displayln "Tests failed, not deploying")))


