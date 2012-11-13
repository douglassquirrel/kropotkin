#!/usr/bin/racket
#lang racket
(require "../catalog/catalog-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((execute-tests) (displayln "Tests succeeded"))
      (else            (displayln "Tests failed, not deploying")))


