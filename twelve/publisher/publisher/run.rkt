#!/usr/bin/racket
#lang racket
(require "publisher-tests.rkt" "publisher.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((execute-tests) ((displayln "Tests succeeded, deploying")
			(run-publisher)))
      (else            (displayln "Tests failed, not deploying")))


