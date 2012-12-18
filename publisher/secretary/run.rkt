#!/usr/bin/racket
#lang racket
(require "secretary-tests.rkt" "secretary.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((execute-tests) ((displayln "Tests succeeded, deploying")
			(run-secretary)))
      (else            (displayln "Tests failed, not deploying")))


