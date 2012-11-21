#!/usr/bin/racket
#lang racket
(require "secretary-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((execute-tests) ((displayln "Tests succeeded, deploying")
			 ;(start-secretary)
			))
      (else            (displayln "Tests failed, not deploying")))


