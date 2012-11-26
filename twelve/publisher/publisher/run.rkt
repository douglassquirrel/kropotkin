#!/usr/bin/racket
#lang racket
(require "publisher-tests.rkt" "publisher.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((not (getenv "PORT")) (error "Environment variable PORT not set")))
(define PORT (string->number (getenv "PORT")))


(cond ((execute-tests) ((displayln "Tests succeeded, deploying")
			(start-server PORT)))
      (else            (displayln "Tests failed, not deploying")))


