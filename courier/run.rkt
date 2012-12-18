#!/usr/bin/racket
#lang racket
(require "courier-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((not (getenv "CLERK_PORT")) (error "Environment variable CLERK_PORT not set")))
(define CLERK_PORT (string->number (getenv "CLERK_PORT")))

(cond ((execute-tests) (displayln "Tests succeeded, deploying")
                       ) ;(start-clerk CLERK_PORT sorting-office-thread))
      (else            (displayln "Tests failed, not deploying")))


