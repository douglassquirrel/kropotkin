#!/usr/bin/racket
#lang racket
(require "tycoon-tests.rkt" "tycoon.rkt" "library/catalog.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(cond ((not (getenv "TYCOON_PORT")) (error "Environment variable TYCOON_PORT not set")))
(define PORT (string->number (getenv "TYCOON_PORT")))
(cond ((not (getenv "CATALOG_FILE")) (error "Environment variable CATALOG_FILE not set")))
(define CATALOG_FILE (getenv "CATALOG_FILE"))

(cond ((execute-tests) (displayln "Tests succeeded, deploying")
                       (let ((cat (make-catalog CATALOG_FILE)))
			 (start-server PORT cat)))
      (else            (displayln "Tests failed, not deploying")))


