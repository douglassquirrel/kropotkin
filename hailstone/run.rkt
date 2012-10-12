#!/usr/bin/racket
#lang racket
(require "../core/lib/run-resource.rkt" "hailstone-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)

(define PORT (string->number (getenv "PORT")))
(cond ((not PORT) (error "Need to specify HTTP port using the PORT environment variable")))
(printf "Running with port ~a\n" PORT)

(run-resource "hailstone.rkt" execute-tests PORT)
