#!/usr/bin/racket
#lang racket
(require file/tar "catalog-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)
(define OUTPUT_DIR (getenv "OUTPUT_DIR"))

(cond ((execute-catalog-tests) (tar (build-path OUTPUT_DIR "catalog.tar") "catalog.rkt" "vendor"))
      (else                    (displayln "Tests failed, not deploying")))


