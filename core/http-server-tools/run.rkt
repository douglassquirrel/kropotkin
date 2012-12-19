#!/usr/bin/racket
#lang racket
(require file/tar) 
(require "http-server-tools.rkt" "http-server-tools-tests.rkt")

(file-stream-buffer-mode (current-output-port) 'line)
(cond ((not (getenv "HTTP_SERVER_TOOLS_PORT")) (error "Environment variable HTTP_SERVER_TOOLS_PORT not set")))
(define PORT (string->number (getenv "HTTP_SERVER_TOOLS_PORT")))

(define (tar-to-bytes paths)
  (let ((out (open-output-bytes)))
    (tar->output paths out)
    (get-output-bytes out)))

(cond ((execute-http-server-tools-tests) (displayln "Tests succeeded, deploying")
                                         (let ((tar-bytes (tar-to-bytes (list (string->path "http-server-tools.rkt") (string->path "http-checks.rkt")))))
					   (serve-bytes-as-file tar-bytes "http-server-tools.tar" PORT)))
      (else                              (displayln "Tests failed, not deploying")))


