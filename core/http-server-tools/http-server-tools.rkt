#lang racket
(require web-server/http web-server/servlet-env)
(provide serve-bytes-as-file)

(define (serve-request request)
  (response/full 200 #"OK" (current-seconds) #"application/octet-stream" empty (list #"hello")))

(define (serve-bytes-as-file bytes name port)
    (serve/servlet serve-request #:port port #:servlet-regexp #rx"" #:command-line? #t))
