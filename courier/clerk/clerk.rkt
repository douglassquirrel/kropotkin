#lang racket
(require web-server/http web-server/servlet-env net/url)
(provide start-clerk)

(define (serve-request request)
  (response/full 200 #"OK" (current-seconds) #"application/octet-stream" empty (list #"hello")))

(define (start-clerk port)
  (printf "Starting clerk on port ~a\n" port)
  (serve/servlet serve-request #:port port #:servlet-regexp #rx"" #:command-line? #t)
  (displayln "Clerk started"))