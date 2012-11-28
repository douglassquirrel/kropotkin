#lang racket
(require web-server/http web-server/servlet-env)
(provide start-server)

(define (fetch-file-from-catalog request)
  (response/full
   404 #"Not Found" (current-seconds) #"text/plain" empty
   (list #"File not found")))

(define (start-server port)
  (printf "Starting publisher on port ~a\n" port)
  (serve/servlet fetch-file-from-catalog #:port port #:servlet-regexp #rx"" #:command-line? #t)
  (displayln "Publisher started"))