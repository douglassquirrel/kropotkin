#lang racket
(require web-server/http web-server/servlet-env)
(provide start-server)

(define (fetch-file-from-catalog request)
  (displayln (request-method request))
  (response/full
   200 #"OK" (current-seconds) #"application/octet-stream" empty
   (list #"response")))

(define (start-server port)
  (printf "Starting publisher on port ~a\n" port)
  (serve/servlet fetch-file-from-catalog #:port port #:servlet-regexp #rx"" #:command-line? #t)
  (displayln "Publisher started"))