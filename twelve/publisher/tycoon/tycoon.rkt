#lang racket
(require web-server/http web-server/servlet-env net/url)
(require "library/catalog.rkt")
(provide start-server)

(define OK        (cons 200 #"OK"))
(define NOT_FOUND (cons 404 #"Not Found"))
(define numeric-code car)
(define message      cdr)

(define (make-response resp-code bytes)
  (response/full (numeric-code resp-code) (message resp-code) (current-seconds) #"application/octet-stream" empty (list bytes)))

(define (path-of-request request) (url-path (request-uri request)))

(define (serve-request cat request)
  (let ((path (path/param-path (first (path-of-request request)))))
    (cond ((get-latest-with-name cat path) (make-response OK        #"foo"))
	  (else                            (make-response NOT_FOUND #"File not found")))))

(define (start-server port cat)
  (printf "Starting tycoon on port ~a\n" port)
  (serve/servlet ((curry serve-request) cat) #:port port #:servlet-regexp #rx"" #:command-line? #t)
  (displayln "Tycoon started"))