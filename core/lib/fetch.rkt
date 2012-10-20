#lang racket
(require net/url)
(provide fetch-files)

(define (lookup-url-for file)
  (cond ((equal? file "run-resource.rkt")    (string->url "http://localhost:8080"))
	((equal? file "thread-monitor.rkt")  (string->url "http://localhost:8081"))
	((equal? file "engine.rkt")          (string->url "http://localhost:8082"))
	((equal? file "complications.rkt")   (string->url "http://localhost:8083"))
	((equal? file "resource-server.rkt") (string->url "http://localhost:8084"))
	((equal? file "fetch.rkt")           (string->url "http://localhost:8085"))
	(else (error "lookup failed for file" file))))

;(define FETCH_PORT (string->number (getenv "FETCH_PORT")))
;(cond ((not FETCH_PORT) (error "Need to specify local port for fetching resources using the FETCH_PORT environment variable")))
;(printf "Running with fetch port ~a\n" FETCH_PORT)
;(define (url-builder file) (string->url (format "http://localhost:~a/~a" FETCH_PORT file)))

(define (slurp-to-file url file-path)
  (define file-bytes (call/input-url url get-pure-port port->bytes))
  (call-with-output-file file-path (lambda (out) (write-bytes file-bytes out)))
  (void))

(define (file-fetcher make-url dir) (lambda (file) (slurp-to-file (make-url file) (build-path dir file))))

(define (fetch-files list-of-files dir [make-url lookup-url-for]) (for-each (file-fetcher make-url dir) list-of-files))

