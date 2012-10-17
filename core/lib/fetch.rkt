#lang racket
(require net/url)
(provide fetch-files)

(define (lookup-url-for file)
  (cond ((equal? file "thread-monitor.rkt") (string->url "http://localhost:8081"))
	((equal? file "engine.rkt")         (string->url "http://localhost:8082"))
	((equal? file "complications.rkt")  (string->url "http://localhost:8083"))
	(else (error "lookup failed for file" file))))

(define (slurp-to-file url file-path)
  (define file-bytes (call/input-url url get-pure-port port->bytes))
  (call-with-output-file file-path (lambda (out) (write-bytes file-bytes out)))
  (void))

(define (fetch-file file-name dir) 
  (define file-path (build-path dir file-name))
  (slurp-to-file (lookup-url-for file-name) file-path))
(define (fetch-files list-of-files dir) (for-each (lambda (file) (fetch-file file dir)) list-of-files))

