#lang racket
(require net/url)
(provide fetch-files switch-executables)

(define (lookup-url-for file)
  (cond ((equal? file "run-resource.rkt")    (string->url "http://localhost:8080"))
	((equal? file "thread-monitor.rkt")  (string->url "http://localhost:8081"))
	((equal? file "engine.rkt")          (string->url "http://localhost:8082"))
	((equal? file "complications.rkt")   (string->url "http://localhost:8083"))
	((equal? file "resource-server.rkt") (string->url "http://localhost:8084"))
	(else (error "lookup failed for file" file))))

(define (slurp-to-file url file-path)
  (define file-bytes (call/input-url url get-pure-port port->bytes))
  (call-with-output-file file-path (lambda (out) (write-bytes file-bytes out)))
  (void))

(define (fetch-file file-name dir) 
  (define file-path (build-path dir file-name))
  (slurp-to-file (lookup-url-for file-name) file-path))
(define (fetch-files list-of-files [dir "lib"]) (for-each (lambda (file) (fetch-file file dir)) list-of-files))

;;;;;;;;;;;;;;;;;;;;;;

(define (current-permission-bits file) (file-or-directory-permissions file 'bits))
(define (make-user-executable file)
  (file-or-directory-permissions file (bitwise-ior (current-permission-bits file) #o100)))
(define (make-user-non-executable file)
  (file-or-directory-permissions file (bitwise-and (current-permission-bits file) #o677)))
(define (switch-executables [build-file "build.rkt"] [run-file "run.rkt"])
  (make-user-non-executable build-file)
  (make-user-executable run-file))