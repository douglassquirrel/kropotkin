#!/usr/bin/racket
#lang racket
(require net/url "bootstrap/fetch.rkt" "bootstrap/file-permissions.rkt")

(define (lookup-url-for file)
  (cond ((equal? file "run-resource.rkt")    (string->url "http://localhost:8080"))
	((equal? file "thread-monitor.rkt")  (string->url "http://localhost:8081"))
	((equal? file "engine.rkt")          (string->url "http://localhost:8082"))
	((equal? file "complications.rkt")   (string->url "http://localhost:8083"))
	((equal? file "resource-server.rkt") (string->url "http://localhost:8084"))
	((equal? file "fetch.rkt")           (string->url "http://localhost:8085"))
	(else (error "lookup failed for file" file))))

(fetch-files (list "run-resource.rkt"
		   "thread-monitor.rkt" 
		   "engine.rkt" 
		   "complications.rkt"
		   "resource-server.rkt"
		   "fetch.rkt")
	     "lib" lookup-url-for)

(make-user-non-executable "build.rkt")
(make-user-executable "run.rkt")