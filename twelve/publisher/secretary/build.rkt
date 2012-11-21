#!/usr/bin/racket
#lang racket
(require net/url)

(file-stream-buffer-mode (current-output-port) 'line)

(define CATALOG_URL (getenv "CATALOG_URL"))

(make-directory "library")
(with-output-to-file "library/catalog.tar" 
		     (lambda () (call/input-url (string->url CATALOG_URL) get-pure-port display-pure-port)))
(system "tar -x -v -C library -f library/catalog.tar")

(file-or-directory-permissions "build.rkt" #o666)
(file-or-directory-permissions "run.rkt"   #o777)