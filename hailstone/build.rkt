#!/usr/bin/racket
#lang racket
(require "bootstrap/fetch.rkt" 
	 "bootstrap/file-permissions.rkt")

(fetch-files (list "run-resource.rkt"
		   "thread-monitor.rkt" 
		   "engine.rkt" 
		   "complications.rkt"
		   "resource-server.rkt")
	     "lib")

(make-user-non-executable "build.rkt")
(make-user-executable "run.rkt")