#!/usr/bin/racket
#lang racket
(require "bootstrap.rkt")

(fetch-files (list "run-resource.rkt"
		   "thread-monitor.rkt" 
		   "engine.rkt" 
		   "complications.rkt"
		   "resource-server.rkt"))

(switch-executables)