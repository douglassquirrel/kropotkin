#lang racket
(provide run-secretary)

(define (run-secretary)
  (displayln "Secretary running")
  (sleep 5)
  (run-secretary))