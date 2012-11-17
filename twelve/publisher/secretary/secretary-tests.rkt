#lang racket
(require "library/catalog/catalog-tests.rkt")
(provide execute-tests)

(define (execute-secretary-tests)
  (displayln "Executing secretary tests"))

(define (execute-tests)
  (and (execute-catalog-tests) (execute-secretary-tests)))
