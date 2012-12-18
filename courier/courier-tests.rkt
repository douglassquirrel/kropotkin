#lang racket
(require rackunit rackunit/text-ui net/url)
(require "clerk/clerk-tests.rkt")
(provide execute-tests)

(define (execute-tests)
  (execute-clerk-tests))
