#lang racket
(provide deploy-resource-server)

(define (deploy-resource-server file)
  (printf "Deploying resource server for file ~a\n" file))