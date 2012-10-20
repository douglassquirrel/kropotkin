#lang racket
(provide make-user-executable make-user-non-executable)

(define (current-permission-bits file) (file-or-directory-permissions file 'bits))
(define (make-user-executable file)
  (file-or-directory-permissions file (bitwise-ior (current-permission-bits file) #o100)))
(define (make-user-non-executable file)
  (file-or-directory-permissions file (bitwise-and (current-permission-bits file) #o677)))