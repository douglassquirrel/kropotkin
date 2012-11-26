#lang racket
(require "vendor/planet/jaymccarthy/sqlite.rkt")
(define DATETIME_REGEX #px"\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")

(define (catalog? cat) db?)

(provide/contract
 (catalog? (any/c . -> . boolean?))
 (make-catalog (string? . -> . catalog?))
 (add-to-catalog (#:catalog catalog? #:name string? #:creation-datetime (and/c string? DATETIME_REGEX) #:contents bytes? . -> . void?))
 (get-latest-with-name (catalog? string? . -> . (or/c bytes? #f))))

(define CREATE-SQL   "create table if not exists catalog(name text, creation_datetime text, contents blob)")
(define STORE-SQL    "insert into catalog (name, creation_datetime, contents) values (?, ?, ?)")
(define RETRIEVE-SQL "select contents from catalog where name = ? order by creation_datetime desc")

(define (make-catalog catalog-file)
  (let ((cat (open (string->path catalog-file))))
    (exec/ignore cat CREATE-SQL)
    cat))

(define (add-to-catalog #:catalog cat #:name name #:creation-datetime creation-datetime #:contents contents)
  (let ((store-statement (prepare cat STORE-SQL)))
    (load-params store-statement name creation-datetime contents)
    (run store-statement)))
  
(define (get-latest-with-name cat name) 
  (let ((retrieve-statement (prepare cat RETRIEVE-SQL)))
    (load-params retrieve-statement name)
    (let ((result (step retrieve-statement)))
      (cond ((and (vector? result) (> (vector-length result) 0)) (vector-ref result 0))
	    (else #f)))))