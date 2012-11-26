#lang racket
(require "vendor/planet/jaymccarthy/sqlite.rkt")
(define DATETIME_REGEX #px"\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")

(provide/contract
 (add-to-catalog (#:name string? #:creation-datetime (and/c string? DATETIME_REGEX) #:contents bytes? . -> . void?))
 (get-latest-with-name (string? . -> . (or/c bytes? #f))))

(define CREATE-SQL   "create table if not exists catalog(name text, creation_datetime text, contents blob)")
(define STORE-SQL    "insert into catalog (name, creation_datetime, contents) values (?, ?, ?)")
(define RETRIEVE-SQL "select contents from catalog where name = ? order by creation_datetime desc")

(define CATALOG_FILE (getenv "CATALOG_FILE"))
(printf "Storing catalog in ~a\n" CATALOG_FILE)

(define db (open (string->path CATALOG_FILE)))
(exec/ignore db CREATE-SQL)

(define (add-to-catalog #:name name #:creation-datetime creation-datetime #:contents contents)
  (let ((store-statement (prepare db STORE-SQL)))
    (load-params store-statement name creation-datetime contents)
    (run store-statement)))
  
(define (get-latest-with-name name) 
  (let ((retrieve-statement (prepare db RETRIEVE-SQL)))
    (load-params retrieve-statement name)
    (let ((result (step retrieve-statement)))
      (cond ((and (vector? result) (> (vector-length result) 0)) (vector-ref result 0))
	    (else #f)))))