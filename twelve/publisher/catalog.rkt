#lang racket
(require "vendor/planet/jaymccarthy/sqlite.rkt")

; (define db (open (string->path "test.s3")))
; (exec/ignore db "create table tbl4(one varchar(10), two smallint)")
; (exec/ignore db "insert into tbl4 values('hello!', 10)")
; (exec db "select * from tbl4" (lambda (names values) (displayln values) 0))
;
; #(hello! 10)
