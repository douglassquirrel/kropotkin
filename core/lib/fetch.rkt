#lang racket
(require net/url)
(provide download-file)

(define (download-file url file)
  (define file-bytes (call/input-url (string->url url) get-pure-port port->bytes))
  (call-with-output-file file (lambda (out) (write-bytes file-bytes out)))
  (void))