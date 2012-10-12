#lang racket
(require web-server/web-server 
	 web-server/dispatchers/dispatch-files)
(provide deploy-resource-server)

(define (deploy-resource-server file-path port)
  (printf "Deploying resource server for file ~a on port ~a\n" file-path port)
  (serve
   #:dispatch
   (make
    #:url->path (lambda (url) (values (string->path file-path) empty))
    #:path->mime-type (lambda (path) #"application/octet-stream"))
   #:port port))