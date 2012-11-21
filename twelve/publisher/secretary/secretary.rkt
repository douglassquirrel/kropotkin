#lang racket
(require racket/date "library/catalog.rkt")
(provide run-secretary)

;add contract

(define INPUT_DIR (getenv "INPUT_DIR"))
(printf "Secretary monitoring ~a\n" INPUT_DIR)

(define (creation-datetime file)
   (parameterize ((date-display-format 'iso-8601)) 
		 (date->string (seconds->date (file-or-directory-modify-seconds file)) #t)))

(define (add-file-to-catalog filename)
  (let ((file (build-path INPUT_DIR filename)))
    (printf "Adding ~a to catalog\n" file)
    (add-to-catalog #:name              (path->string filename) 
		    #:creation-datetime (creation-datetime file)
		    #:contents          (file->bytes file))
    (delete-file file)))

(define (run-secretary)
  (displayln "Secretary running")
  (for-each add-file-to-catalog (directory-list INPUT_DIR))
  (sleep 1)
  (run-secretary))