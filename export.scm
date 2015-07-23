(use args)
(use data-structures)
(use ports)
(use http-client)
(use intarweb uri-common)
(use json)

(define (siphon-input input-port output-port)
  (let ((byte (read-byte input-port)))
    (if (eq? #!eof byte)
        #t
        (begin (write-byte byte output-port)
               (siphon-input input-port output-port)))))

(define (export-call filename)
  (with-input-from-request "http://localhost:8080/pdb/admin/v1/archive" #f
                           (lambda ()
                             (with-output-to-file filename
                               (lambda ()
                                 (siphon-input (current-input-port) (current-output-port)))))))

(define opts
  (list (args:make-option (o outfile)
                          #:required
                          "File name for the archive to export"
                          (export-call arg))
        (args:make-option (h help)
                          #:none
                          "Display this text"
                          (usage))))

(define (usage)
 (with-output-to-port (current-error-port)
   (lambda ()
     (print argv)
     (print "Usage: " (car (argv)) " [options...]")
     (newline)
     (print (args:usage opts))))
 (exit 1))

(define (main args)
  (args:parse args opts))
