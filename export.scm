(use args)
(use data-structures)
(use ports)
(use http-client)
(use intarweb uri-common)
(use json)

(include "common")

(define (siphon-input input-port output-port)
  (let ((byte (read-byte input-port)))
    (if (eq? #!eof byte)
        #t
        (begin (write-byte byte output-port)
               (siphon-input input-port output-port)))))

(define (export-call conn-info cmd-args)
  (let ((filename (acdr 'outfile cmd-args)))
    (maybe-set-ssl-context conn-info)
    (with-input-from-request (archive-uri (hash-table-ref conn-info 'root_url))
                             #f
                             (lambda ()
                               (with-output-to-file filename
                                 (lambda ()
                                   (siphon-input (current-input-port) (current-output-port))))))))

(define opts
  (list (args:make-option (o outfile)
                          (required: "FILE")
                          "File name for the archive to export")
        (args:make-option (a alias)
                          (required: "ALIAS")
                          "Alias for the PuppetDB instance in ~/.pdbrc, uses \"default\" if not specified")
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
  (let ((cmd-args (args:parse args opts)))
    (export-call (get-default-conn-info cmd-args) cmd-args)))
