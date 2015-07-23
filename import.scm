(use args)
(use data-structures)
(use ports)
(use http-client)
(use intarweb uri-common)
(use json)
(use snowtar)
(use z3)

(define (command-metadata? tar-rec)
  (string=? "puppetdb-bak/export-metadata.json" (tar-rec-name tar-rec)))

(define (import-archive archive-filename command-versions)
  (with-input-from-request "http://localhost:8080/pdb/admin/v1/archive"
                           `(("command_versions" . ,command-versions)
                             ("archive" file: ,archive-filename
                              headers: ((content-type application/octet-stream))))
                           read-string))

(define (extract-command-versions file)
  (let ((tr (car (filter command-metadata? (tar-unpack-genport   (z3:open-compressed-input-file file) )))))
    (with-input-from-string (blob->string (u8vector->blob (tar-rec-content tr)))
      (lambda ()
        (with-output-to-string
          (lambda ()
            (json-write
             (cdr (assoc "command_versions" (vector->list (json-read (current-input-port)))))
             (current-output-port))))))))

(define (import-file file)
  (print "importing file " file)
  (import-archive file (extract-command-versions file)))

(define opts
  (list (args:make-option (i infile)
                          #:required
                          "Path the the PuppetDB exported archive"
                          (import-file arg))
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
