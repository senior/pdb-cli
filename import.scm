(use args data-structures ports http-client intarweb uri-common medea snowtar z3)

(include "common")

(define (command-metadata? tar-rec)
  (string=? "puppetdb-bak/export-metadata.json" (tar-rec-name tar-rec)))

(define (import-archive conn-info archive-filename command-versions)
  (maybe-set-ssl-context conn-info)
  (with-input-from-request (archive-uri (hash-table-ref conn-info 'root_url))
                           `(("command_versions" . ,command-versions)
                             ("archive" file: ,archive-filename
                              headers: ((content-type application/octet-stream))))
                           read-string))

(define (extract-command-versions file)
  (let ((tr (car (filter command-metadata? (tar-unpack-genport (z3:open-compressed-input-file file) )))))
    (with-input-from-string (blob->string (u8vector->blob (tar-rec-content tr)))
      (lambda ()
        (with-output-to-string
          (lambda ()
            (json-write
             (cdr (assoc "command_versions" (vector->list (json-read (current-input-port)))))
             (current-output-port))))))))

(define (import-file conn-info cmd-args)
  (let ((file (acdr 'infile cmd-args)))
    (print "importing file " file)
    (import-archive conn-info file (extract-command-versions file))))

(define opts
  (list (args:make-option (i infile)
                          #:required
                          "Path the the PuppetDB exported archive")
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
     (print "Usage: " (car (argv)) " [options...]")
     (newline)
     (print (args:usage opts))))
 (exit 1))

(define (main args)
  (let ((cmd-args (args:parse args opts)))
    (import-file (get-default-conn-info cmd-args) cmd-args)))
