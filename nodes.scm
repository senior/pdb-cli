(use args)
(use data-structures)
(use ports)
(use http-client)
(use intarweb uri-common)
(use medea)
(use pathname-expand)
(use openssl)
(use section-combinators)
(use srfi-13)

(define (user-config)
  (with-input-from-file (pathname-expand "~/.pdbrc") read-json))

(define (make-ssl-context/client-cert ca-cert-path cert-path key-path)
  (let ((ssl-ctx (ssl-make-client-context 'tls)))

    (ssl-load-suggested-certificate-authorities! ssl-ctx ca-cert-path)
    (ssl-load-verify-root-certificates! ssl-ctx ca-cert-path)
    (ssl-set-verify! ssl-ctx #t)

    (ssl-load-certificate-chain! ssl-ctx cert-path)
    (ssl-load-private-key! ssl-ctx key-path)

    ssl-ctx))

(define (make-ssl-server-connector/context ssl-ctx)
  (lambda (uri proxy)
    (let ((remote-end (or proxy uri)))
      (if (eq? 'https (uri-scheme remote-end))
          (ssl-connect (uri-host remote-end)
                       (uri-port remote-end)
                       ssl-ctx)
          (default-server-connector uri proxy)))))

(define (alist->ht alist)
  (alist->hash-table alist eq? symbol-hash))

(define (safe-string-length s)
  (if (eq? s 'null)
      (string-length "null")
      (string-length s)))

(define (column-length row col)
  (safe-string-length (hash-table-ref row col)))

(define (max-col-length row max-alist)
  (map (lambda (max-pair)
         (let ((col (car max-pair)))
           (cons col (max (cdr max-pair)
                          (column-length row col)))))
       max-alist))

(define (stringify s)
  (if (symbol? s)
      (symbol->string s)
      s))

(define (pad-column col max-width)
  (string-append " "  (stringify col)
                 (make-string (- max-width (safe-string-length col)) #\ )))

(json-parsers
 (alist-update 'object
               alist->ht
               (json-parsers)))

(define node-column-order
  '(certname deactivated expired catalog_timestamp facts_timestamp report_timestamp catalog_environment facts_environment report_environment))

(define (order-columns defined-order result-columns)
  (filter (left-section hash-table-exists? result-columns) defined-order))

(define (find-max-col-lengths rows)
  (fold max-col-length
        (map (lambda (col)
               (cons col 0))
             (order-columns node-column-order (car rows)))
        rows))

(define (header-row rows)
  (alist->ht (map (lambda (col-name)
                    (cons col-name (symbol->string col-name)))
                  (hash-table-keys (car rows)))))

(define (tabular-output rows)
  (let* ((rows (cons (header-row rows) rows))
         (max-lengths (find-max-col-lengths rows)))
    (do ((rows rows (cdr rows)))
        ((null? rows) #t)
      (apply print (map (lambda (col-name-and-length)
                          (pad-column (hash-table-ref (car rows) (car col-name-and-length))
                                      (cdr col-name-and-length)))
                        max-lengths)))))

(define (needs-quotes? s)
  (or (string-contains s ",")
      (string-contains s "\"")
      (string-contains s "\n")))

(define (quote-cell s)
  (if (needs-quotes? s)
      (string-append "\"" s "\"")
      s))

(define (replace-with-substring s search-char replace-string)
  (string-fold (lambda (c acc)
                 (if (char=? c search-char)
                     (string-append acc replace-string)
                     (string-append acc (string c))))
               "" s))

(define (escape-quotes s)
  (if (string-contains s "\"")
      (replace-with-substring s #\" "\"\"")
      s))

(define (escape-cell cell)
  (quote-cell (escape-quotes cell)))

(define (emit-csv-row row)
  (string-join (map escape-cell row) ","))

(define (csv-output rows)
  (let* ((header (header-row rows))
         (rows (cons header rows))
         (column-order (filter (lambda (column)
                                 (hash-table-exists? header column))
                               node-column-order)))

    (do ((rows rows (cdr rows)))
        ((null? rows) #t)
      (print
       (emit-csv-row
        (map (lambda (column)
               (stringify (hash-table-ref (car rows) column))) column-order))))))

(define (output->console opts)
  (lambda ()
    (cond
     ((assoc 'json opts)
      (print (read-string)))
     ((assoc 'csv opts)
      (csv-output (vector->list (read-json))))
     (else
      (tabular-output (vector->list (read-json)))))))

(define (acdr k alist)
  (let ((val (assoc k alist)))
    (if val
        (cdr val)
        #f)))

(define (read-query-file filename)
  (call-with-input-file filename
    (lambda (file-input-port)
      (call-with-output-string
       (lambda (string-output-port)
         (copy-port file-input-port string-output-port))))))

(define (query cmd-opts)
  (cond
   ((assoc 'query cmd-opts)
    (acdr 'query cmd-opts))
   ((assoc 'pql cmd-opts)
    (acdr 'pql cmd-opts))
   ((assoc 'query-file cmd-opts)
    (read-query-file (acdr 'query-file cmd-opts)))
   (else
    #f)))

(define (maybe-set-ssl-context conn-info)
  (when (eq? 'https (uri-scheme (uri-reference (hash-table-ref conn-info 'root_url))))
    (let ((ssl-ctx (make-ssl-context/client-cert (hash-table-ref conn-info 'ca)
                                                 (hash-table-ref conn-info 'cert)
                                                 (hash-table-ref conn-info 'key))))
      (server-connector (make-ssl-server-connector/context ssl-ctx)))))

(define (query-uri root-uri pql?)
  (uri-reference
   (string-append root-uri
                  (if pql?
                      "/pdb/query/v4/"
                      "/pdb/query/v4/nodes"))))

(define (query-nodes conn-info cmd-opts)
  (let ((uri (query-uri (hash-table-ref conn-info 'root_url)
                        (assoc 'pql cmd-opts)))
        (query (query cmd-opts)))
    (maybe-set-ssl-context conn-info)
    (with-input-from-request (make-request uri: uri
                                           conn-factory:
                                           method: 'GET)
                             (if query
                                 (list (cons 'query query))
                                 #f)
                             (output->console cmd-opts))))

(define opts
  (list
   (args:make-option (q query)
                     (required: "QUERY")
                     "PuppetDB query string")
   (args:make-option (p pql)
                     (required: "PQL QUERY")
                     "PuppetDB Query Language string")
   (args:make-option (f query-file)
                     (required: "FILE")
                     "Path to file containing a PuppetDB query")
   (args:make-option (a alias)
                     (required: "ALIAS")
                     "Alias for the PuppetDB instance in ~/.pdbrc, uses \"default\" if not specified")
   (args:make-option (t tab)
                     #:none
                     "Tabular output format")
   (args:make-option (j json)
                     #:none
                     "JSON output format")
   (args:make-option (c csv)
                     #:none
                     "CSV output format")
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

(define (get-default-conn-info args)
  (let* ((config (user-config))
         (default (if (assoc 'alias args)
                      (acdr 'alias args)
                      (hash-table-ref config 'default_host)))
         (host-list (hash-table-ref config 'hosts)))
    (hash-table-ref host-list (string->symbol default))))

(define (main args)
  (let ((more-args (args:parse args opts)))
    (query-nodes (get-default-conn-info more-args) more-args)))
