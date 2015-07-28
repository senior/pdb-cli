(use args)
(use data-structures)
(use ports)
(use http-client)
(use intarweb uri-common)
(use medea)
(use pathname-expand)
(use openssl)

(define (user-config)
  (with-input-from-file (pathname-expand "~/.pdbrc") read-json))

(define (ssl-conn-factory ca-cert-path cert-path key-path)
  (lambda (uri-host uri-port)
    (let ((ssl-ctx (ssl-make-client-context 'tls)))
      (ssl-load-certificate-chain! ssl-ctx cert-path)
      (ssl-load-private-key! ssl-ctx key-path)
      (ssl-load-suggested-certificate-authorities! ssl-ctx ca-cert-path)
      (ssl-load-verify-root-certificates! ssl-ctx ca-cert-path)
      (ssl-set-verify! ssl-ctx #t)
      (ssl-connect uri-host
                   uri-port
                   ssl-ctx))))

(define (make-node-uri root-url)
  (uri-reference (string-append root-url "/pdb/query/v4/nodes")))

(define (vassoc k vec-pairs)
  (let ((idx (vector-index (lambda (pair)
                             (string= k (car pair))) vec-pairs)))
    (when idx
      (vector-ref vec-pairs idx))))

(define (string-alist->hash-table string-alist)
  (vector-fold (lambda (index hashtable pair)
                 (hash-table-set! hashtable (car pair) (cdr pair))
                 hashtable)
               (make-hash-table string=? string-hash)
               string-alist))

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
           `(,col . ,(max (cdr max-pair)
                          (column-length row col)))))
       max-alist))

(define (pad-column col max-width)
  (string-append " "  (if (symbol? col)
                          (symbol->string col)
                          col)
                 (make-string (- max-width (safe-string-length col)) #\ )))

(json-parsers
 (alist-update 'object
               alist->ht
               (json-parsers)))

(define (find-max-col-lengths rows)
  (fold max-col-length
        (map (lambda (col)
               `(,col . 0))
             (hash-table-keys (car rows)))
        rows))

(define (header-row rows)
  (alist->ht (map (lambda (col-name)
                    `(,col-name . ,(symbol->string col-name)))
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

(define (output->console opts)
  (lambda ()
    (cond
     ((assoc 'tab opts)
      (tabular-output (vector->list (read-json))))
     ((assoc 'json opts)
      (print (read-string)))
     (else (print (read-string))))))

(define (query-nodes conn-info query)
  (let ((uri (make-node-uri (hash-table-ref conn-info 'root_url))))
    (with-input-from-request (make-request uri: uri
                                           conn-factory: (ssl-conn-factory (hash-table-ref conn-info 'ca)
                                                                           (hash-table-ref conn-info 'cert)
                                                                           (hash-table-ref conn-info 'key))
                                           method: 'GET)
                             #f
                             (output->console query))))

(define opts
  (list (args:make-option (q query)
                          (optional: "QUERY")
                          "PuppetDB query string")
        (args:make-option (f query-file)
                          (optional: "FILE")
                          "Path to file containing a PuppetDB query")
        (args:make-option (a alias)
                          (optional: "ALIAS")
                          "Alias for the PuppetDB instance in ~/.pdbrc, uses \"default\" if not specified")
        (args:make-option (t tab)
                          #:none
                          "Tabular output format")
        (args:make-option (j json)
                          #:none
                          "JSON output format")
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


(define (get-default-conn-info)
  (let* ((config (user-config))
         (default (hash-table-ref config 'default_host))
         (host-list (hash-table-ref config 'hosts)))
    (hash-table-ref host-list (string->symbol default))))

(define (main args)
  (let ((more-args (args:parse args opts)))
    (query-nodes (get-default-conn-info) more-args)))
