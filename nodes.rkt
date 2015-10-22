#lang racket

(require racket/cmdline)
(require openssl)
(require json)
(require net/http-client)
(require net/url)
(require racket/port)

(define query-string (make-parameter #f))
(define pql-string (make-parameter #f))
(define alias (make-parameter #f))
(define output-format (make-parameter 'tab))

(define (json-from-file filename)
  (let [(file (open-input-file filename
                               #:mode 'text))]
    (read-json file)))

(define (user-config)
  (json-from-file (expand-user-path "~/.pdbrc")))

(define (host-config)
  (let* [(config (user-config))
         (hosts (hash-ref config 'hosts))
         (alias (alias))]
    (if alias
        (hash-ref hosts (string->symbol alias))
        (hash-ref hosts (string->symbol (hash-ref config 'default_host))))))

(define (ssl-context host-info)
  (let [(ssl-context (ssl-make-client-context 'auto))]
    (ssl-load-certificate-chain! ssl-context (hash-ref host-info 'cert))
    (ssl-load-verify-source! ssl-context (hash-ref host-info 'ca))
    (ssl-load-private-key! ssl-context (hash-ref host-info 'key))
    ssl-context))

(define (https? url)
  (equal? "https" (url-scheme url)))

(define (create-pdb-url host-info suffix)
  (combine-url/relative (string->url (hash-ref host-info 'root_url))
                        suffix))

(define (stringify s)
  (if (symbol? s)
      (symbol->string s)
      s))

(define (pad-column col max-width)
  (string-append " "  (stringify col)
                 (make-string (- max-width (safe-string-length col)) #\ )))

(define node-column-order
  '(certname deactivated expired catalog_timestamp facts_timestamp report_timestamp catalog_environment facts_environment report_environment latest_report_status))

(define (order-columns defined-order result-columns)
  (filter (lambda (field)
            (hash-has-key? result-columns field)) defined-order))

(define (max-list list-1 list-2)
  (map (lambda (x y)
         (max x y))
       list-1 list-2))

(define (safe-string-length s)
  (if (eq? s 'null)
      (string-length "null")
      (string-length s)))

(define (row->lengths column-order row)
  (map (lambda (column)
         (safe-string-length (hash-ref row column)))
       column-order))

(define (find-max-col-lengths rows)
  (let* ([keys (hash-keys (car rows))]
         [just-lengths (map (lambda (row)
                              (row->lengths keys row))
                            rows)])
    (apply hash (append-map list
                            keys
                            (foldl max-list
                                   (car just-lengths)
                                   (cdr just-lengths))))))

(define (header-row rows)
  (apply hash (append-map (lambda (col)
                            (list col (symbol->string col)))
                          (order-columns node-column-order (car rows)))))

(define (tabular-output rows)
  (let* ([rows (cons (header-row rows) rows)]
         [max-lengths (find-max-col-lengths rows)]
         [order (order-columns node-column-order (car rows))])
    (for-each (lambda (row)
                (displayln (apply string-append
                                  (map (lambda (col)
                                         (pad-column (hash-ref row col)
                                                     (hash-ref max-lengths col)))
                                       order))))
              rows)))

(define (query-host host-info)
  (let* [(url-struct (create-pdb-url host-info "/pdb/query/v4/nodes"))
         (ssl-context (if (https? url-struct)
                          (ssl-context host-info)
                          #f))]
    (call-with-values (lambda ()
                        (http-sendrecv (url-host url-struct)
                                       "/pdb/query/v4/nodes"
                                       #:ssl? ssl-context
                                       #:port (url-port url-struct)
                                       #:method "POST"
                                       #:data (jsexpr->string (hash 'query (query-string)))))
      (lambda (status headers body)
        (let* [(format (output-format))
               (output-fn (cond
                           [(eq? format 'json) (lambda (http-response-port)
                                                 (copy-port http-response-port (current-output-port))
                                                 (close-input-port http-response-port))]
                           [(lambda (http-response-port)
                              (tabular-output (read-json http-response-port))
                              (close-input-port http-response-port))]))]
          (output-fn body))))))

(define (test-it query)
  (query-string (string->jsexpr query))
  (output-format 'tab)
  (query-host (host-config)))

(define (nodes)
  (command-line
   #:program "nodes"
   #:once-any
   [("-q" "--query") query "PuppetDB query string" (query-string (string->jsexpr query))]
   [("-f" "--query-file") query-file
    "Path to a file containing a PuppetDB query string"
    (query-string (json-from-file query-file))]

   #:once-each
   [("-a" "--alias") a
    "Alias for the PuppetDB instance in ~/.pdbrc, uses \"default_host\" if not specified"
    (alias a)]

   #:once-any
   [("-j" "--json") "JSON output-format" (output-format 'json)]
   [("-t" "--tab") "Tabular output-format" (output-format 'tab)]))

(begin
  (nodes)
  (query-host (host-config)))
