(use pathname-expand)
(use http-client)
(use medea)
(use openssl)

(define (alist->ht alist)
  (alist->hash-table alist eq? symbol-hash))

(json-parsers
 (alist-update 'object
               alist->ht
               (json-parsers)))

(define (acdr k alist)
  (let ((val (assoc k alist)))
    (if val
        (cdr val)
        #f)))

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

(define (maybe-set-ssl-context conn-info)
  (when (eq? 'https (uri-scheme (uri-reference (hash-table-ref conn-info 'root_url))))
    (let ((ssl-ctx (make-ssl-context/client-cert (hash-table-ref conn-info 'ca)
                                                 (hash-table-ref conn-info 'cert)
                                                 (hash-table-ref conn-info 'key))))
      (server-connector (make-ssl-server-connector/context ssl-ctx)))))

(define (get-default-conn-info args)
  (let* ((config (user-config))
         (default (if (assoc 'alias args)
                      (acdr 'alias args)
                      (hash-table-ref config 'default_host)))
         (host-list (hash-table-ref config 'hosts)))
    (hash-table-ref host-list (string->symbol default))))

(define (archive-uri root-uri)
  (uri-reference
   (string-append root-uri "/pdb/admin/v1/archive")))
