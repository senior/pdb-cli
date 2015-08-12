(use test)
(load "../nodes.scm")


(define test-rows
  (list (alist->hash-table '((certname . "foo")
                             (deactivated . "bar")
                             (expired . "baz")))
        (alist->hash-table '((certname . "foofoo")
                             (deactivated . "barbar")
                             (expired . "bazbaz")))
        (alist->hash-table '((certname . "foofoofoo")
                             (deactivated . "barbarbar")
                             (expired . "bazbazbaz")))))

(test-group "tabular alignment"
            (test 3
                  (column-length (alist->hash-table '((a . "foo"))) 'a))

            (test 4                     ;<- length of "null"
                  (column-length (alist->hash-table '((a . "foo")
                                                      (b . null))) 'b))

            (test " foo  "
                  (pad-column "foo" 5))
            (test " foo"
                  (pad-column "foo" 3))
            (test " null  "
                  (pad-column 'null 6))

            (test '((certname . 3)
                    (deactivated . 3)
                    (expired . 3))
                  (max-col-length (car test-rows)
                                  '((certname . 0) (deactivated . 0) (expired . 0))))

            (test '((certname . 4)
                    (deactivated . 4)
                    (expired . 4))
                  (max-col-length (car test-rows)
                                  '((certname . 4) (deactivated . 4) (expired . 4))))

            (test '((certname . 9)
                    (deactivated . 9)
                    (expired . 9))
                  (find-max-col-lengths test-rows)))

(test "foo,bar,baz"
      (emit-csv-row '("foo" "bar" "baz")))

(test "foo,'bar',baz"
      (emit-csv-row '("foo" "'bar'" "baz")))

(test "foo,\"b,a,r\",baz"
      (emit-csv-row '("foo" "b,a,r" "baz")))

(test "foo,\"b\"\"a\"\"r\",baz"
      (emit-csv-row '("foo" "b\"a\"r" "baz")))

(test "foo,\"b\na\nr\",baz"
      (emit-csv-row '("foo" "b\na\nr" "baz")))
