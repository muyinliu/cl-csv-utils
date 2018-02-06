(in-package :cl-user)

(defpackage cl-csv-utils-test
  (:use :cl :prove))

(in-package :cl-csv-utils-test)

(plan nil)

(defvar *cl-csv-utils-pathname* (asdf:system-source-directory :cl-csv-utils))

(defvar *cl-csv-utils-test-pathname* (merge-pathnames "test/"
                                                      *cl-csv-utils-pathname*))

(defvar *csv-pathname*
  (merge-pathnames "source.csv"
                   *cl-csv-utils-test-pathname*))

(subtest "read-csv-file test"
  (is (csv-utils:read-csv-file *csv-pathname*)
      '(("field0" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4"))))

(defvar *csv* (csv-utils:read-csv-file *csv-pathname*))
(defvar *origin-csv* (copy-tree *csv*))

(subtest "write-csv-file test"
  (let ((output-pathname (merge-pathnames "test/output.csv"
                                          *cl-csv-utils-pathname*)))
    (csv-utils:write-csv-file *csv* output-pathname
                              :if-exists :supersede)
    (is (csv-utils:read-csv-file output-pathname)
        '(("field0" "field1" "field2" "field3" "field4")
          ("0.0" "0.1" "0.2" "0.3" "0.4")
          ("1.0" "1.1" "1.2" "1.3" "1.4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")))))

(subtest "ensure-field-index test"
  (is (csv-utils:ensure-field-index *csv* 0) 0)
  (is (csv-utils:ensure-field-index *csv* 1) 1)
  (is (csv-utils:ensure-field-index *csv* "field0") 0)
  (is (csv-utils:ensure-field-index *csv* "field1") 1)
  (is-error (csv-utils:ensure-field-index *csv* "field-not-exists") 'error))

(subtest "ensure-field-index-list test"
  (is (csv-utils:ensure-field-index-list *csv* '(0 1))
      '(0 1))
  (is (csv-utils:ensure-field-index-list *csv* '(0 2))
      '(0 2))
  (is (csv-utils:ensure-field-index-list *csv* '("field0" "field1"))
      '(0 1))
  (is (csv-utils:ensure-field-index-list *csv* '("field0" "field2"))
      '(0 2))
  (is-error (csv-utils:ensure-field-index-list *csv* '("field-not-exists"))
            'error))

(subtest "filter-rows test"
  (subtest "filter-rows test with field-index"
    (is (csv-utils:filter-rows *csv*
                               (list (cons 0
                                           #'(lambda (value)
                                               (> (parse-number:parse-number value)
                                                  1.0)))))
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")))
    (is (csv-utils:filter-rows *csv*
                               (list (cons 2
                                           #'(lambda (value)
                                               (> (parse-number:parse-number value)
                                                  1.0)))))
        '(("field0" "field1" "field2" "field3" "field4")
          ("1.0" "1.1" "1.2" "1.3" "1.4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")))
    (is (csv-utils:filter-rows *csv*
                               (list (cons 1
                                           #'(lambda (value)
                                               (> (parse-number:parse-number value)
                                                  1.1)))
                                     (cons 2
                                           #'(lambda (value)
                                               (> (parse-number:parse-number value)
                                                  1.0)))))
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")))
    (subtest "maker sure filter-rows will NOT change origin csv"
      (is *csv* *origin-csv*)))
  (subtest "filter-rows test with field-name"
    (is (csv-utils:filter-rows *csv*
                               (list (cons "field0"
                                           #'(lambda (value)
                                               (> (parse-number:parse-number value)
                                                  1.0)))))
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")))
    (subtest "maker sure filter-rows will NOT change origin csv"
      (is *csv* *origin-csv*))))

(subtest "filter-columns test"
  (subtest "filter-columns test with field-index"
    (is (csv-utils:filter-columns *csv* '(0 1 3))
        '(("field0" "field1" "field3")
          ("0.0" "0.1" "0.3")
          ("1.0" "1.1" "1.3")
          ("2.0" "2.1" "2.3")
          ("3.0" "3.1" "3.3")))
    (is (csv-utils:filter-columns *csv* '(0 1 2))
        '(("field0" "field1" "field2")
          ("0.0" "0.1" "0.2")
          ("1.0" "1.1" "1.2")
          ("2.0" "2.1" "2.2")
          ("3.0" "3.1" "3.2")))
    (is (csv-utils:filter-columns *csv* '(1 2 3))
        '(("field1" "field2" "field3")
          ("0.1" "0.2" "0.3")
          ("1.1" "1.2" "1.3")
          ("2.1" "2.2" "2.3")
          ("3.1" "3.2" "3.3")))
    (subtest "maker sure filter-columns will NOT change origin csv"
      (is *csv* *origin-csv*)))
  (subtest "filter-columns test with field-name"
    (is (csv-utils:filter-columns *csv* '("field0" "field1" "field3"))
        '(("field0" "field1" "field3")
          ("0.0" "0.1" "0.3")
          ("1.0" "1.1" "1.3")
          ("2.0" "2.1" "2.3")
          ("3.0" "3.1" "3.3")))
    (is (csv-utils:filter-columns *csv* '("field0" "field1" "field2"))
        '(("field0" "field1" "field2")
          ("0.0" "0.1" "0.2")
          ("1.0" "1.1" "1.2")
          ("2.0" "2.1" "2.2")
          ("3.0" "3.1" "3.2")))
    (is (csv-utils:filter-columns *csv* '("field1" "field2" "field3"))
        '(("field1" "field2" "field3")
          ("0.1" "0.2" "0.3")
          ("1.1" "1.2" "1.3")
          ("2.1" "2.2" "2.3")
          ("3.1" "3.2" "3.3")))
    (subtest "maker sure filter-columns will NOT change origin csv"
      (is *csv* *origin-csv*))))

(subtest "sort-rows test"
  (subtest "sort-rows test with field-index"
    (is (csv-utils:sort-rows *csv* 0 #'string>)
        '(("field0" "field1" "field2" "field3" "field4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("1.0" "1.1" "1.2" "1.3" "1.4")
          ("0.0" "0.1" "0.2" "0.3" "0.4")))
    (subtest "make sure sort-rows will NOT change origin csv"
      (is *csv* *origin-csv*))))

(subtest "insert-row test"
  (is (csv-utils:insert-row *csv* 0 '("-1.0" "-1.1" "-1.2" "-1.3" "-1.4"))
      '(("field0" "field1" "field2" "field3" "field4")
        ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (is (csv-utils:insert-row *csv* 1 '("-1.0" "-1.1" "-1.2" "-1.3" "-1.4"))
      '(("field0" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (subtest "make sure insert-row will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "insert-rows test"
  (is (csv-utils:insert-rows *csv* 0 '(("-2.0" "-2.1" "-2.2" "-2.3" "-2.4")
                                       ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")))
      '(("field0" "field1" "field2" "field3" "field4")
        ("-2.0" "-2.1" "-2.2" "-2.3" "-2.4")
        ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (is (csv-utils:insert-rows *csv* 1 '(("-2.0" "-2.1" "-2.2" "-2.3" "-2.4")
                                       ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")))
      '(("field0" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("-2.0" "-2.1" "-2.2" "-2.3" "-2.4")
        ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (subtest "make sure insert-rows will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "append-row test"
  (is (csv-utils:append-row *csv* '("4.0" "4.1" "4.2" "4.3" "4.4"))
      '(("field0" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")
        ("4.0" "4.1" "4.2" "4.3" "4.4")))
  (subtest "make sure append-row will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "append-rows test"
  (is (csv-utils:append-rows *csv* '(("4.0" "4.1" "4.2" "4.3" "4.4")
                                     ("5.0" "5.1" "5.2" "5.3" "5.4")))
      '(("field0" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")
        ("4.0" "4.1" "4.2" "4.3" "4.4")
        ("5.0" "5.1" "5.2" "5.3" "5.4")))
  (subtest "make sure append-rows will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "insert-column test"
  (subtest "insert-column test with row-index"
    (is (csv-utils:insert-column *csv* 0 "field-x")
        '(("field-x" "field0" "field1" "field2" "field3" "field4")
          ("1" "0.0" "0.1" "0.2" "0.3" "0.4")
          ("2" "1.0" "1.1" "1.2" "1.3" "1.4")
          ("3" "2.0" "2.1" "2.2" "2.3" "2.4")
          ("4" "3.0" "3.1" "3.2" "3.3" "3.4")))
    (is (csv-utils:insert-column *csv* 0 "field-x" :row-index-begin 2)
        '(("field-x" "field0" "field1" "field2" "field3" "field4")
          ("2" "0.0" "0.1" "0.2" "0.3" "0.4")
          ("3" "1.0" "1.1" "1.2" "1.3" "1.4")
          ("4" "2.0" "2.1" "2.2" "2.3" "2.4")
          ("5" "3.0" "3.1" "3.2" "3.3" "3.4")))
    (subtest "maker sure insert-column will NOT change origin csv"
      (is *csv* *origin-csv*)))
  (subtest "insert-column test with default-value"
    (is (csv-utils:insert-column *csv* 0 "field-x" :default-value "default-value")
        '(("field-x" "field0" "field1" "field2" "field3" "field4")
          ("default-value" "0.0" "0.1" "0.2" "0.3" "0.4")
          ("default-value" "1.0" "1.1" "1.2" "1.3" "1.4")
          ("default-value" "2.0" "2.1" "2.2" "2.3" "2.4")
          ("default-value" "3.0" "3.1" "3.2" "3.3" "3.4")))
    (subtest "maker sure insert-column will NOT change origin csv"
      (is *csv* *origin-csv*))))

(subtest "transform-column test"
  (is (csv-utils:transform-column *csv* 0 #'parse-number:parse-number)
      '(("field0" "field1" "field2" "field3" "field4")
        (0.0 "0.1" "0.2" "0.3" "0.4")
        (1.0 "1.1" "1.2" "1.3" "1.4")
        (2.0 "2.1" "2.2" "2.3" "2.4")
        (3.0 "3.1" "3.2" "3.3" "3.4")))
  (subtest "maker sure transform-column will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "transform-columns test"
  (is (csv-utils:transform-columns *csv*
                                   (list (cons 0 #'parse-number:parse-number)
                                         (cons 1 #'(lambda (value)
                                                     (concatenate 'string "-" value)))))
      '(("field0" "field1" "field2" "field3" "field4")
        (0.0 "-0.1" "0.2" "0.3" "0.4")
        (1.0 "-1.1" "1.2" "1.3" "1.4")
        (2.0 "-2.1" "2.2" "2.3" "2.4")
        (3.0 "-3.1" "3.2" "3.3" "3.4")))
  (subtest "maker sure transform-columns will NOT change origin csv"
    (is *csv* *origin-csv*)))
                                   

(subtest "remove-column test"
  (subtest "remove-column test with field-index"
    (is (csv-utils:remove-column *csv* 0)
        '(("field1" "field2" "field3" "field4")
          ("0.1" "0.2" "0.3" "0.4")
          ("1.1" "1.2" "1.3" "1.4")
          ("2.1" "2.2" "2.3" "2.4")
          ("3.1" "3.2" "3.3" "3.4"))))
  (subtest "remove-column test with field-name"
    (is (csv-utils:remove-column *csv* "field0")
        '(("field1" "field2" "field3" "field4")
          ("0.1" "0.2" "0.3" "0.4")
          ("1.1" "1.2" "1.3" "1.4")
          ("2.1" "2.2" "2.3" "2.4")
          ("3.1" "3.2" "3.3" "3.4"))))
  (subtest "make sure remove-column will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "remove-columns test"
  (subtest "remove-columns test with field-index-list"
    (is (csv-utils:remove-columns *csv* '(0))
        '(("field1" "field2" "field3" "field4")
          ("0.1" "0.2" "0.3" "0.4")
          ("1.1" "1.2" "1.3" "1.4")
          ("2.1" "2.2" "2.3" "2.4")
          ("3.1" "3.2" "3.3" "3.4")))
    (is (csv-utils:remove-columns *csv* '(0 1))
        '(("field2" "field3" "field4")
          ("0.2" "0.3" "0.4")
          ("1.2" "1.3" "1.4")
          ("2.2" "2.3" "2.4")
          ("3.2" "3.3" "3.4")))
    (is (csv-utils:remove-columns *csv* '(0 2))
        '(("field1" "field3" "field4")
          ("0.1" "0.3" "0.4")
          ("1.1" "1.3" "1.4")
          ("2.1" "2.3" "2.4")
          ("3.1" "3.3" "3.4"))))
  (subtest "remove-columns test with field-name-list"
    (is (csv-utils:remove-columns *csv* '("field0"))
        '(("field1" "field2" "field3" "field4")
          ("0.1" "0.2" "0.3" "0.4")
          ("1.1" "1.2" "1.3" "1.4")
          ("2.1" "2.2" "2.3" "2.4")
          ("3.1" "3.2" "3.3" "3.4")))
    (is (csv-utils:remove-columns *csv* '("field0" "field1"))
        '(("field2" "field3" "field4")
          ("0.2" "0.3" "0.4")
          ("1.2" "1.3" "1.4")
          ("2.2" "2.3" "2.4")
          ("3.2" "3.3" "3.4")))
    (is (csv-utils:remove-columns *csv* '("field0" "field2"))
        '(("field1" "field3" "field4")
          ("0.1" "0.3" "0.4")
          ("1.1" "1.3" "1.4")
          ("2.1" "2.3" "2.4")
          ("3.1" "3.3" "3.4"))))
  (subtest "make sure remove-columns will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "search-row-by-cell test"
  (subtest "search-row-by-cell test with searchword"
    (is (csv-utils:search-row-by-cell *csv* "2.2")
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4"))))
  (subtest "search-row-by-cell test with searchword(regex)"
    (is (csv-utils:search-row-by-cell *csv* "[23]\\.3" :regex-p t)
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4"))))
  (subtest "search-row-by-cell test with predicate"
    (is (csv-utils:search-row-by-cell *csv*
                                      #'(lambda (value)
                                          (> (parse-number:parse-number value)
                                             2.3)))
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4"))))
  (subtest "make sure search-row-by-cell will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "search-row-by-column test"
  (subtest "search-row-by-column test with field-name and searchword"
    (is (csv-utils:search-row-by-column *csv* "field2" "2.2")
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4"))))
  (subtest "search-row-by-column test with field-index and searchword"
    (is (csv-utils:search-row-by-column *csv* 2 "2.2")
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4"))))
  (subtest "search-row-by-column test with field-index and searchword(regex)"
    (is (csv-utils:search-row-by-column *csv* "field3" "[23]\\.3" :regex-p t)
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4"))))
  (subtest "search-row-by-column test with field-index and searchword(regex)"
    (is (csv-utils:search-row-by-column *csv* 3 "[23]\\.3" :regex-p t)
        '(("field0" "field1" "field2" "field3" "field4")
          ("2.0" "2.1" "2.2" "2.3" "2.4")
          ("3.0" "3.1" "3.2" "3.3" "3.4")))))

(subtest "rename-field-name test"
  (is (csv-utils:rename-field-name *csv* 0 "field-new-name")
      '(("field-new-name" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (is (csv-utils:rename-field-name *csv* "field0" "field-new-name")
      '(("field-new-name" "field1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (subtest "make sure rename-field-name will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "rename-field-names test"
  (is (csv-utils:rename-field-names *csv*
                                    (list (cons 0 "field-new-name-0")
                                          (cons 1 "field-new-name-1")))
      '(("field-new-name-0" "field-new-name-1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (is (csv-utils:rename-field-names *csv*
                                    (list (cons "field0" "field-new-name-0")
                                          (cons "field1" "field-new-name-1")))
      '(("field-new-name-0" "field-new-name-1" "field2" "field3" "field4")
        ("0.0" "0.1" "0.2" "0.3" "0.4")
        ("1.0" "1.1" "1.2" "1.3" "1.4")
        ("2.0" "2.1" "2.2" "2.3" "2.4")
        ("3.0" "3.1" "3.2" "3.3" "3.4")))
  (subtest "make sure rename-field-names will NOT change origin csv"
    (is *csv* *origin-csv*)))

(subtest "csv->markdown test"
  (subtest "csv->markdown test without beautify"
    (is (csv-utils:csv->markdown *csv* :beautify-p nil)
        "| field0 | field1 | field2 | field3 | field4 |
|-----|-----|-----|-----|-----|
| 0.0 | 0.1 | 0.2 | 0.3 | 0.4 |
| 1.0 | 1.1 | 1.2 | 1.3 | 1.4 |
| 2.0 | 2.1 | 2.2 | 2.3 | 2.4 |
| 3.0 | 3.1 | 3.2 | 3.3 | 3.4 |
"))
  (subtest "csv->markdown test without beautify or split header and body"
    (is (csv-utils:csv->markdown *csv*
                                 :beautify-p nil
                                 :split-header-body-p nil)
        "| field0 | field1 | field2 | field3 | field4 |
| 0.0 | 0.1 | 0.2 | 0.3 | 0.4 |
| 1.0 | 1.1 | 1.2 | 1.3 | 1.4 |
| 2.0 | 2.1 | 2.2 | 2.3 | 2.4 |
| 3.0 | 3.1 | 3.2 | 3.3 | 3.4 |
"))
  (subtest "csv->markdown test with beautify and align by field-index"
    (is (csv-utils:csv->markdown *csv*
                                 :beautify-p t
                                 :align-alist '((1 . :right)
                                                (2 . :center)))
        "| field0 | field1 | field2 | field3 | field4 |
|--------|-------:|:------:|--------|--------|
| 0.0    |    0.1 |  0.2   | 0.3    | 0.4    |
| 1.0    |    1.1 |  1.2   | 1.3    | 1.4    |
| 2.0    |    2.1 |  2.2   | 2.3    | 2.4    |
| 3.0    |    3.1 |  3.2   | 3.3    | 3.4    |
"))
  (subtest "csv->markdown test with beautify and align by field-index but without aplit header and body"
    (is (csv-utils:csv->markdown *csv*
                                 :align-alist '((1 . :right)
                                                (2 . :center))
                                 :split-header-body-p nil)
        "| field0 | field1 | field2 | field3 | field4 |
| 0.0    |    0.1 |  0.2   | 0.3    | 0.4    |
| 1.0    |    1.1 |  1.2   | 1.3    | 1.4    |
| 2.0    |    2.1 |  2.2   | 2.3    | 2.4    |
| 3.0    |    3.1 |  3.2   | 3.3    | 3.4    |
"))
  (subtest "csv->markdown test with beautify and align by field-name"
    (is (csv-utils:csv->markdown *csv*
                                 :align-alist '(("field1" . :right)
                                                ("field2" . :center)))
        "| field0 | field1 | field2 | field3 | field4 |
|--------|-------:|:------:|--------|--------|
| 0.0    |    0.1 |  0.2   | 0.3    | 0.4    |
| 1.0    |    1.1 |  1.2   | 1.3    | 1.4    |
| 2.0    |    2.1 |  2.2   | 2.3    | 2.4    |
| 3.0    |    3.1 |  3.2   | 3.3    | 3.4    |
"))
  (subtest "csv->markdown test with beautify about header align(center)"
    (is (csv-utils:csv->markdown '(("0" "1" "2" "3" "4")
                                   ("0.0" "0.1" "0.2" "0.3" "0.4")
                                   ("1.0" "1.1" "1.2" "1.3" "1.4")
                                   ("2.0" "2.1" "2.2" "2.3" "2.4")
                                   ("3.0" "3.1" "3.2" "3.3" "3.4")))
        "|  0  |  1  |  2  |  3  |  4  |
|-----|-----|-----|-----|-----|
| 0.0 | 0.1 | 0.2 | 0.3 | 0.4 |
| 1.0 | 1.1 | 1.2 | 1.3 | 1.4 |
| 2.0 | 2.1 | 2.2 | 2.3 | 2.4 |
| 3.0 | 3.1 | 3.2 | 3.3 | 3.4 |
")
    (is (csv-utils:csv->markdown '(("0" "1" "2" "3" "4")
                                   ("0.0" "0.1" "0.2" "0.3" "0.4")
                                   ("1.0" "1.1" "1.2" "1.3" "1.4")
                                   ("2.0" "2.1" "2.2" "2.3" "2.4")
                                   ("3.0" "3.1" "3.2" "3.3" "3.4"))
                                 :align-alist '(("1" . :right)
                                                ("2" . :center)))
        "|  0  |  1  |  2  |  3  |  4  |
|-----|----:|:---:|-----|-----|
| 0.0 | 0.1 | 0.2 | 0.3 | 0.4 |
| 1.0 | 1.1 | 1.2 | 1.3 | 1.4 |
| 2.0 | 2.1 | 2.2 | 2.3 | 2.4 |
| 3.0 | 3.1 | 3.2 | 3.3 | 3.4 |
")
    (is (csv-utils:csv->markdown '(("0" "1" "2" "3" "4")
                                   ("0.0000" "0.1" "0.2" "0.3" "0.4")
                                   ("1.000" "01.1" "01.2" "1.3" "1.4")
                                   ("2.00" "002.1" "2.20" "2.3" "2.4")
                                   ("3.0" "0003.1" "03.20" "3.3" "3.4"))
                                 :align-alist '(("1" . :right)
                                                ("2" . :center)))
        "|   0    |   1    |   2   |  3  |  4  |
|--------|-------:|:-----:|-----|-----|
| 0.0000 |    0.1 |  0.2  | 0.3 | 0.4 |
| 1.000  |   01.1 | 01.2  | 1.3 | 1.4 |
| 2.00   |  002.1 | 2.20  | 2.3 | 2.4 |
| 3.0    | 0003.1 | 03.20 | 3.3 | 3.4 |
")))

(finalize)
