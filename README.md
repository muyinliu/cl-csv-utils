# cl-csv-utils - Utilities of CSV for Common Lisp

## Install

In Shell:

```shell
git clone https://github.com/muyinliu/cl-csv-utils
cp -r cl-csv-utils ~/quicklisp/local-projects/cl-csv-utils
```

In Common Lisp REPL:

```lisp
(ql:quickload 'cl-csv-utils)
```
=>
```=>
To load "cl-csv-utils":
  Load 1 ASDF system:
    cl-csv-utils
; Loading "cl-csv-utils"
[package cl-csv-utils].....
(CL-CSV-UTILS)
```

Note: nickname of package `cl-csv-utils` is `csv-utils`

## Dependencies

* [cl-csv](https://github.com/AccelerationNet/cl-csv)
* [cl-ppcre](http://www.weitz.de/cl-ppcre/)
* [prove](https://github.com/fukamachi/prove) (required by the test cases)

## Usage

### read-csv-file filespec &key (external-format :utf-8)

Read CSV as list from file(pathspec).

```lisp
(csv-utils:read-csv-file (merge-pathnames "test/source.csv"
                                          (asdf:system-source-directory :cl-csv-utils)))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
 ```

### write-csv-file csv filespec &key (external-format :utf-8) (if-exists :error)

```lisp
(csv-utils:write-csv-file '(("field0" "field1" "field2" "field3" "field4")
                            ("0.0" "0.1" "0.2" "0.3" "0.4")
                            ("1.0" "1.1" "1.2" "1.3" "1.4")
                            ("2.0" "2.1" "2.2" "2.3" "2.4")
                            ("3.0" "3.1" "3.2" "3.3" "3.4"))
                          (merge-pathnames
                           "test/output.csv"
                           (asdf:system-source-directory :cl-csv-utils))
                          :if-exists :supersede)
```
=>
```=>
#P"/Users/muyinliu/quicklisp/local-projects/cl-csv-utils/test/output.csv"
```

### ensure-field-index csv field-name-or-index



```lisp
(csv-utils:ensure-field-index '(("field0" "field1" "field2" "field3" "field4")
                                ("0.0" "0.1" "0.2" "0.3" "0.4")
                                ("1.0" "1.1" "1.2" "1.3" "1.4")
                                ("2.0" "2.1" "2.2" "2.3" "2.4")
                                ("3.0" "3.1" "3.2" "3.3" "3.4"))
                              0)
```
=>
```=>
0
```

```lisp
(csv-utils:ensure-field-index '(("field0" "field1" "field2" "field3" "field4")
                                ("0.0" "0.1" "0.2" "0.3" "0.4")
                                ("1.0" "1.1" "1.2" "1.3" "1.4")
                                ("2.0" "2.1" "2.2" "2.3" "2.4")
                                ("3.0" "3.1" "3.2" "3.3" "3.4"))
                              "field2")
```
=>
```=>
2
```

```lisp
(csv-utils:ensure-field-index '(("field0" "field1" "field2" "field3" "field4")
                                ("0.0" "0.1" "0.2" "0.3" "0.4")
                                ("1.0" "1.1" "1.2" "1.3" "1.4")
                                ("2.0" "2.1" "2.2" "2.3" "2.4")
                                ("3.0" "3.1" "3.2" "3.3" "3.4"))
                              "not-exists")
```
=>
```=>
(AND CL-CSV-UTILS::FIELD-INDEX
     (< -1 CL-CSV-UTILS::FIELD-NAME-OR-INDEX
        (LENGTH (CAR CL-CSV-UTILS::CSV))))
failed.
   [Condition of type SIMPLE-ERROR]
   ```

### ensure-field-index-list csv field-name-or-index-list

```lisp
(csv-utils:ensure-field-index-list '(("field0" "field1" "field2" "field3" "field4")
                                     ("0.0" "0.1" "0.2" "0.3" "0.4")
                                     ("1.0" "1.1" "1.2" "1.3" "1.4")
                                     ("2.0" "2.1" "2.2" "2.3" "2.4")
                                     ("3.0" "3.1" "3.2" "3.3" "3.4"))
                                   '(0 2))
```
=>
```=>
(0 2)
```

```lisp
(csv-utils:ensure-field-index-list '(("field0" "field1" "field2" "field3" "field4")
                                     ("0.0" "0.1" "0.2" "0.3" "0.4")
                                     ("1.0" "1.1" "1.2" "1.3" "1.4")
                                     ("2.0" "2.1" "2.2" "2.3" "2.4")
                                     ("3.0" "3.1" "3.2" "3.3" "3.4"))
                                   '("field0" "field2"))
```
=>
```=>
(0 2)
```

### filter-rows csv field-name-or-index-predicate-alist &key (predicate #'every)

```lisp
(csv-utils:filter-rows '(("field0" "field1" "field2" "field3" "field4")
                         ("0.0" "0.1" "0.2" "0.3" "0.4")
                         ("1.0" "1.1" "1.2" "1.3" "1.4")
                         ("2.0" "2.1" "2.2" "2.3" "2.4")
                         ("3.0" "3.1" "3.2" "3.3" "3.4"))
                       (list (cons 0
                                   #'(lambda (value)
                                       (> (parse-number:parse-number value)
                                          1.0)))))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

Note: require system `parse-number`, load it by `(ql:quickload 'parse-number)`

```lisp
(csv-utils:filter-rows '(("field0" "field1" "field2" "field3" "field4")
                         ("0.0" "0.1" "0.2" "0.3" "0.4")
                         ("1.0" "1.1" "1.2" "1.3" "1.4")
                         ("2.0" "2.1" "2.2" "2.3" "2.4")
                         ("3.0" "3.1" "3.2" "3.3" "3.4"))
                       (list (cons "field0"
                                   #'(lambda (value)
                                       (> (parse-number:parse-number value)
                                          1.0)))))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

### filter-columns csv field-name-or-index-list

```lisp
(csv-utils:filter-columns '(("field0" "field1" "field2" "field3" "field4")
                            ("0.0" "0.1" "0.2" "0.3" "0.4")
                            ("1.0" "1.1" "1.2" "1.3" "1.4")
                            ("2.0" "2.1" "2.2" "2.3" "2.4")
                            ("3.0" "3.1" "3.2" "3.3" "3.4"))
                          '(0 1 3))
```
=>
```=>
(("field0" "field1" "field3")
 ("0.0" "0.1" "0.3")
 ("1.0" "1.1" "1.3")
 ("2.0" "2.1" "2.3")
 ("3.0" "3.1" "3.3"))
```

```lisp
(csv-utils:filter-columns '(("field0" "field1" "field2" "field3" "field4")
                            ("0.0" "0.1" "0.2" "0.3" "0.4")
                            ("1.0" "1.1" "1.2" "1.3" "1.4")
                            ("2.0" "2.1" "2.2" "2.3" "2.4")
                            ("3.0" "3.1" "3.2" "3.3" "3.4"))
                          '("field0" "field1" "field3"))

(("field0" "field1" "field3")
 ("0.0" "0.1" "0.3")
 ("1.0" "1.1" "1.3")
 ("2.0" "2.1" "2.3")
 ("3.0" "3.1" "3.3"))
```

### sort-rows csv field-name-or-index predicate

```lisp
(csv-utils:sort-rows '(("field0" "field1" "field2" "field3" "field4")
                       ("0.0" "0.1" "0.2" "0.3" "0.4")
                       ("1.0" "1.1" "1.2" "1.3" "1.4")
                       ("2.0" "2.1" "2.2" "2.3" "2.4")
                       ("3.0" "3.1" "3.2" "3.3" "3.4"))
                     0
                     #'string>)
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("3.0" "3.1" "3.2" "3.3" "3.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("0.0" "0.1" "0.2" "0.3" "0.4"))
```

### insert-row csv index row

```lisp
(csv-utils:insert-row '(("field0" "field1" "field2" "field3" "field4")
                        ("0.0" "0.1" "0.2" "0.3" "0.4")
                        ("1.0" "1.1" "1.2" "1.3" "1.4")
                        ("2.0" "2.1" "2.2" "2.3" "2.4")
                        ("3.0" "3.1" "3.2" "3.3" "3.4"))
                      1
                      '("-1.0" "-1.1" "-1.2" "-1.3" "-1.4"))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

### insert-rows csv index row-list

```lisp
(csv-utils:insert-rows '(("field0" "field1" "field2" "field3" "field4")
                         ("0.0" "0.1" "0.2" "0.3" "0.4")
                         ("1.0" "1.1" "1.2" "1.3" "1.4")
                         ("2.0" "2.1" "2.2" "2.3" "2.4")
                         ("3.0" "3.1" "3.2" "3.3" "3.4"))
                       0
                       '(("-2.0" "-2.1" "-2.2" "-2.3" "-2.4")
                         ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("-2.0" "-2.1" "-2.2" "-2.3" "-2.4")
 ("-1.0" "-1.1" "-1.2" "-1.3" "-1.4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

### append-row csv row

```lisp
(csv-utils:append-row '(("field0" "field1" "field2" "field3" "field4")
                        ("0.0" "0.1" "0.2" "0.3" "0.4")
                        ("1.0" "1.1" "1.2" "1.3" "1.4")
                        ("2.0" "2.1" "2.2" "2.3" "2.4")
                        ("3.0" "3.1" "3.2" "3.3" "3.4"))
                      '("4.0" "4.1" "4.2" "4.3" "4.4"))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4")
 ("4.0" "4.1" "4.2" "4.3" "4.4"))
```

### append-rows csv rows

```lisp
(csv-utils:append-rows '(("field0" "field1" "field2" "field3" "field4")
                         ("0.0" "0.1" "0.2" "0.3" "0.4")
                         ("1.0" "1.1" "1.2" "1.3" "1.4")
                         ("2.0" "2.1" "2.2" "2.3" "2.4")
                         ("3.0" "3.1" "3.2" "3.3" "3.4"))
                       '(("4.0" "4.1" "4.2" "4.3" "4.4")
                         ("5.0" "5.1" "5.2" "5.3" "5.4")))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4")
 ("4.0" "4.1" "4.2" "4.3" "4.4")
 ("5.0" "5.1" "5.2" "5.3" "5.4"))
```

### insert-column csv index field-name &key (default-value :row-index) (row-index-begin 1)

```lisp
(csv-utils:insert-column '(("field0" "field1" "field2" "field3" "field4")
                           ("0.0" "0.1" "0.2" "0.3" "0.4")
                           ("1.0" "1.1" "1.2" "1.3" "1.4")
                           ("2.0" "2.1" "2.2" "2.3" "2.4")
                           ("3.0" "3.1" "3.2" "3.3" "3.4"))
                         0
                         "field-x")
```
=>
```=>
(("field-x" "field0" "field1" "field2" "field3" "field4")
 ("1" "0.0" "0.1" "0.2" "0.3" "0.4")
 ("2" "1.0" "1.1" "1.2" "1.3" "1.4")
 ("3" "2.0" "2.1" "2.2" "2.3" "2.4")
 ("4" "3.0" "3.1" "3.2" "3.3" "3.4"))
```

```lisp
(csv-utils:insert-column '(("field0" "field1" "field2" "field3" "field4")
                           ("0.0" "0.1" "0.2" "0.3" "0.4")
                           ("1.0" "1.1" "1.2" "1.3" "1.4")
                           ("2.0" "2.1" "2.2" "2.3" "2.4")
                           ("3.0" "3.1" "3.2" "3.3" "3.4"))
                         0
                         "field-x"
                         :row-index-begin 2)
```
=>
```=>
(("field-x" "field0" "field1" "field2" "field3" "field4")
 ("2" "0.0" "0.1" "0.2" "0.3" "0.4")
 ("3" "1.0" "1.1" "1.2" "1.3" "1.4")
 ("4" "2.0" "2.1" "2.2" "2.3" "2.4")
 ("5" "3.0" "3.1" "3.2" "3.3" "3.4"))
```

```lisp
(csv-utils:insert-column '(("field-x" "field0" "field1" "field2" "field3" "field4")
                           ("default-value" "0.0" "0.1" "0.2" "0.3" "0.4")
                           ("default-value" "1.0" "1.1" "1.2" "1.3" "1.4")
                           ("default-value" "2.0" "2.1" "2.2" "2.3" "2.4")
                           ("default-value" "3.0" "3.1" "3.2" "3.3" "3.4"))
                         0
                         "field-x"
                         :default-value "default-value")
```
=>
```=>
(("field-x" "field0" "field1" "field2" "field3" "field4")
 ("default-value" "0.0" "0.1" "0.2" "0.3" "0.4")
 ("default-value" "1.0" "1.1" "1.2" "1.3" "1.4")
 ("default-value" "2.0" "2.1" "2.2" "2.3" "2.4")
 ("default-value" "3.0" "3.1" "3.2" "3.3" "3.4"))
```

### transform-column csv field-name-or-index function

```lisp
(csv-utils:transform-column '(("field0" "field1" "field2" "field3" "field4")
                              ("0.0" "0.1" "0.2" "0.3" "0.4")
                              ("1.0" "1.1" "1.2" "1.3" "1.4")
                              ("2.0" "2.1" "2.2" "2.3" "2.4")
                              ("3.0" "3.1" "3.2" "3.3" "3.4"))
                            0
                            #'parse-number:parse-number)
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 (0.0 "0.1" "0.2" "0.3" "0.4")
 (1.0 "1.1" "1.2" "1.3" "1.4")
 (2.0 "2.1" "2.2" "2.3" "2.4")
 (3.0 "3.1" "3.2" "3.3" "3.4"))
```

Note: require system `parse-number`, load it by `(ql:quickload 'parse-number)`

### transform-columns csv field-name-or-index-function-alist

```lisp
(csv-utils:transform-columns '(("field0" "field1" "field2" "field3" "field4")
                               ("0.0" "0.1" "0.2" "0.3" "0.4")
                               ("1.0" "1.1" "1.2" "1.3" "1.4")
                               ("2.0" "2.1" "2.2" "2.3" "2.4")
                               ("3.0" "3.1" "3.2" "3.3" "3.4"))
                             (list (cons 0 #'parse-number:parse-number)
                                   (cons 1 #'(lambda (value)
                                               (concatenate 'string "-" value)))))
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 (0.0 "-0.1" "0.2" "0.3" "0.4")
 (1.0 "-1.1" "1.2" "1.3" "1.4")
 (2.0 "-2.1" "2.2" "2.3" "2.4")
 (3.0 "-3.1" "3.2" "3.3" "3.4"))
```

Note: require system `parse-number`, load it by `(ql:quickload 'parse-number)`

### remove-column csv field-name-or-index

```lisp
(csv-utils:remove-column '(("field0" "field1" "field2" "field3" "field4")
                           ("0.0" "0.1" "0.2" "0.3" "0.4")
                           ("1.0" "1.1" "1.2" "1.3" "1.4")
                           ("2.0" "2.1" "2.2" "2.3" "2.4")
                           ("3.0" "3.1" "3.2" "3.3" "3.4"))
                         0)
```
=>
```=>
(("field1" "field2" "field3" "field4")
 ("0.1" "0.2" "0.3" "0.4")
 ("1.1" "1.2" "1.3" "1.4")
 ("2.1" "2.2" "2.3" "2.4")
 ("3.1" "3.2" "3.3" "3.4"))
```

```lisp
(csv-utils:remove-column '(("field0" "field1" "field2" "field3" "field4")
                           ("0.0" "0.1" "0.2" "0.3" "0.4")
                           ("1.0" "1.1" "1.2" "1.3" "1.4")
                           ("2.0" "2.1" "2.2" "2.3" "2.4")
                           ("3.0" "3.1" "3.2" "3.3" "3.4"))
                         "field0")
```
=>
```=>
(("field1" "field2" "field3" "field4")
 ("0.1" "0.2" "0.3" "0.4")
 ("1.1" "1.2" "1.3" "1.4")
 ("2.1" "2.2" "2.3" "2.4")
 ("3.1" "3.2" "3.3" "3.4"))
```

### remove-columns csv field-name-or-index-list

```lisp
(csv-utils:remove-columns '(("field0" "field1" "field2" "field3" "field4")
                            ("0.0" "0.1" "0.2" "0.3" "0.4")
                            ("1.0" "1.1" "1.2" "1.3" "1.4")
                            ("2.0" "2.1" "2.2" "2.3" "2.4")
                            ("3.0" "3.1" "3.2" "3.3" "3.4"))
                          '(0 2))
```
=>
```=>
(("field1" "field3" "field4")
 ("0.1" "0.3" "0.4")
 ("1.1" "1.3" "1.4")
 ("2.1" "2.3" "2.4")
 ("3.1" "3.3" "3.4"))
```

```lisp
(csv-utils:remove-columns '(("field0" "field1" "field2" "field3" "field4")
                            ("0.0" "0.1" "0.2" "0.3" "0.4")
                            ("1.0" "1.1" "1.2" "1.3" "1.4")
                            ("2.0" "2.1" "2.2" "2.3" "2.4")
                            ("3.0" "3.1" "3.2" "3.3" "3.4"))
                          '("field0" "field1"))
```
=>
```=>
(("field2" "field3" "field4")
 ("0.2" "0.3" "0.4")
 ("1.2" "1.3" "1.4")
 ("2.2" "2.3" "2.4")
 ("3.2" "3.3" "3.4"))
```

### search-row-by-cell csv searchword-or-predicate &key regex-p

```lisp
(csv-utils:search-row-by-cell '(("field0" "field1" "field2" "field3" "field4")
                                ("0.0" "0.1" "0.2" "0.3" "0.4")
                                ("1.0" "1.1" "1.2" "1.3" "1.4")
                                ("2.0" "2.1" "2.2" "2.3" "2.4")
                                ("3.0" "3.1" "3.2" "3.3" "3.4"))
                              "2.2")
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("2.0" "2.1" "2.2" "2.3" "2.4"))
```

```lisp
(csv-utils:search-row-by-cell '(("field0" "field1" "field2" "field3" "field4")
                                ("0.0" "0.1" "0.2" "0.3" "0.4")
                                ("1.0" "1.1" "1.2" "1.3" "1.4")
                                ("2.0" "2.1" "2.2" "2.3" "2.4")
                                ("3.0" "3.1" "3.2" "3.3" "3.4"))
                              "[23]\\.3" :regex-p t)
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

### search-row-by-column csv field-name-or-index searchword-or-predicate &key regex-p

```lisp
(csv-utils:search-row-by-column '(("field0" "field1" "field2" "field3" "field4")
                                  ("0.0" "0.1" "0.2" "0.3" "0.4")
                                  ("1.0" "1.1" "1.2" "1.3" "1.4")
                                  ("2.0" "2.1" "2.2" "2.3" "2.4")
                                  ("3.0" "3.1" "3.2" "3.3" "3.4"))
                                "field2"
                                "2.2")
```
=>
```=>
(("field0" "field1" "field2" "field3" "field4")
 ("2.0" "2.1" "2.2" "2.3" "2.4"))
```

### rename-field-name csv old-field-name-or-index new-field-name

```lisp
(csv-utils:rename-field-name '(("field0" "field1" "field2" "field3" "field4")
                               ("0.0" "0.1" "0.2" "0.3" "0.4")
                               ("1.0" "1.1" "1.2" "1.3" "1.4")
                               ("2.0" "2.1" "2.2" "2.3" "2.4")
                               ("3.0" "3.1" "3.2" "3.3" "3.4"))
                             0
                             "field-new-name")
```
=>
```=>
(("field-new-name" "field1" "field2" "field3" "field4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

### rename-field-names csv old-field-name-or-index-new-field-name-alist

```lisp
(csv-utils:rename-field-names '(("field0" "field1" "field2" "field3" "field4")
                                ("0.0" "0.1" "0.2" "0.3" "0.4")
                                ("1.0" "1.1" "1.2" "1.3" "1.4")
                                ("2.0" "2.1" "2.2" "2.3" "2.4")
                                ("3.0" "3.1" "3.2" "3.3" "3.4"))
                              (list (cons "field0" "field-new-name-0")
                                    (cons "field1" "field-new-name-1")))
```
=>
```=>
(("field-new-name-0" "field-new-name-1" "field2" "field3" "field4")
 ("0.0" "0.1" "0.2" "0.3" "0.4")
 ("1.0" "1.1" "1.2" "1.3" "1.4")
 ("2.0" "2.1" "2.2" "2.3" "2.4")
 ("3.0" "3.1" "3.2" "3.3" "3.4"))
```

### csv->markdown csv &key (beautify-p t) align-alist (split-header-body-p t)

```lisp
(csv-utils:csv->markdown '(("0" "1" "2" "3" "4")
                           ("0.0000" "0.1" "0.2" "0.3" "0.4")
                           ("1.000" "01.1" "01.2" "1.3" "1.4")
                           ("2.00" "002.1" "2.20" "2.3" "2.4")
                           ("3.0" "0003.1" "03.20" "3.3" "3.4"))
                         :align-alist '(("1" . :right)
                                        ("2" . :center)))
```
=>
```=>
"|   0    |   1    |   2   |  3  |  4  |
|--------|-------:|:-----:|-----|-----|
| 0.0000 |    0.1 |  0.2  | 0.3 | 0.4 |
| 1.000  |   01.1 | 01.2  | 1.3 | 1.4 |
| 2.00   |  002.1 | 2.20  | 2.3 | 2.4 |
| 3.0    | 0003.1 | 03.20 | 3.3 | 3.4 |
"
```

### More examples

More examples can be found in `test/cl-csv-utils-test.lisp`


## Run test case

Make sure system [prove](https://github.com/fukamachi/prove) is
already installed. You can install it with `(ql:quickload 'prove)`

```shell
sbcl --noinform --eval "(asdf:test-system :cl-csv-utils)" --eval "(quit)"
```

## License

MIT (See LICENSE file for details).
