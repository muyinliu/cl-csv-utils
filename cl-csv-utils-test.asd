(defsystem "cl-csv-utils-test"
  :name "cl-csv-utils-test"
  :description "test case for cl-csv-utils"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :depends-on ("cl-csv-utils"
               "prove"
               "parse-number")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module "test"
                        :serial t
                        :components ((:file "cl-csv-utils-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
