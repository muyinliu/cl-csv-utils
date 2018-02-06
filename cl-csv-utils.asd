(defsystem "cl-csv-utils"
  :name "cl-csv-utils"
  :description "Utilities for cl-csv"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("cl-csv" "cl-ppcre")
  :in-order-to ((test-op (test-op "cl-csv-utils-test")))
  :serial t
  :components ((:file "packages")
               (:file "cl-csv-utils")))
