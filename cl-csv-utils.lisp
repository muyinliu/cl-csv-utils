(in-package :cl-csv-utils)

;;; list utilities

(defmacro insert (list index value)
  `(if (or (zerop ,index)
           (null ,list))
       (push ,value ,list)
       (progn
         (push ,value (cdr (nthcdr (1- ,index) ,list)))
         ,list)))

(defmacro delete-index (list index)
  `(when ,list
     (if (zerop ,index)
         (pop ,list)
         (pop (cdr (nthcdr (1- ,index) ,list))))
     ,list))

;;; I/O utilities

(defun read-csv-file (filespec &key (external-format :utf-8))
  (with-open-file (stream filespec :external-format external-format)
    (cl-csv:read-csv stream)))

(defun write-csv-file (csv filespec &key (external-format :utf-8)
                                      (if-exists :error))
  (with-open-file (stream filespec
                          :direction :output
                          :external-format external-format
                          :if-exists if-exists)
    (cl-csv:write-csv csv :stream stream))
  filespec)

;;; field-index utilities

(defun ensure-field-index (csv field-name-or-index)
  (assert (or (integerp field-name-or-index)
              (stringp field-name-or-index)))
  (let ((field-index
         (if (integerp field-name-or-index)
             field-name-or-index
             (setf field-name-or-index (position field-name-or-index (car csv)
                                                 :test #'equal)))))
    (assert (and field-index
                 (< -1 field-name-or-index (length (car csv)))))
    field-index))

(defun ensure-field-index-list (csv field-name-or-index-list)
  (assert (and (listp field-name-or-index-list)
               (or (every #'integerp field-name-or-index-list)
                   (every #'stringp field-name-or-index-list))))
  (if (every #'integerp field-name-or-index-list)
      field-name-or-index-list
      (mapcar #'(lambda (field-name-or-index)
                  (ensure-field-index csv field-name-or-index))
              field-name-or-index-list)))

;;; filter utilities

(defun filter-rows (csv field-name-or-index-predicate-alist &key (predicate #'every))
  (assert (and (listp field-name-or-index-predicate-alist)
               (or (every #'stringp (mapcar #'car
                                            field-name-or-index-predicate-alist))
                   (every #'integerp (mapcar #'car
                                             field-name-or-index-predicate-alist)))
               (every #'functionp (mapcar #'cdr
                                          field-name-or-index-predicate-alist))))
  (let* ((field-index-list (ensure-field-index-list
                            csv
                            (mapcar #'car
                                    field-name-or-index-predicate-alist)))
         (predicate-list (mapcar #'cdr field-name-or-index-predicate-alist))
         (result
          (loop for row in (cdr csv)
             when (funcall
                   predicate
                   #'(lambda (v) (not (null v)))
                   (mapcar #'(lambda (field-index predicate)
                               (funcall predicate
                                        (nth field-index row)))
                           field-index-list
                           predicate-list))
             collect row)))
    (push (car csv) result)))

(defun filter-columns (csv field-name-or-index-list)
  (loop
     with field-index-list = (ensure-field-index-list csv
                                                      field-name-or-index-list)
     for row in csv
     collect (mapcar #'(lambda (field-index)
                         (nth field-index row))
                     field-index-list)))

;;; sort utilities

(defun sort-rows (csv field-name-or-index predicate)
  (let* ((field-index (ensure-field-index csv field-name-or-index))
         (result (sort (copy-tree (cdr csv))
                       predicate
                       :key #'(lambda (row)
                                (nth field-index row)))))
    (push (car csv) result)))

;;; insert utilities

(defun insert-row (csv index row)
  (let ((rows-without-header (cdr csv)))
    (append (list (car csv))
            (subseq rows-without-header 0 index)
            (list row)
            (subseq rows-without-header index))))

(defun insert-rows (csv index row-list)
  (let ((rows-without-header (cdr csv)))
    (append (list (car csv))
            (subseq rows-without-header 0 index)
            row-list
            (subseq rows-without-header index))))

(defun append-row (csv row)
  (append csv
          (list row)))

(defun append-rows (csv rows)
  (append csv rows))

(defun insert-column (csv index field-name &key (default-value :row-index)
                                             (row-index-begin 1))
  (let ((header (car csv))
        (result (if (eql default-value :row-index)
                    (loop
                       with row-index = row-index-begin
                       for row in (cdr csv)
                       collect (insert row index (write-to-string row-index))
                       do (incf row-index))
                    (let ((default-value (if (eql default-value :empty)
                                             ""
                                             default-value)))
                      (loop
                         for row in (cdr csv)
                         collect (insert row index default-value))))))
    (push (insert header index field-name)
          result)))

;;; transform utilities

(defun transform-column (csv field-name-or-index function)
  (let ((result
         (loop
            with field-index = (ensure-field-index csv field-name-or-index)
            for row in (copy-tree (cdr csv))
            collect (progn
                      (setf (nth field-index row)
                            (funcall function (nth field-index row)))
                      row))))
    (push (car csv) result)))

(defun transform-columns (csv field-name-or-index-function-alist)
  (assert (and (listp field-name-or-index-function-alist)
               (or (every #'stringp (mapcar #'car
                                            field-name-or-index-function-alist))
                   (every #'integerp (mapcar #'car
                                             field-name-or-index-function-alist)))
               (every #'functionp (mapcar #'cdr
                                          field-name-or-index-function-alist))))
  (let ((result
         (loop
            with field-index-list = (ensure-field-index-list
                                     csv
                                     (mapcar #'car field-name-or-index-function-alist))
            with function-list = (mapcar #'cdr field-name-or-index-function-alist)
            for row in (copy-tree (cdr csv))
            collect (progn
                      (mapcar #'(lambda (field-index function)
                                  (setf (nth field-index row)
                                        (funcall function (nth field-index row))))
                              field-index-list
                              function-list)
                      row))))
    (push (car csv) result)))

;;; remove utilities

(defun remove-column (csv field-name-or-index)
  (loop
     with index = (ensure-field-index csv field-name-or-index)
     for row in csv
     collect (delete-index row index)))

(defun remove-columns (csv field-name-or-index-list)
  (loop
     with index-list = (ensure-field-index-list csv field-name-or-index-list)
     with remain-index-list = (sort (set-difference (loop
                                                       with length = (length (car csv))
                                                       for i from 0 below length
                                                       collect i)
                                                    index-list)
                                    #'<)
     for row in csv
     collect (mapcar #'(lambda (index)
                         (nth index row))
                     remain-index-list)))

;;; search utilities

(defun search-row-by-cell (csv searchword-or-predicate &key regex-p)
  "Search row by each cell with searchword(can be regex)"
  (assert (or (stringp searchword-or-predicate)
              (functionp searchword-or-predicate)))
  (let* ((predicate (if (stringp searchword-or-predicate)
                        (if regex-p
                            (let ((regex (ppcre:create-scanner searchword-or-predicate)))
                              #'(lambda (value)
                                  (let ((value (if (stringp value)
                                                   value
                                                   (write-to-string value))))
                                    (ppcre:all-matches regex value))))
                            #'(lambda (value)
                                (let ((value (if (stringp value)
                                                 value
                                                 (write-to-string value))))
                                  (search searchword-or-predicate value))))
                        searchword-or-predicate))
         (result (loop
                    for row in (cdr csv)
                    when (loop
                            with result = nil
                            for cell in row
                            when (funcall predicate cell)
                            do (progn
                                 (setf result t)
                                 (loop-finish))
                            finally (return result))
                    collect row)))
    (push (car csv) result)))

(defun search-row-by-column (csv field-name-or-index searchword-or-predicate
                             &key regex-p)
  "Search row by specific column with searchword(can be regex).

Note: search-row-by-column is like function filter-rows in some way but more convenient."
  (assert (or (stringp searchword-or-predicate)
              (functionp searchword-or-predicate)))
  (let* ((predicate (if (stringp searchword-or-predicate)
                        (if regex-p
                            (let ((regex (ppcre:create-scanner searchword-or-predicate)))
                              #'(lambda (value)
                                  (let ((value (if (stringp value)
                                                   value
                                                   (write-to-string value))))
                                    (ppcre:all-matches regex value))))
                            #'(lambda (value)
                                (let ((value (if (stringp value)
                                                 value
                                                 (write-to-string value))))
                                  (search searchword-or-predicate value))))
                        searchword-or-predicate))
         (field-index (ensure-field-index csv field-name-or-index))
         (result (loop
                    for row in (cdr csv)
                    when (funcall predicate (nth field-index row))
                    collect row)))
    (push (car csv) result)))

;;; rename-field-name utilities

(defun rename-field-name (csv old-field-name-or-index new-field-name)
  (let ((result (copy-tree csv)))
    (setf (nth (ensure-field-index csv old-field-name-or-index)
               (car result))
          new-field-name)
    result))

(defun rename-field-names (csv old-field-name-or-index-new-field-name-alist)
  (assert (and (listp old-field-name-or-index-new-field-name-alist)
               (or (every #'stringp
                          (mapcar #'car old-field-name-or-index-new-field-name-alist))
                   (every #'integerp
                          (mapcar #'car old-field-name-or-index-new-field-name-alist)))
               (every #'stringp (mapcar #'cdr old-field-name-or-index-new-field-name-alist))))
  (let ((result (copy-tree csv)))
    (mapcar #'(lambda (field-index new-field-name)
                (setf (nth field-index (car result))
                      new-field-name))
            (ensure-field-index-list csv
                                     (mapcar #'car
                                             old-field-name-or-index-new-field-name-alist))
            (mapcar #'cdr old-field-name-or-index-new-field-name-alist))
    result))

;;; csv->markdown utilities

(defun csv->markdown (csv &key (beautify-p t) align-alist (split-header-body-p t))
  (if beautify-p
      (progn
        (assert (or (null align-alist)
                    (and (listp align-alist)
                         (or (every #'stringp (mapcar #'car align-alist))
                             (every #'integerp (mapcar #'car align-alist)))
                         (every #'(lambda (align) (member align '(:left :right :center)))
                                (mapcar #'cdr align-alist)))))
        (let* ((header (car csv))
               (field-count (length header))
               (field-max-length-list
                (loop
                   for field-index from 0 below field-count
                   collect (apply #'max
                                  (mapcar #'(lambda (row)
                                              (let ((value (nth field-index row)))
                                                (unless (stringp value)
                                                  (setf value (write-to-string value)))
                                                (length value)))
                                          csv))))
               (align-alist (loop for (field-name-or-index . align) in align-alist
                               collect (cons (ensure-field-index csv field-name-or-index)
                                             align)))
               (field-align-list
                (loop
                   for i from 0 below field-count
                   collect (if (assoc i align-alist)
                               (cdr (assoc i align-alist))
                               :left))))
          ;; Format begin
          (with-output-to-string (stream)
            ;; Deal with header
            (format stream "|")
            (mapcar #'(lambda (field-name field-max-length)
                        (format stream
                                (format nil " ~~~A:@<~~A~~> |" 
                                        field-max-length)
                                field-name))
                    header
                    field-max-length-list)
            (format stream "~%")
            ;; --- thing(between header and body)
            (when split-header-body-p
              (format stream "|")
              (mapcar #'(lambda (field-max-length field-align)
                          (format stream
                                  "~A|"
                                  (case field-align
                                    (:right (format nil
                                                    (format nil "~~A~~~A,,,'-@A~~A"
                                                            field-max-length)
                                                    "-"
                                                    "-"
                                                    ":"))
                                    (:center (format nil
                                                     (format nil "~~A~~~A,,,'-@A~~A"
                                                             field-max-length)
                                                     ":"
                                                     "-"
                                                     ":"))
                                    (t (format nil
                                               (format nil "~~A~~~A,,,'-@A~~A"
                                                       field-max-length)
                                               "-"
                                               "-"
                                               "-")))))
                      field-max-length-list
                      field-align-list)
              (format stream "~%"))
            ;; Deal with cell
            (mapcar #'(lambda (row)
                        (format stream "|")
                        (mapcar #'(lambda (cell field-max-length field-align)
                                    (format stream
                                            " ~A |"
                                            (case field-align
                                              (:right (format nil
                                                              (format nil "~~~A@A" 
                                                                      field-max-length) 
                                                              cell))
                                              (:center (format nil
                                                               (format nil "~~~A:@<~~A~~>" 
                                                                       field-max-length)
                                                               cell))
                                              ;; left
                                              (t (format nil
                                                         (format nil "~~~A,A" 
                                                                 field-max-length) 
                                                         cell)))))
                                row
                                field-max-length-list
                                field-align-list)
                        (format stream "~%"))
                    (cdr csv)))))
      ;; without beautify
      (with-output-to-string (stream)
        (format stream "|~{ ~A |~}~%" (car csv))
        (when split-header-body-p
          (format stream "|")
          (dotimes (_ (length (car csv)))
            (format stream "-----|"))
          (format stream "~%"))
        (format stream "~{|~{ ~A |~}~%~}" (cdr csv)))))
