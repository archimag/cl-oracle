;;;; query.lisp
;;;;
;;;; This file is part of the cl-oracle library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>, Dmitry Statyvka <dmitry@statyvka.org.ua>

(in-package #:oracle)

(defctype %result-set :pointer)
(defctype %column :pointer)

(define-oci-function ("OCI_GetResultset" %get-resultset) %result-set
  (statement %statement))

(define-oci-function ("OCI_FetchNext" %fetch-next) :boolean
  (rs %result-set))

(define-oci-function ("OCI_GetColumnCount" %get-column-count) :uint
  (rs %result-set))

(define-oci-function ("OCI_GetColumn" %get-column) %column
  (rs %result-set)
  (index :uint))

(defcenum (%oci-value-type :uint)
  (:oci-cdt-unknown    0)
  (:oci-cdt-numeric    1)
  (:oci-cdt-datetime   3)
  (:oci-cdt-text       4)
  (:oci-cdt-long       5)
  (:oci-cdt-cursor     6)
  (:oci-cdt-lob        7)
  (:oci-cdt-file       8)
  (:oci-cdt-timestamp  9)
  (:oci-cdt-interval   10)
  (:oci-cdt-raw        11)
  (:oci-cdt-object     12)
  (:oci-cdt-collection 13)
  (:oci-cdt-ref        14))

(define-oci-function ("OCI_ColumnGetType" %get-column-type) %oci-value-type
  (column %column))

(define-oci-function ("OCI_ColumnGetName" %get-column-name) :string
  (column %column))

(define-oci-function ("OCI_ColumnGetSubType" %get-column-subtype) :uint
  (column %column))

(define-oci-function ("OCI_GetString" %get-string) :string
  (rs %result-set)
  (index :uint))

(defun get-numeric (rs index)
  (let ((str (%get-string rs index)))
    (if str
        (parse-number:parse-number str))))

(defun column-value-getter (column)
  (case (%get-column-type column)
    (:oci-cdt-text #'%get-string)
    (:oci-cdt-numeric #'get-numeric)
    (otherwise #'%get-string)))

(defun field-name-s (str)
  (let ((name (string-upcase str)))
    (iter (for i from 0 below (length name))
          (when (char= (char str i)
                       #\_)
            (setf (char name i)
                  #\-)))
    (intern name :keyword)))

(defun one-row (rs columns get-row-fun)
  (when (%fetch-next rs)
    (funcall get-row-fun rs columns)))

(defun all-rows (rs columns get-row-fun)
  (iter (while (%fetch-next rs))
        (collect (funcall get-row-fun rs columns))))

(defmacro define-row-reader (name (column value) &body body)
  (let ((rs (gensym))
        (columns (gensym))
        (column-info (gensym))
        (i (gensym)))
    `(defun ,name (,rs ,columns)
       (iter (for ,column-info in ,columns)
             (for ,i from 1)
             (for ,column = (car ,column-info))
             (for ,value = (funcall (cdr ,column-info) ,rs ,i))
             ,@body))))

(define-row-reader read-plist-row (column value)
  (collect column)
  (collect value))

(define-row-reader read-alist-row (column value)
  (collect (cons column value)))

(define-row-reader read-list-row (column value)
  (collect value))

(defun read-single-value (rs columns)
  (funcall (cdr (car columns))
           rs
           1))

(defparameter *query-formats*
  `((:lists read-list-row all-rows)
    (:list read-list-row one-row)
    (:rows read-list-row all-rows)
    (:row read-list-row one-row)
    (:alists read-alist-row all-rows keyword-column)
    (:alist read-alist-row one-row keyword-column)
    (:str-alists read-alist-row all-rows)
    (:str-alist read-alist-row one-row)
    (:plists read-plist-row all-rows keyword-column)
    (:plist read-plist-row one-row keyword-column)
    (:single read-single-value one-row)))

(defun fetch-results (rs format &aux (format-info (cdr (assoc format *query-formats*))))
  (unless format-info
    (error "Unknow query format: ~A" format))
  (let ((columns (iter (for i from 1 to (%get-column-count rs))
                       (for %column = (%get-column rs i))
                       (collect
                           (cons (if (eql (third format-info) 'keyword-column)
                                     (field-name-s (%get-column-name %column))
                                     (%get-column-name %column))
                                 (column-value-getter %column))))))
      (funcall (second format-info)
               rs
               columns
               (first format-info))))

(defun query (query &key (format :lists))
  (with-statement ()
    (%execute-stmt *statement* query)
    (fetch-results (%get-resultset *statement*)
                   format)))
