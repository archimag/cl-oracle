;;;; statement.lisp
;;;;
;;;; This file is part of the cl-oracle library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>, Dmitry Statyvka <dmitry@statyvka.org.ua>

(in-package :oracle)

(defctype %statement :pointer)

(define-oci-function ("OCI_StatementCreate" %create-statement) %statement
  (connection %connection))

(defun create-statement (&optional (connection *connection*))
  (%create-statement connection))

(define-oci-function ("OCI_StatementFree" %free-statement) :boolean
  (statement %statement))

(defun release-statement (statement)
  (%free-statement statement))

(defvar *statement* nil)

(defmacro with-statement ((&optional sql (connection '*connection*) (var '*statement*)) &body body)
  `(let ((,var (create-statement ,connection)))
     (unwind-protect
          (progn 
            ,(when sql
               `(oci-execute-stmt ,var ,sql))
            ,@body)
       (release-statement ,var))))

;;;; execute

(define-oci-function ("OCI_ExecuteStmt" %execute-stmt) :boolean
  (statement %statement)
  (sql :string))

(defun execute (sql)
  (with-statement ()
    (%execute-stmt *statement* sql)))

