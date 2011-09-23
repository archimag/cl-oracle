;;;; connnection.lisp
;;;;
;;;; This file is part of the cl-oracle library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>, Dmitry Statyvka <dmitry@statyvka.org.ua>

(in-package :oracle)

(defctype %connection :pointer)

(defcenum (%mode :uint)
  (:oci-session-default     0)
  (:oci-session-xa          1)
  (:oci-session-sysdba      2)
  (:oci-session-sysoper     4)
  (:oci-session-prelim-auth 8))


(define-oci-function ("OCI_ConnectionCreate" %connection-create) %connection
  (db :string)
  (user :string)
  (pwd :string)
  (mode %mode))

(define-oci-function ("OCI_ConnectionFree" %connection-free) :boolean
  (connection %connection))

(defvar *connection* nil)

(defun create-connection (db user password &optional (mode :oci-session-default))
  (%connection-create db user password mode))

(defun release-connection (connection)
  (%connection-free connection))

(defmacro with-connection ((db user password &optional (var '*connection*)) &body body)
  `(let ((,var (create-connection ,db ,user ,password)))
     (unwind-protect (progn ,@body)
       (release-connection ,var))))
