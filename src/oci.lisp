;;;; oci.lisp
;;;;
;;;; This file is part of the cl-oracle library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>, Dmitry Statyvka <dmitry@statyvka.org.ua>

(in-package #:oracle)

(define-foreign-library ocilib
  (:linux "libocilib.so")
  (:windows "ocilib"))

(use-foreign-library ocilib)

(defcenum (%error-type :uint)
  (:oci-err-oracle 1)
  (:oci-err-ocilib 2)
  (:oci-err-warning 3))

(defcfun ("OCI_ErrorGetString" %error-get-string) :string
  (err :pointer))

(defcfun ("OCI_ErrorGetType" %error-get-type) %error-type
  (err :pointer))

(define-condition oci-error (error)
  ((messages :initarg :messages :initform nil)))

(defmethod print-object ((err oci-error) stream)
  (print-unreadable-object (err stream :type t)
    (format stream "~{~A~^%~}" (slot-value err 'messages))))

(defvar *error-message-list*)

(defcallback %error-handler :void ((%err :pointer))
  (cond
    ((eql (%error-get-type %err) :oci-err-warning)
     (warn (%error-get-string %err)))
    ((boundp '*error-message-list*)
     (push (string-right-trim #(#\Newline) (%error-get-string %err))
           *error-message-list*))
    (t
     (warn (%error-get-string %err)))))

(defmacro define-oci-function ((foreign-name lisp-name) return-type &rest args)
  (let ((impl-name (intern (format nil "%~A" (symbol-name lisp-name))))
        (simplify-args (map 'list #'car args)))
    `(progn
       (defcfun (,foreign-name ,impl-name) ,return-type ,@args)
       (defun ,lisp-name ,simplify-args
         (let ((*error-message-list* nil))
           (prog1
               (,impl-name ,@simplify-args)
             (when *error-message-list*
               (error 'oci-error :messages (reverse  *error-message-list*)))))))))

(defcenum (%env :uint)
  (:oci-env-default  0)
  (:oci-env-threaded 1)
  (:oci-env-context  2)
  (:oci-env-events   4))

(defcfun ("OCI_Initialize" %initialize) :boolean
  (error-handler :pointer)
  (lib-path :string)
  (mode %env))

(defcfun ("OCI_Cleanup" %cleanup) :boolean)

(define-oci-function ("OCI_EnableWarnings" %enable-warnings) :void
  (enable :boolean))

(defun initialize (&key lib-path (enable-warnings t))
  (prog1
      (%initialize (callback %error-handler)
                   (or lib-path (null-pointer))
                   :oci-env-default)
    (%enable-warnings enable-warnings)))

(defun cleanup ()
  (%cleanup))
