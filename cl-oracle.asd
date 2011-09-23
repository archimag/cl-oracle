;;;; cl-oracle.asd
;;;;
;;;; This file is part of the cl-oracle library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>, Dmitry Statyvka <dmitry@statyvka.org.ua>


(defsystem #:cl-oracle
  :author "Dmitry Statyvka <dmitry@statyvka.org.ua>, Moskvitin Andrey <archimag@gmail.com>"
  :version "0.0.1"
  :depends-on (#:cffi #:iterate #:parse-number)
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "oci" :depends-on ("package"))
                         (:file "connection" :depends-on ("oci"))
                         (:file "statement" :depends-on ("connection"))
                         (:file "query" :depends-on ("statement"))))))
