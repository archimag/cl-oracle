;;;; package.lisp
;;;;
;;;; This file is part of the cl-oracle library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Authors: Moskvitin Andrey <archimag@gmail.com>, Dmitry Statyvka <dmitry@statyvka.org.ua>

(defpackage #:oracle
  (:use #:cl #:cffi #:iter)
  (:export #:initialize
           #:cleanup

           #:with-connection
           #:execute
           #:query))

