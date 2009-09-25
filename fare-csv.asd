;;; -*- Lisp -*-
(in-package :cl)

(asdf:defsystem #:fare-csv
  :depends-on ()
  :components ((:file "package") (:file "csv"))
  :serial t)
