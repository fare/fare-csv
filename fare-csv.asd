;;; -*- Lisp -*-

(defsystem #:fare-csv
  :depends-on ()
  :licence "MIT"
  :components ((:file "package") (:file "csv"))
  :description "Robust CSV parser and printer"
  :long-description "Robust CSV (Comma-separated values) parser and printer,
tries to follow the fine print of de facto standards,
can be configured to choose which standard exactly."
  :license "BSD or bugroff"
  :serial t)
