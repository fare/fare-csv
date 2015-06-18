;;; -*- Lisp -*-

(defsystem "fare-csv"
  :license "BSD or bugroff"
  :author "Francois-Rene Rideau"
  :version "1.0.1"
  :depends-on ()
  :components ((:file "package") (:file "csv"))
  :description "Robust CSV parser and printer"
  :long-description "Robust CSV (Comma-separated values) parser and printer,
tries to follow the fine print of de facto standards,
can be configured to choose which standard exactly."
  :serial t .
  #.(or #+asdf3.1
        '(:class :package-inferred-system
          :in-order-to ((test-op (load-op "fare-csv/test")))
          :perform (test-op (o s) (symbol-call :fare-csv/test :test-suite)))))
