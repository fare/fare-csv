(uiop:define-package :fare-csv/test
  (:mix :cl :fare-csv :uiop :hu.dwim.stefil)
  (:export #:test-suite))

(in-package :fare-csv/test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing fare-csv"))

(defun normalize-crlf (input &optional output)
  (format output "~{~A~%~}"
    (with-input (input)
      (slurp-stream-lines input))))

(defun csv->lines (csv)
  (with-input (csv) (read-csv-stream csv)))

(defun lines->csv (lines)
  (normalize-crlf (with-output (o nil) (write-csv-lines lines o))))

(defun test-read (csv expected)
  (eval `(is (equal (csv->lines ',csv) ',expected))))

(defun test-write (lists expected)
  (eval `(is (equal (lines->csv ',lists) ',expected))))

(defun test-both (csv lists)
  (test-read csv lists)
  (test-write lists csv))

(defun test-read-write (csv lists csv-again)
  (test-read csv lists)
  (test-write lists csv-again))

(defun test-both-lf (csv lists)
  (test-read csv lists)
  (test-write lists (strcat csv +lf+)))


(deftest test-empty-fields ()
  (test-both
   "1,2,3
,2,3
1,,3
1,2,
"
   '(("1" "2" "3")
     ("" "2" "3")
     ("1" "" "3")
     ("1" "2" ""))))

(deftest test-happy-ending ()
  (test-both-lf
   "one,two,three,"
   '(("one" "two" "three" "")))
  (test-read-write
   "one,two,three   "
   '(("one" "two" "three"))
   "one,two,three
")
  (test-read-write
   "   "
   '(())
   +lf+)
  (test-both
   ""
   '()))
