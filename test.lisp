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

(deftest test-strict-record-endings ()
  (with-strict-rfc4180-csv-syntax ()
    (let ((rows '(("a") ("b"))))
      (let ((csv (with-output-to-string (out) (write-csv-lines rows out))))
        (is (equal csv (strcat "a" +crlf+ "b" +crlf+)))
        (is (equal rows (csv->lines csv)))))
    (signals error (csv->lines (strcat "a" +lf+ "b")))
    (signals error (csv->lines (strcat "a" +cr+ "b")))
    (is (equal (csv->lines (strcat "\"" +lf+ "\"" +crlf+))
               (list (list +lf+))))))

(deftest test-single-empty-field ()
  (let ((rows (list nil '("") '(nil) (list (make-symbol "")) '("" ""))))
    (is (equal (csv->lines (with-output-to-string (out)
                             (write-csv-lines rows out)))
               '(nil ("") ("") ("") ("" ""))))))

(deftest test-configured-separator ()
  (let ((*separator* #\/))
    (is (equal (csv->lines (with-output-to-string (out)
                             (write-csv-line '(1/2 :a/b "x/y") out)))
               '(("1/2" "A/B" "x/y"))))))

(deftest test-field-round-trips ()
  (let ((rows (loop for x in (list "a" " a" "a " "a b" "," "\"" "a,\"b"
                                   +cr+ +lf+ +crlf+ "" nil)
                    collect (list x "tail"))))
    (flet ((check ()
             (is (equal (csv->lines (with-output-to-string (out)
                                      (write-csv-lines rows out)))
                        (loop for (x tail) in rows collect (list (or x "") tail))))))
      (with-creativyst-csv-syntax () (check))
      (with-rfc4180-csv-syntax () (check))
      (with-strict-rfc4180-csv-syntax () (check)))))

(deftest test-custom-record-endings ()
  (let ((*line-endings* (list +cr+)) (*eol* +cr+))
    (is (equal (csv->lines (strcat "a" +lf+ "b" +cr+))
               (list (list (strcat "a" +lf+ "b"))))))
  (with-strict-rfc4180-csv-syntax ()
    (with-creativyst-csv-syntax ()
      (let ((*line-endings* (list +lf+)))
        (is (equal (csv->lines (strcat "a" +cr+ "b" +lf+))
                   (list (list (strcat "a" +cr+ "b")))))))))
