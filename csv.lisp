;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; csv: reading files in Comma-Separated Values format.

#+xcvb (module (:depends-on ("package")))

#| "
HOME PAGE:
	http://www.cliki.net/fare-csv

LICENSE:
	http://tunes.org/legalese/bugroff.html
	Also under no-restriction BSD license for those who insist.

DEPENDENCIES:
	asdf

USAGE:
	(asdf:load-system :fare-csv)
	(read-csv-file "foo.csv")
	(read-csv-stream stream)
	(read-csv-line stream)
	(write-csv-lines lines stream)
	(write-csv-line fields stream)

EXAMPLE USE:
	...

BUGS:
	I implemented just enough of CSV to import a specific file
	from a PC application that will remain unnamed.
	If you need more, you can cont(r)act me, and/or hack it yourself.

	CSV is intrinsically an underspecified lossy format,
	and the particular PC application I'm using loses heavily
	(i.e. no quoting convention at all, not even a pascal-like one)
	when text fields contain the quote character. Ouch.

SEE ALSO:
	This spec seems to explain popular usage, is refered by docs below.
	http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm

	This one says about the same:
	http://edoceo.com/utilitas/csv-file-format

	There's now an RFC that tries to standardize CSV:
	http://www.rfc-editor.org/rfc/rfc4180.txt

	Here's what Perl hackers think CSV is:
	http://search.cpan.org/~hmbrand/Text-CSV_XS-0.59/CSV_XS.pm


Share and enjoy!
" |#

; -----------------------------------------------------------------------------
;;; Packaging stuff

(in-package :fare-csv)

; -----------------------------------------------------------------------------
;;; Optimization
(eval-when (:compile-toplevel :execute)
  (declaim (optimize (speed 3) (safety 1) (debug 3))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)))

; -----------------------------------------------------------------------------
;;; Thin compatibility layer
#| ;;; Not needed anymore
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'parse-number)
    (defun parse-number (string)
      (with-standard-io-syntax ()
	(let* ((*read-eval* nil)
	       (*read-default-float-format* 'double-float)
	       (n (read-from-string string)))
	  (when (numberp n) n)))))) |#

; -----------------------------------------------------------------------------
;;; Parameters

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +cr+ #.(format nil "~A" #\Return) "String containing a CR (Carriage Return)")
  (defparameter +lf+ #.(format nil "~A" #\Linefeed) "String containing a LF (Linefeed)")
  (defparameter +crlf+ #.(format nil "~A~A" #\Return #\Linefeed) "String containing a CRLF line termination")
  (defparameter *csv-variables* '())) ; list of (var rfc4180-value creativyst-value)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet
      ((def (var rfc4180 creativyst doc)
         `(progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (pushnew `(,',var ,,rfc4180 ,,creativyst) *csv-variables* :key #'car))
            (defparameter ,var ,creativyst ,doc))))
    (def *separator*
	#\, #\,
      "Separator between CSV fields")
    (def *quote*
	#\" #\"
      "delimiter of string data; pascal-like quoted as double itself in a string.")
    (def *unquoted-quotequote*
	nil nil
      "does a pair of quotes represent a quote outside of quotes?
M$, RFC says NIL, csv.3tcl says T")
    (def *loose-quote*
	nil nil
      "can quotes appear anywhere in a field?")
    (def *allow-binary*
	t t
      "do we accept non-ascii data?")
    (def *keep-meta-info*
	nil nil
      "when parsing, include meta information?")
    (def *eol*
	+lf+ +crlf+
      "line ending when exporting CSV")
    (def *line-endings*
	(list +crlf+ +lf+) (list +cr+ +lf+ +crlf+)
      "acceptable line endings when importing CSV")
    (def *skip-whitespace*
	nil t
      "shall we skip unquoted whitespace around separators?")))

(defun char-ascii-text-p (c)
  (<= #x20 (char-code c) #x7E))

(defmacro with-creativyst-csv-syntax ((&optional) &body body)
  "bind CSV syntax parameters to the CREATIVYST standard around evaluation of BODY"
  `(call-with-creativyst-csv-syntax (lambda () ,@body)))
(defun call-with-creativyst-csv-syntax (thunk)
  (progv (mapcar #'first *csv-variables*) (mapcar #'third *csv-variables*)
    (funcall thunk)))

(defmacro with-rfc4180-csv-syntax ((&optional) &body body)
  "bind CSV syntax parameters to the RFC 4180 standard around evaluation of BODY"
  `(call-with-rfc4180-csv-syntax (lambda () ,@body)))
(defun call-with-rfc4180-csv-syntax (thunk)
  (progv (mapcar #'first *csv-variables*) (mapcar #'second *csv-variables*)
    (funcall thunk)))

(defmacro with-strict-rfc4180-csv-syntax ((&optional) &body body)
  "bind CSV syntax parameters to the strict RFC 4180 standard around evaluation of BODY,
forcing CRLF as line ending and disallowing binary data amongst values"
  `(call-with-strict-rfc4180-csv-syntax (lambda () ,@body)))

(defun call-with-strict-rfc4180-csv-syntax (thunk)
  (with-rfc4180-csv-syntax ()
    (setf *line-endings* (list +crlf+)
	  *allow-binary* nil)
    (funcall thunk)))

(defun valid-eol-p (x)
  (member x (list +cr+ +lf+ +crlf+) :test #'equal))

(defun validate-csv-parameters ()
  (assert (typep *separator* 'character) ())
  (assert (typep *quote* 'character) ())
  (assert (not (eql *separator* *quote*)) ())
  (assert (typep *unquoted-quotequote* 'boolean) ())
  (assert (typep *loose-quote* 'boolean) ())
  (assert (typep *keep-meta-info* 'boolean) ())
  (assert (valid-eol-p *eol*) ())
  (assert (not (member (aref *eol* 0) (list *separator* *quote*))) ())
  (assert (and *line-endings* (every #'valid-eol-p *line-endings*)) ())
  (assert (typep *skip-whitespace* 'boolean) ()))

;; For internal use only
(defvar *accept-cr* t "internal: do we accept cr?")
(defvar *accept-lf* t "internal: do we accept lf?")
(defvar *accept-crlf* t "internal: do we accept crlf?")

; -----------------------------------------------------------------------------
;;; The parser

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name))
	  (defun ,name ,arglist ,@body)))

(defsubst char-space-p (c)
  "Is character C some kind of white space?
NB: this only handles a tiny subset of whitespace characters,
even if restricted to ASCII. However, it's rather portable,
and is what the creativyst document specifies.
Be careful to not skip a separator, as it could be e.g. a tab!"
  (declare (type (or null character) c))
  (and c (member c '(#\Space #\Tab)) (not (eql c *separator*))))

;;#+DEBUG (defparameter *max* 2000)
;;#+DEBUG (defun maxbreak () (when (<= *max* 0) (setf *max* 2000) (break)) (decf *max*))

(defsubst accept-p (x stream)
  (let ((c (peek-char nil stream nil nil)))
    ;;#+DEBUG (format t "~&Current char: ~S~%" c)
    ;;#+DEBUG (maxbreak)
    (etypecase x
      (character (eql x c))
      ((or function symbol) (funcall x c))
      (integer (eql x (char-code c))))))

(defsubst accept (x stream)
  (and (accept-p x stream)
       (read-char stream)))

(defsubst accept-eof (stream)
  (not (peek-char nil stream nil nil)))

(defsubst accept-eol (stream)
  (block nil
    (when (and *accept-lf* (accept #\Linefeed stream)) (return t))
    (when (or *accept-crlf* *accept-cr*)
      (when (accept #\Return stream)
	(when *accept-crlf*
	  (if (accept #\Linefeed stream)
	      (return t)
	      (unless *accept-cr*
		(error "Carriage-return without Linefeed!"))))
	(return t)))
    nil))

(defsubst accept-space (stream)
  (accept #'char-space-p stream))

(defsubst accept-spaces (stream)
  (loop :for x = (accept-space stream) :while x :collect x))

(defsubst accept-quote (stream)
  (accept *quote* stream))

(defsubst accept-separator (stream)
  (accept *separator* stream))

(defun read-csv-line (stream)
  "Read one line from STREAM in CSV format, using the current syntax parameters.
  Return a list of strings, one for each field in the line.
  Entries are read as strings;
  it is up to you to interpret the strings as whatever you want."
  (validate-csv-parameters)
  (let ((ss (make-string-output-stream))
	(fields '())
	(had-quotes nil)
	;;(had-spaces nil)
	;;(had-binary nil)
	(*accept-cr* (member +cr+ *line-endings* :test #'equal))
	(*accept-lf* (member +lf+ *line-endings* :test #'equal))
	(*accept-crlf* (member +crlf+ *line-endings* :test #'equal)))
    (labels
	((do-fields ()
	   ;;#+DEBUG (format t "~&do-field~%")
	   (setf had-quotes nil)
	   (when *skip-whitespace*
	     (accept-spaces stream))
	   ;;#+DEBUG (format t "~&do-field, after spaces~%")
           (cond
             ((and (= 0 (length fields))
                   (or (accept-eol stream) (accept-eof stream)))
              (done))
             (t
              (do-field-start))))
	 (do-field-start ()
	   ;;#+DEBUG (format t "~&do-field-start~%")
	   (cond
	     ((accept-separator stream)
	      (add "") (do-fields))
	     ((accept-quote stream)
	      (cond
		((and *unquoted-quotequote* (accept-quote stream))
		 (add-char *quote*) (do-field-unquoted))
		(t
		 (do-field-quoted))))
	     (t
	      (do-field-unquoted))))
	 (do-field-quoted ()
	   ;;#+DEBUG (format t "~&do-field-quoted~%")
	   (setf had-quotes t)
           (cond
	     ((accept-eof stream)
	      (error "unexpected end of stream in quotes"))
	     ((accept-quote stream)
	      (cond
		((accept-quote stream)
		 (quoted-field-char *quote*))
		(*loose-quote*
		 (do-field-unquoted))
		(t
		 (add (current-string))
		 (end-of-field))))
	     (t
	      (quoted-field-char (read-char stream)))))
	 (quoted-field-char (c)
	   ;;#+DEBUG (format t "~&quoted-field-char~%")
	   (add-char c)
	   (do-field-quoted))
	 (do-field-unquoted ()
	   ;;#+DEBUG (format t "~&do-field-unquoted~%")
	   (if *skip-whitespace*
	       (let ((spaces (accept-spaces stream)))
		 (cond
		   ((accept-separator stream)
		    (add (current-string))
		    (do-fields))
		   ((or (accept-eol stream) (accept-eof stream))
		    (add (current-string))
		    (done))
		   (t
		    (map () #'add-char spaces)
		    (do-field-unquoted-no-skip))))
	       (do-field-unquoted-no-skip)))
	 (do-field-unquoted-no-skip ()
	   ;;#+DEBUG (format t "~&do-field-unquoted-no-skip~%")
	   (cond
	     ((accept-separator stream)
	      (add (current-string))
	      (do-fields))
	     ((or (accept-eol stream) (accept-eof stream))
	      (add (current-string))
	      (done))
	     ((accept-quote stream)
	      (cond
		((and *unquoted-quotequote* (accept-quote stream))
		 (add-char *quote*) (do-field-unquoted))
		(*loose-quote*
		 (do-field-quoted))
		(t
		 (error "unexpected quote in middle of field"))))
	     (t
	      (add-char (read-char stream))
	      (do-field-unquoted))))
	 (end-of-field ()
	   ;;#+DEBUG (format t "~&end-of-field~%")
	   (when *skip-whitespace*
	     (accept-spaces stream))
	   (cond
	     ((or (accept-eol stream) (accept-eof stream))
	      (done))
	     ((accept-separator stream)
	      (do-fields))
	     (t
	      (error "end of field expected"))))
	 (add (x)
	   ;;#+DEBUG (format t "~&add ~S~%" x)
	   (push
	    (if *keep-meta-info*
		(list x :quoted had-quotes)
		x)
	    fields))
	 (add-char (c)
	   ;;#+DEBUG (format t "~&add-char ~S~%" c)
	   (write-char c ss))
	 (current-string ()
	   (get-output-stream-string ss))
	 (done ()
	   ;;#+DEBUG (format t "~&done ~S~%" fields)
	   (nreverse fields)))
      (do-fields))))

(defun read-csv-stream (stream)
  "Read lines from STREAM in CSV format, using the current syntax parameters.
  Return a list of list of strings, one entry for each line,
  that contains one entry for each field.
  Entries are read as strings;
  it is up to you to interpret the strings as whatever you want."
  (loop :until (accept-eof stream) :collect (read-csv-line stream)))

(defun read-csv-file (pathname &rest keys &key element-type external-format)
  "Open the file designated by PATHNAME, using the provided keys if any,
  and call READ-CSV-STREAM on it."
  (declare (ignore element-type external-format))
  (with-open-stream (stream (apply 'open pathname
                                   :direction :input :if-does-not-exist :error keys))
    (read-csv-stream stream)))

(defun char-needs-quoting (x)
  (or (eql x *quote*)
      (eql x *separator*)
      (not (char-ascii-text-p x))))

(defun string-needs-quoting (x)
  (and (not (zerop (length x)))
       (or (char-space-p (char x 0))
	   (char-space-p (char x (1- (length x))))
	   (some #'char-needs-quoting x))
       t))

(defun write-csv-lines (lines stream)
  "Given a list of LINES, each of them a list of fields, and a STREAM,
  format those lines as CSV according to the current syntax parameters."
  (dolist (x lines)
    (write-csv-line x stream)))

(defun write-csv-line (fields stream)
  "Format one line of FIELDS to STREAM in CSV format,
  using the current syntax parameters."
  (loop :for x :on fields :do
    (write-csv-field (first x) stream)
    (when (cdr x)
      (write-char *separator* stream)))
  (write-string *eol* stream))

(defun write-csv-field (field stream)
  (etypecase field
    (null t)
    (number (princ field stream))
    (string (write-csv-string-safely field stream))
    (symbol (write-csv-string-safely (symbol-name field) stream))))

(defun write-csv-string-safely (string stream)
  (if (string-needs-quoting string)
      (write-quoted-string string stream)
      (write-string string stream)))

(defun write-quoted-string (string stream)
  (write-char *quote* stream)
  (loop :for c :across string :do
    (when (char= c *quote*)
      (write-char c stream))
    (write-char c stream))
  (write-char *quote* stream))

;;#+DEBUG (trace read-csv-line read-csv-stream)
;;#+DEBUG (write (read-csv-file "test.csv"))
;;#+DEBUG (progn (setq *separator* #\;) (write (read-csv-file "/samba/ciev.csv")))
