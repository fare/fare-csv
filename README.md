fare-csv
========

This library allows you to read and write CSV files, according to
any of the prevailing "standards" and their popular variants.


Example usage
-------------

    (ql:quickload :fare-csv)
    (fare-csv:with-rfc4180-csv-syntax ()
      (let ((fare-csv:*separator* #\;))
       (fare-csv:read-csv-file "/tmp/semicolon-separated.csv")))


Competing standards
-------------------

CSV means "Comma-Separated Values". It's a vastly underspecified "standard",
as each and every implementation seems to behave differently, and sometimes,
even major implementations (e.g. Microsoft Excel) change their behavior
from one version to the next. Moreover, programs using CSV often explicitly
allow for variants, whereby another character can be used instead of
the standard comma `U+2C #\,` as a separator (typically, a tab, `U+09`, or
a semi-colon, `U+3B #\;`), and another character can be used for quoting
instead of the standard double-quote `U+22 #\"` (typically, a single-quote
`U+27 #\'`). Finally, some implementations don't handle quotation properly
when printing, and different implementations do different things with
respect to line-ending. We try to handle all sensible such variants.
However, one thing we do not try to do is encoding or decoding complex
objects, as there is no standard whatsoever that covers this.
The only standardized type for entries is strings, and
we parse everything as (properly quoted) strings.
We print strings by properly quoting them, and we `PRINC` numbers:
it is up to you to make sure numbers are printed as you desire,
or else to pass a string if CL's `PRINC` doesn't do what you want.

By default, we follow the specification from creativyst,
that seems to describe popular usage:
	<http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm>

This document says about the same:
	<http://edoceo.com/utilitas/csv-file-format>

There is now an RFC that tries to standardize CSV,
and we support it as well:
	<http://www.rfc-editor.org/rfc/rfc4180.txt>

Finally, here's what Perl hackers think CSV is:
	<http://search.cpan.org/~hmbrand/Text-CSV_XS-0.59/CSV_XS.pm>


Exported Functionality
----------------------

fare-csv defines and uses package `FARE-CSV`.

* `Function READ-CSV-STREAM (STREAM)`
  Read lines from `STREAM` in CSV format, using the current syntax parameters.
  Return a list of list of strings, one entry for each line,
  that contains one entry for each field.
  Entries are read as strings;
  it is up to you to interpret the strings as whatever you want.

* `Function READ-CSV-LINE (STREAM)`
  Read one line from `STREAM` in CSV format, using the current syntax parameters.
  Return a list of strings, one for each field in the line.
  Entries are read as strings;
  it is up to you to interpret the strings as whatever you want.

* `Function READ-CSV-FILE (PATHNAME &KEY ELEMENT-TYPE EXTERNAL-FORMAT)`
  Open the file designated by `PATHNAME`, using the provided keys if any,
  and call `READ-CSV-STREAM` on it.

* `Function WRITE-CSV-LINES (LINES STREAM)`
  Given a list of `LINES`, each of them a list of fields, and a `STREAM`,
  format those lines as CSV according to the current syntax parameters.

* `Function WRITE-CSV-LINE (FIELDS STREAM)`
  Format one line of `FIELDS` to `STREAM` in CSV format,
  using the current syntax parameters.
  Take a list of `FIELDS`, and format them as follows:
  if it's a string, write it,
  only using quotes if needed for escaping;
  if it's null, write an empty field;
  if it's a different symbol, write its name as if a string,
  only using quotes if needed for escaping;
  if it's a number, format it as per `PRINC`.

* `Constant +CR+`
  a string with the ASCII character 13 (Carriage Return).
  It's the standard line termination for text on MacOS.

* `Constant +LF+`
  a string with the ASCII character 10 (Line Feed).
  It's the standard line termination for text on Unix.

* `Constant +CRLF+`
  a string with the ASCII characters 13 and 10 (CR, LF).
  It's the standard line termination for text on Windows, and many RFCs.

* `Variable *SEPARATOR*`
  The separator to use when reading or writing CSV files.
  A character. By default, a comma: `#\,`

* `Variable *QUOTE*`
  The quote character to use when reading or writing CSV files.
  A character. By default, a double-quote: `#\"`

* `Variable *UNQUOTED-QUOTEQUOTE*`
  A boolean that is true iff a pair of quotes
  represents a quote outside of quotes.
  Microsoft and RFC4180 says `NIL`, csv.3tcl says `T`.
  A boolean. By default, `NIL`.

* `Variable *LOOSE-QUOTE*`
  A boolean that is true iff quotes appear anywhere in a field?
  By default, `NIL`.

* `Variable *ALLOW-BINARY*`
  A boolean that is true iff we accept non-ASCII data.
  A boolean. By default, `T`.

* `Variable *KEEP-META-INFO*`
  A boolean that when true causes the reader functions to return
  for each entry, instead of a string, a list of a string and a plist;
  the plist currently only has one property, :quoted, that has a boolean value
  which is true iff the string included quotes.
  A boolean. By default, `NIL`.

* `Variable *EOL*`
  Line ending to use when writing CSV files.
  A string. By default, `+CRLF+` as specified by creativyst.

* `Variable *LINE-ENDINGS*`
  A list of line endings accepted when parsing a CSV file.
  Valid elements of that list are the constants `+CRLF+`, `+LF+` and `+CR+`.
  By default, contains all three values, as specified by creativyst.

* `Variable *SKIP-WHITESPACE*`
  A boolean that when true causes initial and final (unquoted) spaces
  to be ignored while parsing CSV.
  A boolean. By default, `T` as specified by creativyst.

* `Macro WITH-CREATIVYST-CSV-SYNTAX () &BODY BODY`
  A macro in which to wrap a program `BODY`, around which
  all the above parameters will be bound to their default value,
  as specified by creativyst.

* `Macro WITH-RFC4180-CSV-SYNTAX () &BODY BODY`
  A macro in which to wrap a program `BODY`, around which
  all the above parameters will be bound as per the RFC4180 specification.
  As compared to creativyst, `*EOL*` is `+LF+`,
  `*LINE-ENDINGS*` doesn't contain `+CR+` but only `+CRLF+` and `+LF+`
  and `*SKIP-WHITESPACE*` is `NIL`.

* `Macro WITH-STRICT-RFC4180-CSV-SYNTAX () &BODY BODY`
  A macro in which to wrap a program `BODY`, around which
  all the above parameters will be bound as per the RFC4180 specification,
  but with a stricter interpretation:
  only `+CRLF+` is accepted as `*LINE-ENDINGS*`, and we don't `*ALLOW-BINARY*` data.
