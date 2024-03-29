;; -*- mode: lisp; -*- 

(defpackage :cl-strings 
  (:nicknames :strings)
  (:use :cl
        :cl-user)
  (:export *whitespace-regex*
           *whitespace-bag*
	   :string-bytes
           :string-char-at
           :string-compare-to
           :string-concat
           :string-contains
           :string-content-equals
           :string-copy
           :string-ends-with
           :string-equals
           :string-format
           :string-index-of
           :string-last-index-of
           :string-length
           :string-matches
           :string-octets
           :string-replace-char
           :string-replace-substring
           :string-replace-all
           :string-replace-first
           :string-region-matches
           :string-split
           :string-starts-with
           :string-subsequence
           :string-substring
           :string-to-char-list
           :string-to-lower
           :string-to-upper
           :string-to-symbol
           :string-strip
           :string-value-of
           :string-tokenizer
           :string-make-tokenizer
           :string-tokenizer-next
           :string-tokenizer-count
           :string-tokenizer-peak
           :string-tokenizer-more-p
           :string-tokenizer-reset))

(in-package :cl-strings)

(defparameter *whitespace-regex* "\\s")
(defparameter *whitespace-bag* (list #\NewLine #\Space #\Return #\Tab))

(defmacro booleanize (expr)
  `(if ,expr t nil))

(defun string-bytes (string)
  "Returns a representation of STRING as a list of bytes 
(unsigned-byte 8), otherwise known as octets."
  (sb-ext:string-to-octets string))

(defun string-char-at (string index)
  "Returns the char value of the specified INDEX in STRING."
  (declare (type string string)
           (type (integer 0 *) index))
  (aref string index))

;; thanks to Zach Beane for this implementation.
(defun string-compare-to (string1 string2 &key (ignore-case nil))
  "Compares two string lexicographicallay.
 -1 if string1 precedes string2
  0 if string1 and string2 are equal
  1 if string1 follows string2"
  (declare (type string string1 string2))
  (if ignore-case
      (cond ((string-equal string1 string2) 0)
            ((string-greaterp string1 string2) 1)
            (t -1))
      (cond ((string= string1 string2) 0)
            ((string> string1 string2) 1)
            (t -1))))

(defun string-concat (string &rest strings)
  "Concatenates any number of STRINGS to the end of the first STRING."
  (declare (type string string))
  (apply #'concatenate 'string string strings))

(defun string-contains (string str-or-char-list)
  "Returns true if STR-OR-CHAR-LIST is contained in STRING."
  (declare (type string string))
  (booleanize (search (coerce str-or-char-list 'string) string 
                      :test #'string=)))

(defun string-content-equals (string char-list)
  "Returns true of the CHAR-LIST is equal to STRING."
  (declare (type string string)
           (type array char-list))
  (let ((string2 (coerce char-list 'string)))
    (string= string string2)))

;; implementation suggested by Zach Beane
(defun string-copy (str-or-char-list)
  "Returns a string representation of STR-OR-CHAR-LIST."
  (if (listp str-or-char-list)
      (coerce str-or-char-list 'string)
      (copy-seq str-or-char-list)))

(defun string-ends-with (string sub-string)
  "Tests if STRING ends with SUB-STRING."
  (declare (type string string sub-string))
  (let* ((len (length string))
         (start1 (- len (length sub-string))))
      (string= string sub-string :start1 start1 :end1 len)))

(defun string-equals (string1 string2 &key (ignore-case nil))
  "Tests if STRING1 is equal to STRING2."
  (declare (type string string1 string2))
  (if ignore-case
      (string-equal string1 string2)
      (string= string1 string2)))

(defun string-format (format &rest arguments)
  "Returns a formatted string using the specified FORMAT (cl printer)
string applied to ARGUMENTS."
  (apply #'format nil format arguments))

(defun string-index-of (string sub-str-or-char &optional (from-index 0))
  "Returns the index of the first occurrance of SUB-STR-OR-CHAR 
in STRING, optionally starting at FROM-INDEX."
  (let ((substring (string-value-of sub-str-or-char)))
    (search substring string :start2 from-index :test #'string=)))

(defun string-last-index-of (string sub-str-or-char &optional from-index)
  "Returns the index of the last occurance of SUB-STR-OR-CHAR 
in STRING, optionally starting at FROM-INDEX."
  (let ((substring (string-value-of sub-str-or-char)))
    (search substring (subseq string 0 from-index) :from-end t 
            :test #'string=)))

(defun string-length (string)
  "Returns the length of STRING."
  (declare (type string string))
  (length string))

(defun string-matches (string regex)
  "Tests whether or not STRING matches the regular expression in REGEX."
  (declare (type string string regex))
  (booleanize (cl-ppcre:all-matches regex string)))

(defun string-octets (string)
  "Returns a representation of STRING as an array of 
octets (unsigned-byte 8)."
  (sb-ext:string-to-octets string))

(defun string-region-matches (string1 string1-offset string1-len
                              string2 string2-offset string2-len
                              &key ignore-case)
  "Test if two string regions are equal, optionally ignoring case."
  (let ((test (if ignore-case #'string-equal #'string=)))
    (funcall test string1 string2 
             :start1 string1-offset :end1 (+ string1-offset string1-len)
             :start2 string2-offset :end2 (+ string2-offset string2-len))))

(defun string-replace-char (string old-char new-char)
  "Replaces all occurances of OLD-CHAR in STRING with NEW-CHAR."
  (declare (type string string)
           (type character old-char new-char))
  (substitute new-char old-char string))

(defun string-replace-substring (string old-charlist new-charlist)
  "Returns a new string with all occurances of OLD-CHARLIST
replaced by NEW-CHARLIST."
  (declare (type string string)
           (type list old-charlist new-charlist))
  (let ((old-str (coerce old-charlist 'string))
        (new-str (coerce new-charlist 'string)))
  (string-replace-all string old-str new-str)))

(defun string-replace-all (string regex replacement)
  "Returns a new string with all occurances of the substring
matching REGEX in STRING replaced by the string REPLACEMENT."
  (declare (type string string regex replacement))
  (cl-ppcre:regex-replace-all regex string replacement))

(defun string-replace-first (string regex replacement)
  "Returns a copy of STRING in which the first substring matching
REGEX is replaced by REPLACEMENT."
  (declare (type string string regex replacement))
  (cl-ppcre:regex-replace regex string replacement))

(defun string-split (string regex &key limit omit-empty)
  "Returns a list of strings split on STRING based on the REGEX
optionally stopping after LIMIT times, and optional removing empty
strings if OMIT-EMTPY."
  (declare (ignorable string regex limit))
  (flet ((empty (x)
           (zerop (length x))))
    (let ((splits (cl-ppcre:split regex string :limit limit)))
      (if omit-empty
          (remove-if #'empty splits)
          splits))))

(defun string-starts-with (string token &optional (offset 0))
  "Tests if STRING begins with TOKEN optionally beginning at
OFFSET."
  (declare (type string string token))
  (string= string token :start1 offset :end1 (+ offset (length token))))

(defun string-subsequence (string begin-index end-index)
  "Returns the list of characters found in STRING from 
BEGIN-INDEX to END-INDEX."
  (declare (type string string)
           (type integer begin-index end-index))
  (string-to-char-list (string-substring string begin-index end-index)))

(defun string-substring (string begin-index &optional end-index)
  "Returns a new string representing the characters in STRING
from position BEGIN-INDEX to the end of STRING, or position 
END-INDEX if provided."
  (declare (type string string)
           (type integer begin-index))
  (cond
    ((< begin-index 0)
     nil)
    ((and (not (null end-index))
         (> end-index (length string)))
     nil)
    (t
     (subseq string begin-index end-index))))

(defun string-to-char-list (string)
  "Returns a list of characters found in STRING."
  (declare (type string string))
  (coerce string 'list))

(defun string-to-lower (string)
  "Returns a lowercase copy of STRING."
  (declare (type string string))
  (string-downcase string))

(defun string-to-upper (string)
  "Returns an uppercase copy of STRING."
  (declare (type string string))
  (string-upcase string))

(defun string-to-symbol (string)
  "Returns an interned symbol named STRING."
  (declare (type string string))
  (intern (string-upcase string)))

(defun string-strip (string &key (charbag *whitespace-bag*))
  "Removes whitespace (defined as CHARBAG) from both ends
of STRING."
  (declare (type string string))
  (string-trim charbag string))

(defun string-value-of (object &key lowercase)
  "Returns the string representation of OBJECT, optionally
forcing it to LOWERCASE."
  (let ((value (format nil "~a" object)))
    (if lowercase
        (string-downcase value)
        value)))

(defgeneric string-tokenizer-count (tokenizer)
  (:documentation "Returns the number of tokens in the tokenizer."))

(defgeneric string-tokenizer-more-p (tokenizer)
  (:documentation "Tests if there are more tokens available."))

(defgeneric string-tokenizer-next (tokenizer)
  (:documentation "Returns next token, advancing the token pointer."))

(defgeneric string-tokenizer-peek (tokenizer)
  (:documentation "Return the next token without advancing the token pointer."))

(defgeneric string-tokenizer-reset (tokenizer)
  (:documentation "Reset the token pointer to the beginning."))

(defclass string-tokenizer ()
  ((delimiter
    :initform "\\s"
    :initarg :delimiter
    :accessor delimiter-of)
   (count
    :initform 0
    :initarg :count
    :reader number-of)
   (tokens
    :initform '()
    :initarg :tokens
    :reader tokens-of)
   (position
    :initform 0
    :initarg :position
    :accessor position-of)))

(defun string-make-tokenizer (string &key (delimiter *whitespace-regex*))
  "Returns a tokenizer object for use with string-tokenizer functions."
  (let* ((tokens (string-split string delimiter))
         (count (length tokens))
         (position 0)
         (delimiter delimiter))
    (make-instance 'string-tokenizer 
                   :delimiter delimiter
                   :count count
                   :tokens tokens
                   :position position)))

(defmethod string-tokenizer-next ((self string-tokenizer))
  (with-slots (tokens position) self
      (let ((result (nth position tokens)))
        (incf position)
        result)))

(defmethod string-tokenizer-peek ((self string-tokenizer))
  (nth (position-of self) (tokens-of self)))

(defmethod string-tokenizer-more-p ((self string-tokenizer))
  (< (position-of self) (number-of self)))

(defmethod string-tokenizer-count ((self string-tokenizer))
  (number-of self))

(defmethod string-tokenizer-reset ((self string-tokenizer))
  (setf (position-of self) 0))

;; eof