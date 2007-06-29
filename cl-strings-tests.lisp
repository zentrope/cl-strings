
(defpackage :cl-strings-tests
  (:use #:cl
        #:cl-user
        #:cl-strings))

(in-package :cl-strings-tests)

(defparameter *test-suite* '()
  "List of tests to run.")

(defun clear-suite ()
  (setf *test-suite* '()))

(defun format-result (name result)
  (string-to-lower (string-format "~a => ~a" result name)))

(defun run-tests ()
  (loop for (name test) in *test-suite*
       do (let ((result (funcall test)))
            (format *standard-output* "~a~%" (format-result name result)))))

(defmacro symbolfy (str)
  `(intern (string-upcase ,str)))

(defun test-result (actual expected)
  (if (equal actual expected)
      'PASSED
      'FAILED))

(defun err-result (name expected condition)
  (if (eq expected :err)
      'passed
      (format nil "ERROR : ~a : ~a" name condition)))

(defun store-test (name test)
  (push (list name test) *test-suite*))

(defmacro deftest (test-name (expected-result) &body test-expr)
  (let ((test-fn (gensym))
        (actual-result (gensym))
        (name (symbolfy test-name)))
    `(let ((,test-fn (lambda ()
                       (handler-case 
                           (let ((,actual-result ,@test-expr))
                             (test-result ,expected-result ,actual-result))
                         (condition (c)
                           (err-result ',name ,expected-result c))))))
       (store-test ',name ,test-fn))))


(clear-suite)

(deftest test-string-bytes (t)
  (string-equals (sb-ext:octets-to-string (string-bytes "abc"))
                 "abc"))

(deftest test-string-octets (t)
  (string-equals (sb-ext:octets-to-string (string-octets "tests are fun"))
                 "tests are fun"))

(deftest test-string-char-at (#\s)
  (string-char-at "test" 2))

(deftest test-string-char-at-type-error (:err)
  (string-char-at 23 2))

(deftest test-string-char-range-error (:err)
  (string-char-at "abc" 5))

(deftest test-string-compare-to-eq (0)
  (string-compare-to "a" "a"))

(deftest test-string-compare-to-lt (-1)
  (string-compare-to "a" "b"))

(deftest test-string-compare-to-gt (1)
  (string-compare-to "b" "a"))

(deftest test-string-copy-eq (t)
  (and (equal "abc" (string-copy "abc"))
       (let ((a "abc"))
         (not (eq a (string-copy a))))))

(deftest test-string-copy-chars (t)
  (equal "abc" (string-copy (list #\a #\b #\c))))

(deftest test-string-ends-with-true (t)
  (string-ends-with "this is a string" "string"))

(deftest test-string-ends-with-false (nil)
  (string-ends-with "this is a string" "strong"))

(deftest test-string-equal (t)
  (string-equals "foo" "foo"))

(deftest test-string-equal-ignore-case (t)
  (string-equals "FOO" "foo" :ignore-case t))

(deftest test-string-not-equal (nil)
  (string-equals "foo" "bar"))

(deftest test-string-equal-type-error-left (:err)
  (string-equals "foo" 23))

(deftest test-string-equal-type-error-right (:err)
  (string-equals 23 "foo"))

(deftest test-string-replace-char (t)
  (string= (string-replace-char "faa" #\a #\o) "foo"))

(deftest test-string-to-char-list (t)
  (let ((result (string-to-char-list "abc")))
    (and (= 3 (length result))
         (equal #\a (first result))
         (equal #\b (second result))
         (equal #\c (third result)))))

(deftest test-string-starts-with-true (t)
  (string-starts-with "string starts with" "string"))

(deftest test-string-starts-with-false (nil)
  (string-starts-with "string starts with" "strong"))

(deftest test-string-starts-with-offset (t)
  (string-starts-with "string starts with" "starts" 7))

(deftest test-string-region-matches-true (t)
  (string-region-matches "a bc d" 2 2 "a bc d" 2 2))

(deftest test-string-region-matches-false (nil)
  (string-region-matches "a bc d" 1 2 "a bc d" 2 2))

(deftest test-string-region-matches-odd (t)
  (string-region-matches "a d bc" 4 2 "a bc d" 2 2))

(deftest test-string-region-matches-caseless (t)
  (string-region-matches "ABCabc" 4 2 "abcABC" 4 2 :ignore-case t))
