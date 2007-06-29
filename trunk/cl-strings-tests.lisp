
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
            (format t "~a~%" (format-result name result)))))

(defmacro symbolfy (str)
  `(intern (string-upcase ,str)))

(defun test-result (actual expected)
  (if (equal actual expected)
      'PASSED
      'FAILED))

(defun err-result (expected condition)
  (if (eq expected :err)
      'passed
      (format nil "ERROR :: ~a" condition)))

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
                           (err-result ,expected-result c))))))
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

