;;; test-more.el --- Test framework like Perl's Test::More

;; Copyright (C) 2011  by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
;; Version: 0.1

;; EL-TEST-MORE is freely distributable under the MIT License
;; (http://www.opensource.org/licenses/mit-license).

;;; Commentary:

;; test-more.el
;;
;; This package respects `cl-test-more' developed by fukamachi
;;   - http://github.com/fukamachi/cl-test-more
;;

;;; History:


;;; Code:

(eval-when-compile
  (require 'cl))

(defvar test-more:plan :unspecified)
(defvar test-more:counter 0)
(defvar test-more:failed 0)
(defvar test-more:default-test-function #'equal)
(defvar test-more:tests nil)
;;(defvar *test-result-output* *standard-output*)

(defvar test-more:buffer-name "*test-more*")

(defvar test-more:symbol-names
  '(
    "ok"
    "is"
    "isnt"
    "diag"
    "is-type"
    "like"
    "plan"
    "pass"
    "fail"
    "finalize"
    )
  "Symbols inported by calling `test-more:import'")

(defun test-more:import ()
  (dolist (name test-more:symbol-names)
    (let ((real (intern (format "%s:%s" "test-more" name)))
          (alias (intern (format "%s" name))))
      (fset alias real))))

(defun test-more:init ()
  (with-current-buffer (get-buffer-create test-more:buffer-name)
    (erase-buffer)))

(test-more:init)

(defmacro test-more:format (fmt &rest args)
  (let ((str (gensym)))
    `(with-current-buffer (get-buffer ,test-more:buffer-name)
       (let ((,str (format ,fmt ,@args)))
         (if noninteractive
             (princ ,str)
           (insert ,str))))))

(defun test-more:plan (num)
  (setf test-more:plan num)
  (when num
    (test-more:format "1..%d\n" num)))

(defun test-more:finalize ()
  (cond
   ((eq test-more:plan :unspecified)
    (test-more:format "# Tests were run but no plan was declared\n"))
   ((and test-more:plan (not (= test-more:counter test-more:plan)))
    (test-more:format "#  Looks like you planned %d tests but ran %d\n"
                      test-more:plan test-more:counter)))
  (when (< 0 test-more:failed)
    (test-more:format "#  Looks like you failed %d tests of %d run\n"
                      test-more:failed test-more:counter))
  (unless noninteractive
    (pop-to-buffer test-more:buffer-name))
  (setf test-more:plan :unspecified test-more:counter 0 test-more:failed 0))

(defun test-more:test (got expected desc &optional test)
  (incf test-more:counter)
  (let* ((res (funcall (or test test-more:default-test-function)
                       got expected)))
    (test-more:format "%sok %d - %s%s\n"
                      (if res "" "not ")
                      test-more:counter desc
                      (or (and test-more:todo-desc
                               (format " # TODO %s" test-more:todo-desc))
                          ""))
    (when (not res)
      (incf test-more:failed)
      (if test-more:todo-desc
          (test-more:format "#  Failed (TODO) test '%s'\n"
                            test-more:todo-desc)))
    res))

(defun test-more:ok (test &optional desc)
  (test-more:test (not (null test)) t desc))

(defun test-more:is (got expected &optional desc)
  (or (test-more:test got expected desc)
      (test-more:format "#    got: %s   expected: %s\n" got expected)))

(defun test-more:isnt (got expected &optional desc)
  (or (test-more:test got expected desc)
      (test-more:format "#    got: %s   not expected: %s\n" got expected)))

(defun test-more:diag (desc)
  (test-more:format "# %s" desc))

(defun test-more:is-type (got expected-type &optional desc)
  (or (test-more:test (typep got expected-type) t desc)
      (test-more:format "#    got: %s    expected type: %s\n" got expected-type)))

(defun test-more:like (got regex &optional desc)
  (or (test-more:test (not (null (string-match regex got))) t desc)
      (test-more:format "#    got: %s    like: %s\n" got regexp)))

(defmacro test-more:skip (why how-many &rest body)
  (let ((i (gensym)))
    `(dotimes (,i (or ,how-many 1))
       (incf test-more:counter)
       (test-more:format "ok %d # skip: %s" test-more:counter ,why))))

(defvar test-more:todo-desc nil
  "Description of TODO(likes Test::More $TODO)")

(defmacro test-more:todo (msg &rest body)
  `(let ((test-more:todo-desc ,msg))
     ,@body))

(defmacro test-more:is-print (got expected &optional desc)
  (let ((res (gensym)))
    `(let ((,res (with-output-to-string ,got)))
       (test-more:test ,res ,expected ,desc))))

(defun test-more:pass (desc)
  (test-more:test t t desc))

(defun test-more:fail (desc)
  (test-more:test t nil desc))

(defmacro test-more:is-error (form &optional desc)
  (let ((err (gensym)))
    `(let ((,err (ignore-errors
                   ,form
                   t)))
       (test-more:test ,err nil ,desc))))

(provide 'test-more)
;;; test-more.el ends here
