;;; test-more.el --- Test framework like Perl's Test::More

;; Copyright (C) 2011, 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-test-more
;; Version: 0.1


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defvar test-more:output-strings nil)
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
    "skip"
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

(defun test-more:append-output-string (str)
  (push str test-more:output-strings))

(defmacro test-more:format (fmt &rest args)
  (let ((str (gensym))
        (func (gensym)))
    `(let ((,func (if noninteractive #'princ #'test-more:append-output-string)))
       (if test-more:subtest-p
           (funcall ,func "    "))
       (let ((,str (format ,fmt ,@args)))
         (funcall ,func ,str)))))

(defun test-more:plan (num)
  (setf test-more:plan num)
  (when num
    (test-more:format "1..%d\n" num)))

(defun test-more:fail-message (failed tests)
  (test-more:format "#  Looks like you failed %d tests of %d run\n"
                    failed tests))

(defun test-more:finalize ()
  (cond
   ((eq test-more:plan :unspecified)
    (test-more:format "# Tests were run but no plan was declared\n"))
   ((and test-more:plan (not (= test-more:counter test-more:plan)))
    (test-more:format "#  Looks like you planned %d tests but ran %d\n"
                      test-more:plan test-more:counter)))
  (when (< 0 test-more:failed)
    (test-more:fail-message test-more:failed test-more:counter))
  (with-current-buffer (get-buffer-create test-more:buffer-name)
    (erase-buffer)
    (insert (mapconcat #'identity (reverse test-more:output-strings) "")))
  (unless noninteractive
    (pop-to-buffer test-more:buffer-name))
  (setf test-more:plan :unspecified
        test-more:counter 0
        test-more:failed 0
        test-more:output-strings nil))

(defun test-more:increment-counter ()
  (if test-more:subtest-p
      (incf test-more:subtest-counter)
    (incf test-more:counter)))

(defun test-more:increment-failed ()
  (if test-more:subtest-p
      (incf test-more:subtest-failed)
    (incf test-more:failed)))

(defun test-more:get-counter ()
  (if test-more:subtest-p
      test-more:subtest-counter
    test-more:counter))

(defun test-more:test (got expected desc &optional notp test)
  (test-more:increment-counter)
  (let ((res (funcall (or test test-more:default-test-function)
                      got expected)))
    (if notp
        (setq res (not res)))
    (test-more:format "%sok %d%s%s\n"
                      (if res "" "not ")
                      (test-more:get-counter)
                      (if desc (format " - %s" desc) "")
                      (or (and test-more:todo-desc
                               (format " # TODO %s" test-more:todo-desc))
                          ""))
    (when (not res)
      (if test-more:subtest-p
          (setq test-more:subtest-failed-p t))
      (test-more:increment-failed)
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
  (or (test-more:test got expected desc t)
      (test-more:format "#    got: %s   not expected: %s\n" got expected)))

(defun test-more:diag (desc)
  (test-more:format "# %s\n" desc))

(defun test-more:is-type (got expected-type &optional desc)
  (or (test-more:test (typep got expected-type) t desc)
      (test-more:format "#    got: %s    expected type: %s\n" got expected-type)))

(defun test-more:like (got regexp &optional desc)
  (or (test-more:test (not (null (string-match regexp got))) t desc)
      (test-more:format "#    got: %s    like: %s\n" got regexp)))

(defun test-more:skip (why how-many)
  (dotimes (i (or how-many 1))
    (incf test-more:counter)
    (test-more:format "ok %d # skip: %s\n" test-more:counter why)))

(defvar test-more:todo-desc nil
  "Description of TODO(likes Test::More $TODO)")

(defmacro test-more:todo (msg &rest body)
  `(let ((test-more:todo-desc ,msg))
     ,@body))

(defmacro test-more:is-print (got expected &optional desc)
  (let ((res (gensym)))
    `(let ((,res (with-output-to-string ,got)))
       (test-more:test ,res ,expected ,desc))))

(defvar test-more:subtest-p nil
  "Flag of enable subtest")

(defvar test-more:subtest-failed-p nil
  "Flag of tests in subtest is failed")

(defvar test-more:subtest-counter 0)
(defvar test-more:subtest-failed  0)

(defmacro test-more:subtest (desc &rest body)
  `(let ((test-more:subtest-failed-p nil))
     (setq test-more:subtest-counter 0
           test-more:subtest-failed  0)
     (let ((test-more:subtest-p t))
       ,@body
       (test-more:format "1..%d\n" test-more:subtest-counter)
       (if (> test-more:subtest-failed 0)
           (test-more:fail-message test-more:subtest-failed
                                   test-more:subtest-counter)))
     (test-more:test (not test-more:subtest-failed-p) t ,desc)))

(defmacro test-more:done-testing (&rest body)
  `(progn
     ,@body
     (test-more:plan (test-more:get-counter))
     (test-more:finalize)))

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
