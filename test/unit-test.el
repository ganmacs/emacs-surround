;;; test/unit-test  -- some unit test -*- lexical-binding: t -*-

;; Copyright (C) 2015 ganmacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; execute

;; emacs --directory . -batch -l test/unit-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'emacs-surround)

(defmacro test-emacs-surround (fn src expect)
  (let ((got (cl-gensym)))
    `(let ((got (with-temp-buffer
                  (insert ,src)
                  (goto-char (+ 1 (point-min)))
                  ,fn
                  (buffer-string))))
       (should (string= got ,expect)))))

;; insert

(ert-deftest test-eamcs-surround-insert ()
  "emacs-surround-insert WITHOUT region"
  (test-emacs-surround (emacs-surround-insert "'")
                       "Hello World" "'Hello' World"))

(ert-deftest test-eamcs-surround-insert-with-region ()
  "emacs-surround-insert WITH region"
  (let* ((input "Hello World")
         (got (with-temp-buffer
                (insert input)
                (goto-char (point-min))
                (set-mark (point))
                (goto-char (point-max))
                (activate-mark)
                (emacs-surround-insert "'")
                (buffer-string))))
    (should (string= got "'Hello World'"))))

;; line

(ert-deftest test-eamcs-surround-line ()
  "emacs-surround-line"
  (test-emacs-surround (emacs-surround-line "(")
                       "  Hello World" "  (Hello World)"))

;; delete

(ert-deftest test-eamcs-surround-delete ()
  "emacs-surround-delete WITHOUT region"
  (test-emacs-surround (emacs-surround-delete "(")
                       "(Hello World)" "Hello World"))

(ert-deftest test-eamcs-surround-delete-with-multiple-parens ()
  "emacs-surround-delete WITH multiple parens"
  (test-emacs-surround (emacs-surround-delete "(")
                       "(Hello (World) !)" "Hello (World) !"))

(ert-deftest test-eamcs-surround-delete-with-multiple-quote ()
  "emacs-surround-delete WITH multiple parens"
  (test-emacs-surround (emacs-surround-delete "\"")
                       "\"Hello \\\"World\\\"!\"" "Hello \\\"World\\\"!"))

(ert-deftest test-eamcs-surround-delete-with-multiple-line ()
  "emacs-surround-delete WITH multiple parens"
  (test-emacs-surround (emacs-surround-delete "(")
                       "(Hello [W\norld] !)" "Hello [W\norld] !"))

;; change

(ert-deftest test-eamcs-surround-change ()
  "emacs-surround-change"
  (test-emacs-surround (emacs-surround-change "'" "[")
                       "'Hello World'" "[Hello World]"))

(ert-deftest test-eamcs-surround-change-with-multiple-parens ()
  "emacs-surround-change WITH multiple parens"
  (test-emacs-surround (emacs-surround-change "(" "'")
                       "(Hello (World) !)" "'Hello (World) !'"))

(ert-deftest test-eamcs-surround-change-with-multiple-parens2 ()
  "emacs-surround-change WITH multiple parens"
  (let* ((input "(Hello (World) aaaa!)")
         (got (with-temp-buffer
                (insert input)
                (goto-char (- (point-max) 3))
                (emacs-surround-change "(" "'")
                (buffer-string))))
    (should (string= got "'Hello (World) aaaa!'"))))

(ert-deftest test-eamcs-surround-change-with-multiple-lines ()
  "emacs-surround-change WITH multiple lines"
  (test-emacs-surround (emacs-surround-change "(" "[")
                       "(Hello \nWorld)" "[Hello \nWorld]"))

(ert-deftest test-eamcs-surround-change-with-multiple-quote ()
  "emacs-surround-change WITH multiple parens"
  (test-emacs-surround (emacs-surround-change "\"" "(")
                       "\"Hello \\\"World\\\"!\"" "(Hello \\\"World\\\"!)"))

;; customize

(ert-deftest test-eamcs-surround-change-for-customize ()
  "emacs-surround-change for customize"
  (let ((emacs-surround-alist (cons '("b" . ("{ " . " }")) emacs-surround-alist)))
    (test-emacs-surround (emacs-surround-change "{" "b")
                         "{Hello world}" "{ Hello world }")))

;;; tests/unit-test.el ends here
