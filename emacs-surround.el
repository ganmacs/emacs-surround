;;; emacs-surround.el --- surround symbol or change surrouning char -*- lexical-binding: t -*-

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/emacs-surround
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

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

;;; Code:

(defgroup emacs-surround nil
  "surround.vim for Emacs"
  :group 'surround)

(defcustom emacs-surround-alist
  '((""   . (""  . ""))
    ("'"  . ("'" . "'"))
    ("("  . ("(" . ")"))
    (")"  . ("(" . ")"))
    ("{"  . ("{" . "}"))
    ("}"  . ("{" . "}"))
    ("["  . ("[" . "]"))
    ("]"  . ("[" . "]"))
    ("/"  . ("/" . "/"))
    ("\"" . ("\"" . "\"")))
  "Surround key list."
  :group 'surround)

(defvar emacs-surround-separator "^\s()[]:;,=.\n{}")

(defun emacs-surround-mark-region-line ()
  "Mark region at line."
  (back-to-indentation)
  (set-mark (point))
  (move-end-of-line 1))

(defun emacs-surround-mark-region-sep ()
  "Mark region with emacs-surround-separator."
  (skip-chars-backward emacs-surround-separator)
  (set-mark (point))
  (skip-chars-forward emacs-surround-separator))

(defun emacs-surround-same-count-p (str a b)
  "STR Count A, B appear number in STR."
  (cl-flet ((count-match-str (regex)
                             (with-temp-buffer
                               (insert str)
                               (goto-char (point-min))
                               (count-matches regex))))
    (not (= (count-match-str a)
            (count-match-str b)))))

(defun emacs-surround-quote-p (type)
  "Return quote string or not.
check TYPE."
  (let ((min (point-at-bol))
        (max (point-at-eol))
        (ppoint (if (eq type 'forward) (point) (- (point) 1))))
    (defun iter (i p)
      (if (<= min p max)
          (if (= (char-before p) 92) ; quote
              (iter (+ i 1) (- p 1))
            (= (mod i 2) 1))))
    (iter 0 ppoint)))

(defun emacs-surround-mark-between (prefix &optional suffix)
  "Wrap str with PREFIX and SUFFIX."
  (cl-flet ((search-prefix () (search-backward prefix (point-at-bol) nil 1))
            (search-suffix () (search-forward (or suffix prefix) (point-at-eol) nil 1)))
    (if (search-prefix)
        (progn
          (while (emacs-surround-quote-p 'forward) (search-prefix))
          (set-mark (point))
          (forward-char)
          (search-suffix)
          (while (if (string= prefix suffix)
                     (emacs-surround-quote-p 'backward)
                   (emacs-surround-same-count-p
                    (buffer-substring (region-beginning) (region-end))
                    prefix
                    suffix))
            (search-suffix))))))

(defun emacs-surround-get-alist (key)
  "Get list by emacs-surround-alit with KEY."
  (cdr (or (assoc key emacs-surround-alist)
           (assoc "" emacs-surround-alist))))

(defun emacs-surround-wrap (str prefix &optional suffix)
  "Wrap STR with PREFIX and SUFFIX(if suffix exists)."
  (concat prefix str (or suffix prefix)))

(defun emacs-surround-replace (str from to)
  "STR FROM TO.
(FROM)STR(FROM) -> (TO)STR(TO).
Switch FROM surrounding STR to TO"
  (let* ((f-prefix (car from)) (f-suffix (cdr from))
         (t-prefix (car to)) (t-suffix (cdr to))
         (regx (format "^%s\\(.*\\)%s$" f-prefix f-suffix)))
    (if (string-match regx str)
        (let ((match (match-string 1 str)))
          (emacs-surround-wrap match t-prefix t-suffix)))))

(defun emacs-surround-cut-region ()
  "Cut region at point."
  (buffer-substring (region-beginning) (region-end)))

(defun emacs-surround-helper (mark-fn prefix suffix)
  "Helper emacs-suround (inset|delte|line|change).
MARK-FN PREFIX SUFFIX."
  (let ((now (point)))
    (unless (use-region-p) (funcall mark-fn))
    (let* ((target-str (emacs-surround-cut-region))
           (replaced-str (emacs-surround-replace
                          target-str
                          (emacs-surround-get-alist prefix)
                          (emacs-surround-get-alist suffix))))
      (if replaced-str
          (progn
            (delete-region (region-beginning) (region-end))
            (insert replaced-str)
            (goto-char now))
        (message "not found prefix and suffix")))))

(defun emacs-surround-insert (str)
  "Insert surround word STR."
  (emacs-surround-helper 'emacs-surround-mark-region-sep "" str))

(defun emacs-surround-delete (str)
  "Delete surround word which is STR."
  (let ((s (emacs-surround-get-alist str)))
    (emacs-surround-helper (lambda () (emacs-surround-mark-between (car s) (cdr s)))
                           str "")))

(defun emacs-surround-line (str)
  "Wrap line with STR."
  (emacs-surround-helper 'emacs-surround-mark-region-line "" str))

(defun emacs-surround-change (to end)
  "Change suround string TO into END."
  (let ((s (emacs-surround-get-alist to)))
    (emacs-surround-helper (lambda () (emacs-surround-mark-between (car s) (cdr s)))
                           to end)))

;;;###autoload
(defun emacs-surround ()
  "Surround or Delete symbol etc.
if cmd1 is i(insert), surround at-point-symbol.
if cmd1 is d(delete), delete surround cmd2 char.
if cmd1 is l(line), surround line which point is here.
else change surround cmd1 to cmd2"
  (interactive)
  (let* ((cmd1 (char-to-string (read-char)))
         (_cmd2 (read-char))
         (cmd2 (char-to-string _cmd2)))
    (cond ((string= cmd1 "i") (emacs-surround-insert cmd2))
          ((string= cmd1 "d") (emacs-surround-delete cmd2))
          ((string= cmd1 "l") (emacs-surround-line cmd2))
          (t (if (= 13 _cmd2)              ;return
                 (emacs-surround-insert cmd1)
               (emacs-surround-change cmd1 cmd2))))))

(provide 'emacs-surround)

;;; emacs-surround.el ends here
