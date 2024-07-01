;;; eshell-syntax-highlighting-tests.el --- Tests for eshell highlighting  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Alex Kreisher

;; Author: Alex Kreisher <akreisher18@gmail.com>
;; URL: https://github.com/akreisher/eshell-syntax-highlighting

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'eshell-syntax-highlighting)


(defun esht-test (command)
  "Test syntax highlighting on eshell COMMAND."
  (with-temp-buffer
    (let ((eshell-syntax-highlighting-highlight-elisp t)
          (eshell-syntax-highlighting-highlight-in-remote-dirs nil))
      (eshell-mode)
      (eshell-syntax-highlighting-mode)
      (goto-char (point-max))
      (insert command)
      (eshell-syntax-highlighting--enable-highlighting)
      (eshell-syntax-highlighting--assert-highlighted))))

(defun eshell-syntax-highlighting--assert-highlighted ()
  "Ensure all Eshell command text is highlighted."
  ;; TODO: Add assertions on faces and mock environment-dependent functions.
  (goto-char (point-max))
  (eshell-previous-prompt 0)
  (let ((end (point-max)))
    (while (< (point) end)
      (let ((next-change (or (next-property-change (point) nil end) end)))
        (should (get-text-property (point) 'font-lock-face))
        (goto-char next-change)))))


(ert-deftest esht-test-command ()
  (esht-test "echo hello")
  (esht-test "*echo hello")
  (esht-test "time ls"))

(ert-deftest esht-test-double-quotes ()
  (esht-test "echo \"hello\""))

(ert-deftest esht-test-single-quotes ()
  (esht-test "echo \'hello\'"))

(ert-deftest esht-test-delimiters ()
  (esht-test "echo hello; echo world; true && false || true"))

(ert-deftest esht-test-redirection ()
  (esht-test "echo hello >/dev/null")
  (esht-test "ls -la >>#<buffer my-dir")
  (esht-test "echo hello &1>2"))

(ert-deftest esht-test-pipe ()
  (esht-test "echo hello | cat"))

(ert-deftest esht-test-env-var ()
  (esht-test "echo $MYVAR")
  (esht-test "MYVAR=123 echo $\"MYVAR\"")
  (esht-test "echo $'MYVAR'[0]")
  (esht-test "echo $'MYVAR'[0]")
  (esht-test "echo $@SPLICE")
  (esht-test "echo $#LIST"))

(ert-deftest esht-test-command-substitution ()
  (esht-test "echo ${echo hello && echo \"world\"}"))

(ert-deftest esht-test-list-substitution ()
  (esht-test "echo ${echo hello && echo \"world\"}")
  (esht-test "cat \"$<echo 'hello' && echo ${echo world}>\"")
  (esht-test "echo $(message \"hello world\")"))

(ert-deftest esht-test-lisp ()
  (esht-test "(use-package eshell-syntax-highlighting :ensure t)"))

(ert-deftest esht-test-for-loop ()
  (esht-test "for i in #'(1 2 3) {echo \"i: $i\"}"))

(ert-deftest esht-test-comment ()
  (esht-test "ls -la # comment"))

(ert-deftest esht-test-newline ()
  (esht-test "ls -la \\\nmy_dir"))

(provide 'eshell-syntax-highlighting-tests)
;;; eshell-syntax-highlighting-tests.el ends here
