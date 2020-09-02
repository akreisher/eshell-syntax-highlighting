;;; eshell-syntax-highlighting.el --- Highlight eshell command  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Alex Kreisher

;; Author: Alex Kreisher <akreisher18@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: processes
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

;;; Commentary:
;;
;; Highlight eshell commands based on syntax.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'esh-mode)
  (require 'eshell)
  (require 'em-alias))

(require 'esh-util)


;;;###autoload
(progn
(defgroup eshell-syntax-highlighting nil
  "This module provides syntax-highlighting of user entered commands."
  :tag "Syntax highlighting"
  :group 'eshell-module))

(defface eshell-syntax-highlighting-default-face
		 '((t :inherit 'default ))
  "Default face for eshell commands."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-envvar-face
		 '((t :foreground "orange" ))
  "Face used for environment variables in an eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-comment-face
		 '((t :foreground "gray" :weight bold))
  "Face used for environment variables in an eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-string-face
		 '((t :inherit 'font-lock-string-face))
  "Face used for environment variables in an eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-shell-command-face
		 '((t :foreground "green" ))
  "Face used for valid shell in an eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-lisp-function-face
		 '((t :foreground "yellow" ))
  "Face used for elisp functions."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-alias-face
		 '((t :inherit 'eshell-syntax-highlighting-shell-command-face ))
  "Face used for eshell aliases."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-invalid-face
		 '((t :foreground "red" ))
  "Face used for invalid eshell commands."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-directory-face
		 '((t :foreground "cyan" ))
  "Face used for directory commands in an eshell command."
  :group 'eshell-syntax-highlighting)

(defun eshell-syntax-highlighting--highlight (beg end type)
  "Highlight word from beg to end based on type"
  (let ((face
		 (cond
		  ((eq type 'default) 'eshell-syntax-highlighting-default-face)
		  ((eq type 'command) 'eshell-syntax-highlighting-shell-command-face)
		  ((eq type 'alias) 'eshell-syntax-highlighting-alias-face)
		  ((eq type 'lisp) 'eshell-syntax-highlighting-lisp-function-face)
		  ((eq type 'string) 'eshell-syntax-highlighting-string-face)
		  ((eq type 'invalid) 'eshell-syntax-highlighting-invalid-face)
		  ((eq type 'envvar) 'eshell-syntax-highlighting-envvar-face)
		  ((eq type 'directory) 'eshell-syntax-highlighting-directory-face)
		  ((eq type 'comment) 'eshell-syntax-highlighting-comment-face)
		  (t 'eshell-syntax-highlighting-default-face))))
	(add-face-text-property beg end face)))


(defun eshell-syntax-highlighting--parse-command (beg command)
  "Parse a command and dispatch to highlighting and continued parsing."
  (cond
   ;; Environment variabale
   ((string-match "[a-zA-Z0-9_]+=.*" command)
	(eshell-syntax-highlighting--highlight beg (point) 'envvar)
	(eshell-syntax-highlighting--parse-and-highlight 'command))

   ;; Forced external command
   ((and (string-prefix-p "*" command)
		 (executable-find (substring command 1 nil)))
	(eshell-syntax-highlighting--highlight beg (point) 'command)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Prioritized lisp function
   ((and eshell-prefer-lisp-functions (functionp (intern command)))
	(eshell-syntax-highlighting--highlight beg (point) 'lisp)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Executable
   ((or (executable-find command)
		(and (file-regular-p command) (file-executable-p command)))
	(eshell-syntax-highlighting--highlight beg (point) 'command)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Eshell aliases
   ((or (eshell-lookup-alias command)
		(functionp (intern (concat "eshell/" command))))
	(eshell-syntax-highlighting--highlight beg (point) 'alias)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Lisp
   ((functionp (intern command))
	(eshell-syntax-highlighting--highlight beg (point) 'lisp)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Parenthesized lisp
   ((string-prefix-p "(" command)
	(eshell-syntax-highlighting--highlight beg (point-max) 'default)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Directory for cd
   ((and eshell-cd-on-directory (file-directory-p command))
	(eshell-syntax-highlighting--highlight beg (point) 'directory)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))

   ;; Invalid command
   (t
	(eshell-syntax-highlighting--highlight beg (point) 'invalid)
	(eshell-syntax-highlighting--parse-and-highlight 'argument))))


(defun eshell-syntax-highlighting--parse-and-highlight (expected)
  "Parse and highlight from point."

  ;; Skip whitespace
  (when (looking-at "\\s-*") (goto-char (match-end 0)))

  (let ((beg (point)))
	(cond
	 ;; Exit at eol
	 ((eolp) nil)

	 ;; Comments
	 ((looking-at "#")
	  (eshell-syntax-highlighting--highlight beg (point-max) 'comment))

	 ;; Line-wrapping backslash
	 ((looking-at "\\\\\n")
	  (goto-char (match-end 0))
	  (eshell-syntax-highlighting--highlight beg (point) 'default)
	  (eshell-syntax-highlighting--parse-and-highlight expected))

	 ;; Delimiters
	 ((looking-at "[&|;]")
	  (goto-char (match-end 0))
	  (if (eq 'expected 'command)
		  (eshell-syntax-highlighting--highlight beg (point) 'invalid)
		(eshell-syntax-highlighting--highlight beg (point) 'default))
	  (eshell-syntax-highlighting--parse-and-highlight 'command))

	 ;; Commands
	 ((eq expected 'command)
	  (search-forward-regexp "[^[:space:]&|;]*" (line-end-position))
	  (eshell-syntax-highlighting--parse-command beg (match-string-no-properties 0)))

	 (t
	  (cond

	   ;; Quoted string
	   ((looking-at "[\"']")
		(unless
			(re-search-forward
			 (concat "\\(\\`\\|[^\\\\]\\)" (match-string 0)) nil t)
		  (goto-char (point-max)))
		(eshell-syntax-highlighting--highlight beg (point) 'string)
		(eshell-syntax-highlighting--parse-and-highlight 'argument))

	   ;; Argument
	   (t
		(search-forward-regexp "[^[:space:]&|;]*" (line-end-position))
		(eshell-syntax-highlighting--highlight beg (point) 'default)
		(eshell-syntax-highlighting--parse-and-highlight 'argument)))))))


(defun eshell-syntax-highlighting--enable-highlighting ()
  "Parse and highlight the command at the last eshell prompt."
  (add-hook
   'post-command-hook
   (lambda ()
	 (when (eq major-mode 'eshell-mode)
	   (save-excursion
		 (goto-char eshell-last-output-end)
		 (forward-line 0)
		 (when (re-search-forward eshell-prompt-regexp (line-end-position) t)
		   (eshell-syntax-highlighting--parse-and-highlight 'command)))))))


(defun eshell-syntax-highlighting-enable ()
  "Enable highlighting of eshell commands."
  (interactive)
  (add-hook 'eshell-mode-hook 'eshell-syntax-highlighting--enable-highlighting))

(defun eshell-syntax-highlighting-disable ()
  "Disable highlighting of eshell commands."
  (interactive)
  (remove-hook 'eshell-mode-hook 'eshell-syntax-highlighting--enable-highlighting))



(provide 'eshell-syntax-highlighting)
;;; eshell-syntax-highlighting.el ends here
