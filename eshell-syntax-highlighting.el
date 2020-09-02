;;; eshell-syntax-highlighting.el --- Highlight eshell command  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Alex Kreisher

;; Author: Alex Kreisher <akreisher18@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
  (require 'eshell))

(require 'esh-util)
(require 'misc)
(require 'seq)

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
		 '((t :foreground "gray" ))
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
  (cond
   ((eq type 'envvar)
	(add-face-text-property beg end 'eshell-syntax-highlighting-envvar-face))
   ((eq type 'command)
	(add-face-text-property beg end 'eshell-syntax-highlighting-shell-command-face))
   ((eq type 'alias)
	(add-face-text-property beg end 'eshell-syntax-highlighting-alias-face))
   ((eq type 'lisp)
	(add-face-text-property beg end 'eshell-syntax-highlighting-lisp-function-face))
   ((eq type 'directory)
	(add-face-text-property beg end 'eshell-syntax-highlighting-directory-face))
   ((eq type 'comment)
	(add-face-text-property beg end 'eshell-syntax-highlighting-comment-face))
   ((eq type 'invalid)
	(add-face-text-property beg end 'eshell-syntax-highlighting-invalid-face))
   (t
	(add-face-text-property beg end 'eshell-syntax-highlighting-default-face))))


(defun eshell-syntax-highlighting--parse-and-highlight (expected)
  "Parse and highlight words."

  ;; Skip whitespace
  (when (looking-at "\\s-*") (goto-char (match-end 0)))          

  (let ((beg (point)))
	(cond
	 ;; Exit at eob
	 ((eobp) nil)

	 ;; Comments
	 ((looking-at "#")
	  (eshell-syntax-highlighting--highlight beg (point-max) 'comment))

	 ;; Line-wrapping backslash
	 ((looking-at "\\\\\n")
	  (goto-char (match-end 0))
	  (eshell-syntax-highlighting--highlight beg (point) 'default)
	  (eshell-syntax-highlighting--parse-and-highlight expected))

	 ;; Delimiters
	 ((looking-at "\\(&\\||\\|;\\)")
	  (goto-char (match-end 0))
	  (if (eq 'expected 'command)
		  (eshell-syntax-highlighting--highlight beg (point) 'invalid)
		(eshell-syntax-highlighting--highlight beg (point) 'default))
	  (eshell-syntax-highlighting--parse-and-highlight 'command))

	 ;; Commands
	 ((eq expected 'command)
	  (search-forward-regexp "\\S-*" (line-end-position))
	  (let ((word (match-string-no-properties 0)))
		(cond
		 ;; Environment variabale
		 ((string-match "[a-zA-Z0-9_]+=.*" word)
		  (eshell-syntax-highlighting--highlight beg (point) 'envvar)
		  (eshell-syntax-highlighting--parse-and-highlight expected))

		 ;; Forced external command
		 ((and (string-prefix-p "*" word)
			   (executable-find (substring word 1 nil)))
		  (eshell-syntax-highlighting--highlight beg (point) 'command)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument))

		 ;; Prioritized lisp function
		 ((and eshell-prefer-lisp-functions (functionp (intern word)))
		  (eshell-syntax-highlighting--highlight beg (point) 'lisp)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument)) 
		 
		 ;; Executable
		 ((or (executable-find word)
			  (and (file-regular-p word) (file-executable-p word)))
			(eshell-syntax-highlighting--highlight beg (point) 'command)
			(eshell-syntax-highlighting--parse-and-highlight 'argument))
			
		 ;; Eshell aliases
		 ((or (seq-contains-p (eshell-alias-completions "") word)
			 (functionp (intern (concat "eshell/" word))))
		  (eshell-syntax-highlighting--highlight beg (point) 'alias)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument))
		 
		 ;; Lisp
		 ((functionp (intern word))
		  (eshell-syntax-highlighting--highlight beg (point) 'lisp)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument))

		 ;; Parenthesized lisp
		 ((string-prefix-p "(" word)
		  (eshell-syntax-highlighting--highlight beg (point-max) 'default)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument))
		
		 ;; Directory for cd
		 ((and eshell-cd-on-directory (file-directory-p word))
		  (eshell-syntax-highlighting--highlight beg (point) 'directory)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument))

		 ;; Invalid command
		 (t
		  (eshell-syntax-highlighting--highlight beg (point) 'invalid)
		  (eshell-syntax-highlighting--parse-and-highlight 'argument)))))

	 ((eq expected 'argument)
	  (search-forward-regexp "\\S-*" (line-end-position))
	  (eshell-syntax-highlighting--highlight beg (point) 'default)
	  (eshell-syntax-highlighting--parse-and-highlight 'argument)))))


(defun eshell-syntax-highlighting--highlight-command ()
  "Parse and highlight command at the last eshell prompt."
  (save-excursion
	(goto-char eshell-last-output-end)
	(forward-line 0)
    (when (re-search-forward eshell-prompt-regexp (line-end-position) t)
	  (eshell-syntax-highlighting--parse-and-highlight 'command))))


(defun eshell-syntax-highlighting-enable ()
  "Enable highlighting of eshell commands."
  (interactive)
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (add-hook 'post-command-hook #'eshell-syntax-highlighting--highlight-command t t))))

(defun eshell-syntax-highlighting-disable ()
  "Enable highlighting of eshell commands."
  (interactive)
  (remove-hook 'eshell-mode-hook
			(lambda ()
			  (add-hook 'post-command-hook #'eshell-syntax-highlighting--highlight-command t t))))
    

(provide 'eshell-syntax-highlighting)
;;; eshell-syntax-highlighting.el ends here
