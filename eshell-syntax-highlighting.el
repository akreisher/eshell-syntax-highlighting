;;; eshell-syntax-highlighting.el --- Highlight eshell commands  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Alex Kreisher

;; Author: Alex Kreisher <akreisher18@gmail.com>
;; Version: 0.6
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
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
;; Provides syntax highlighting for Eshell.
;;
;; Highlights commands as the user types to validate commands and syntax.
;;


;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'esh-mode)
  (require 'eshell)
  (require 'em-alias)
  (require 'em-dirs))


(require 'esh-util)
(require 'em-alias)
(require 'em-prompt)


(defgroup eshell-syntax-highlighting nil
  "Faces used to highlight the syntax of Eshell commands."
  :tag "Eshell Syntax Highlighting"
  :group 'eshell)

(defcustom eshell-syntax-highlighting-highlight-elisp t
  "Whether to natively parse Emacs Lisp through a temporary buffer."
  :type 'boolean
  :group 'eshell-syntax-highlighting)


(defcustom eshell-syntax-highlighting-highlight-in-remote-dirs nil
  "Whether to perform syntax highlighting in remote directories."
  :type 'boolean
  :group 'eshell-syntax-highlighting)


(defface eshell-syntax-highlighting-default-face
  '((t :inherit default))
  "Default face for Eshell commands."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-envvar-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for environment variables in an Eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-comment-face
  '((t :inherit font-lock-comment-face))
  "Face used for comments in an Eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-delimiter-face
  '((t :inherit font-lock-operator-face))
  "Face used for delimiters in an Eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-option-face
  '((t :inherit font-lock-constant-face))
  "Face used for options in an Eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-string-face
  '((t :inherit font-lock-string-face))
  "Face used for quoted strings in Eshell arguments."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-shell-command-face
  '((t :inherit success))
  "Face used for valid shell in an Eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-builtin-command-face
  '((t :inherit eshell-syntax-highlighting-shell-command-face))
  "Face used for a builtin Eshell command."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-lisp-function-face
  '((t :inherit font-lock-function-name-face))
  "Face used for Emacs Lisp functions."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-alias-face
  '((t :inherit eshell-syntax-highlighting-shell-command-face))
  "Face used for Eshell aliases."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-invalid-face
  '((t :inherit error))
  "Face used for invalid Eshell commands."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-directory-face
  '((t :inherit font-lock-type-face))
  "Face used for directories in command position if ‘eshell-cd-on-directory’ is t."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-file-arg-face
  '((t :underline t))
  "Face used for command arguments which are existing files."
  :group 'eshell-syntax-highlighting)

(defface eshell-syntax-highlighting-command-substitution-face
  '((t :inherit font-lock-escape-face))
  "Face for $ command substitution delimiters."
  :group 'eshell-syntax-highlighting)


(defvar eshell-syntax-highlighting--word-boundary-regexp "[^[:space:]&|;$'\"]*")

(defmacro eshell-syntax-highlighting--executable-find (command)
  "Check if COMMAND is on the variable `exec-path'."
  (if (< emacs-major-version 27)
      `(executable-find ,command)
    `(executable-find ,command t)))

(defun eshell-syntax-highlighting--goto-string-end (quote end)
  "Find end of string marked by QUOTE before END."
  (goto-char (or (eshell-find-delimiter quote quote end nil t) end))
  (if (eq (char-after) quote) (forward-char)))

(defun eshell-syntax-highlighting--escaped-p (&optional point escaped)
  "Return t if char at POINT is escaped, with ESCAPED as prev escape state."
  (let ((point (or point (point))))
    (if (eq (char-before point) ?\\)
        (eshell-syntax-highlighting--escaped-p (- point 1) (not escaped))
      escaped)))

(defun eshell-syntax-highlighting--highlight (beg end type)
  "Highlight word from BEG to END based on TYPE."
  (set-text-properties beg end nil nil)
  (let ((face
         (cl-case type
           (default 'eshell-syntax-highlighting-default-face)
           (command 'eshell-syntax-highlighting-shell-command-face)
           (builtin 'eshell-syntax-highlighting-builtin-command-face)
           (alias 'eshell-syntax-highlighting-alias-face)
           (lisp-function 'eshell-syntax-highlighting-lisp-function-face)
           (string 'eshell-syntax-highlighting-string-face)
           (invalid 'eshell-syntax-highlighting-invalid-face)
           (envvar 'eshell-syntax-highlighting-envvar-face)
           (directory 'eshell-syntax-highlighting-directory-face)
           (comment 'eshell-syntax-highlighting-comment-face)
           (delimiter 'eshell-syntax-highlighting-delimiter-face)
           (option 'eshell-syntax-highlighting-option-face)
           (file-arg 'eshell-syntax-highlighting-file-arg-face)
           (substitution 'eshell-syntax-highlighting-command-substitution-face)
           (t 'eshell-syntax-highlighting-default-face))))
    (add-face-text-property beg end face)))

(defvar eshell-syntax-highlighting--indirect-elisp-buffer nil)

(defvar eshell-syntax-highlighting-elisp-buffer-setup-hook nil
  "Hook run to configure syntax highlighting in an indirect ELisp buffer.")

(defun eshell-syntax-highlighting--get-indirect-elisp-buffer ()
  "Return the indirect buffer for Emacs Lisp syntax highlighting."
  (if (buffer-live-p eshell-syntax-highlighting--indirect-elisp-buffer)
      eshell-syntax-highlighting--indirect-elisp-buffer
    (with-current-buffer
        (setq-local eshell-syntax-highlighting--indirect-elisp-buffer
                    (make-indirect-buffer
                     (current-buffer)
                     (generate-new-buffer-name
                      (concat " " (buffer-name) "-esh-elisp-indirect"))))
      (setq-local delay-mode-hooks t)
      (let ((change-major-mode-hook nil)
            (after-change-major-mode-hook nil))
        (emacs-lisp-mode))
      (setq-local font-lock-dont-widen t)
      (setq-local font-lock-support-mode nil)
      (run-hooks 'eshell-syntax-highlighting-elisp-buffer-setup-hook))
    eshell-syntax-highlighting--indirect-elisp-buffer))

(defun eshell-syntax-highlighting--highlight-elisp (beg end)
  "Highlight Emacs Lisp in region (BEG, END) through an indirect buffer."
  (when eshell-syntax-highlighting-highlight-elisp
    (let ((elisp-end (or
                      (condition-case nil
                          (scan-sexps beg 1)
                        (scan-error nil))
                      end)))
      (with-current-buffer (eshell-syntax-highlighting--get-indirect-elisp-buffer)
        (narrow-to-region beg elisp-end)
        (font-lock-fontify-region beg elisp-end))
      (goto-char elisp-end))))

(defun eshell-syntax-highlighting--highlight-command-substitution (beg-symbol end-symbol end)
  "Highlight a command between BEG-SYMBOL and END-SYMBOL until END."
  (forward-char)
  (eshell-syntax-highlighting--highlight (- (point) 1) (point) 'substitution)
  (eshell-syntax-highlighting--parse-and-highlight
   'command
   (or (eshell-find-delimiter beg-symbol end-symbol end) end))
  (when (eq (char-after) end-symbol)
    (forward-char)
    (eshell-syntax-highlighting--highlight (- (point) 1) (point) 'substitution)))

(defvar eshell-syntax-highlighting--substitution-start-regexp "\\$\\(?:#\\|@\\)?[0-9a-zA-Z*${\(<'\"]")

(defun eshell-syntax-highlighting--highlight-substitution (end)
  "Highlight a dollar substitution until END."
  (let ((start (point)))
    (when (eq (char-after) ?$)
      (forward-char)
      (when (or (eq (char-after) ?@)
                (eq (char-after) ?#))
        (forward-char)))
    (cond
     ;; Command substitutions.
     ((eq (char-after) ?{)
      (eshell-syntax-highlighting--highlight start (point) 'substitution)
      (eshell-syntax-highlighting--highlight-command-substitution ?{ ?} end))
     ((eq (char-after) ?<)
      (eshell-syntax-highlighting--highlight start (point) 'substitution)
      (eshell-syntax-highlighting--highlight-command-substitution ?< ?> end))
     ;; Elisp substitutions.
     ((eq (char-after) ?\()
      (eshell-syntax-highlighting--highlight start (point) 'substitution)
      (eshell-syntax-highlighting--highlight-elisp (point) end))
     ;; Variable substitutions.
     ((or (eq (char-after) ?\') (eq (char-after) ?\"))
      (eshell-syntax-highlighting--highlight start (point) 'envvar)
      (eshell-syntax-highlighting--highlight-string (char-after) end 'envvar))
     ((looking-at "\\([0-9*$]\\|[[:alpha:]][[:alnum:]-_]*\\)")
      (goto-char (min (match-end 0) end))
      (eshell-syntax-highlighting--highlight start (point) 'envvar))))

  ;; Handle an arbitrary number of variable indexing (e.g. $my_var[0][1]...)
  (while (and (< (point) end) (eq (char-after) ?\[))
    (eshell-syntax-highlighting--highlight (point) (+ (point) 1) 'substitution)
    (let ((end-pos (or (eshell-find-delimiter ?\[ ?\] end) end)))
      (forward-char)
      (eshell-syntax-highlighting--highlight (point) end-pos 'default)
      (goto-char end-pos)
      (when (eq (char-after) ?\])
        (eshell-syntax-highlighting--highlight end-pos (+ end-pos 1) 'substitution)
        (forward-char)))))


(defun eshell-syntax-highlighting--highlight-with-substitutions (beg end type)
  "Highlight (BEG, END) as TYPE, and highlight found substitutitions."
  (eshell-syntax-highlighting--highlight beg end type)
  (let ((curr-point (point)))
    (goto-char beg)
    (while (and (< (point) end)
                (re-search-forward eshell-syntax-highlighting--substitution-start-regexp end t)
                (not (eshell-syntax-highlighting--escaped-p (match-beginning 0))))
      (goto-char (match-beginning 0))
      (eshell-syntax-highlighting--highlight-substitution end))
    (goto-char curr-point)))

(defun eshell-syntax-highlighting--highlight-string (quote-char end &optional face)
  "Highlight a string with QUOTE-CHAR until END with FACE (default string)."
  (let ((beg (point))
        (face (or face 'string)))
    (eshell-syntax-highlighting--goto-string-end quote-char end)
    (if (eq quote-char ?\")
        ;; Double quotes can have nested substitutions.
        (eshell-syntax-highlighting--highlight-with-substitutions beg (point) face)
      (eshell-syntax-highlighting--highlight beg (point) face))))


(defun eshell-syntax-highlighting--highlight-filename (beg end)
  "Highlight argument file in region (BEG, END)."
  ;; HACK: Handle single $, which should not count as a substitution.
  (when (eq (char-after) ?$) (forward-char))
  (re-search-forward eshell-syntax-highlighting--word-boundary-regexp (min end (line-end-position)))
  (eshell-syntax-highlighting--highlight
   beg (point)
   (if (and
        (not (string-equal (match-string 0) ""))
        (file-exists-p (match-string 0)))
       'file-arg
     'default)))

(defun eshell-syntax-highlighting--highlight-special-reference (end)
  "Highlight a special argument reference starting with #< up til END."
  (forward-char)
  (eshell-syntax-highlighting--highlight (- (point) 1) (+ (point) 1) 'substitution)
  (let ((end-pos (or (eshell-find-delimiter ?< ?> end) end)))
    (forward-char)
    ;; TODO: Highlight if buffer/process exists.
    (eshell-syntax-highlighting--highlight-with-substitutions (point) end-pos 'default)
    (goto-char end-pos))
  (when (eq (char-after) ?>)
    (forward-char)
    (eshell-syntax-highlighting--highlight (- (point) 1) (point) 'substitution)))

(defvar eshell-syntax-highlighting--control-flow-commands '("if" "unless" "while" "until"))

(defun eshell-syntax-highlighting--parse-command (beg end command)
  "Parse COMMAND in region (BEG, END) and highlight."
  (let ((next-expected
         (cond

          ;; For loop with in (e.g. for my_var in (1 2 3) ...)
          ((and (string-equal "for" command)
                (looking-at (format "\\s-+\\(%s\\)\\s-+\\(in\\)\\s-+"
                                    eshell-syntax-highlighting--word-boundary-regexp)))
           (eshell-syntax-highlighting--highlight beg (point) 'builtin)
           (eshell-syntax-highlighting--highlight (match-beginning 1) (match-end 1) 'envvar)
           (eshell-syntax-highlighting--highlight (match-beginning 2) (match-end 2) 'builtin)
           (goto-char (match-end 0))
           'argument)

          ;; Other control flow
          ((member command eshell-syntax-highlighting--control-flow-commands)
           (eshell-syntax-highlighting--highlight beg (point) 'builtin)
           ;; TODO: Parse conditional here
           'argument)

          ;; Command wrappers (sudo, time)
          ((string-match "^\\(\\*\\|eshell/\\)?\\(sudo\\|time\\)$" command)
           (eshell-syntax-highlighting--highlight
            beg (point)
            (if (and (match-string 1 command)
                     (string-equal (match-string 1 command) "eshell/"))
                'lisp-function
              'command))
           'command)

          ;; Executable file
          ((and (string-match-p ".*/.+" command)
                (file-regular-p command)
                (file-executable-p command))
           (eshell-syntax-highlighting--highlight beg (point) 'command)
           'argument)

          ;; Explicit external command
          ((and (not (string-equal command ""))
                (char-equal eshell-explicit-command-char (aref command 0))
                (eshell-syntax-highlighting--executable-find (substring command 1 nil)))
           (eshell-syntax-highlighting--highlight beg (point) 'command)
           'argument)

          ;; Eshell alias
          ((eshell-lookup-alias command)
           (eshell-syntax-highlighting--highlight beg (point) 'alias)
           'argument)

          ;; Built-in
          ((functionp (intern (concat "eshell/" command)))
           (eshell-syntax-highlighting--highlight beg (point) 'builtin)
           'argument)

          ;; Prioritized lisp function
          ((and eshell-prefer-lisp-functions
                (functionp (intern command)))
           (eshell-syntax-highlighting--highlight beg (point) 'lisp-function)
           'argument)

          ;; Executable
          ((eshell-syntax-highlighting--executable-find command)
           (eshell-syntax-highlighting--highlight beg (point) 'command)
           'argument)

          ;; Lisp function
          ((functionp (intern command))
           (eshell-syntax-highlighting--highlight beg (point) 'lisp-function)
           'argument)


          ;; Directory for cd
          ((and eshell-cd-on-directory
                (file-directory-p command))
           (eshell-syntax-highlighting--highlight beg (point) 'directory)
           'argument)

          ;; Invalid command
          (t
           (eshell-syntax-highlighting--highlight beg (point) 'invalid)
           'argument))))
    (eshell-syntax-highlighting--parse-and-highlight next-expected end)))

(defun eshell-syntax-highlighting--parse-and-highlight (expected end)
  "Parse and highlight from point until END, expecting token of type EXPECTED."
  ;; Whitespace
  (when (re-search-forward "\\s-*" end t)
    (eshell-syntax-highlighting--highlight
     (match-beginning 0) (match-end 0) 'default))

  (let ((beg (point)))
    (cond
     ;; Exit at eol
     ((eolp) nil)
     ((>= beg end) nil)

     ;; Command Block
     ((eq (char-after) ?\{)
      (eshell-syntax-highlighting--highlight-command-substitution ?\{ ?\} end)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Redirection
     ((and (eq expected 'argument)
           (looking-at "[0-9&]?>+\\(?:&[0-9]?\\)?\\s-*"))
      (goto-char (match-end 0))
      (eshell-syntax-highlighting--highlight beg (point) 'delimiter)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Comments
     ((looking-at "#\\(?:[^<']\\|\\'\\)")
      (eshell-syntax-highlighting--highlight beg end 'comment))

     ;; Line-wrapping backslash
     ((looking-at "\\\\\n")
      (goto-char (min end (match-end 0)))
      (eshell-syntax-highlighting--highlight beg (point) 'default)
      (eshell-syntax-highlighting--parse-and-highlight expected end))

     ;; Delimiters
     ((looking-at "\\(\\(|\\|&\\|;\\)+\\s-*\\)+")
      (goto-char (min end (match-end 0)))
      (if (eq expected 'command)
          (eshell-syntax-highlighting--highlight beg (point) 'invalid)
        (eshell-syntax-highlighting--highlight beg (point) 'delimiter))
      (eshell-syntax-highlighting--parse-and-highlight 'command end))

     ;; Quoted or parenthesized Emacs Lisp
     ((looking-at-p eshell-lisp-regexp)
      (eshell-syntax-highlighting--highlight-elisp beg end)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Buffer/marker/process
     ((looking-at "#<")
      (eshell-syntax-highlighting--highlight-special-reference end)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Commands
     ((eq expected 'command)
      (cond

       ;; Environment variable definition
       ((looking-at "[[:alpha:]_][[:alnum:]_]*=")
        (goto-char (min end (match-end 0)))
        (cond
         ((eq (char-after) ?\")
          (eshell-syntax-highlighting--goto-string-end ?\" end)
          (eshell-syntax-highlighting--highlight-with-substitutions beg (point) 'envvar))
         ((eq (char-after) ?\')
          (eshell-syntax-highlighting--goto-string-end ?\' end)
          (eshell-syntax-highlighting--highlight beg (point) 'envvar))
         (t (re-search-forward
             eshell-syntax-highlighting--word-boundary-regexp (min end (line-end-position)))
            (eshell-syntax-highlighting--highlight beg (point) 'envvar)))
        (eshell-syntax-highlighting--parse-and-highlight 'command end))

       ;; Command string
       (t
        (re-search-forward eshell-syntax-highlighting--word-boundary-regexp (min end (line-end-position)))
        (eshell-syntax-highlighting--parse-command beg end (match-string-no-properties 0)))))

     ;; Quoted strings
     ((or (eq (char-after) ?\') (eq (char-after) ?\"))
      (eshell-syntax-highlighting--highlight-string (char-after) end)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Options
     ((eq (char-after) ?-)
      (re-search-forward eshell-syntax-highlighting--word-boundary-regexp (min end (line-end-position)))
      (eshell-syntax-highlighting--highlight beg (point) 'option)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Argument $ substitution
     ((and (looking-at-p eshell-syntax-highlighting--substitution-start-regexp)
           (not (eshell-syntax-highlighting--escaped-p)))
      (eshell-syntax-highlighting--highlight-substitution end)
      (eshell-syntax-highlighting--parse-and-highlight 'argument end))

     ;; Arguments
     (t (eshell-syntax-highlighting--highlight-filename beg end)
        (eshell-syntax-highlighting--parse-and-highlight 'argument end)))))


(defmacro eshell-syntax-highlighting--command-running-p ()
  "Return non-nil if a foreground command is currently running."
  (if (>= emacs-major-version 30)
      '(eshell-head-process)
    'eshell-current-command))


(defun eshell-syntax-highlighting--enabled-p ()
  "Return whether syntax highlighting should be enabled in the current buffer."
  (and (eq major-mode 'eshell-mode)
       (not eshell-non-interactive-p)
       (not mark-active)
       (not (eshell-syntax-highlighting--command-running-p))
       (or
        eshell-syntax-highlighting-highlight-in-remote-dirs
        (not (file-remote-p default-directory)))))

(defun eshell-syntax-highlighting--enable-highlighting ()
  "Parse and highlight the command at the last Eshell prompt."
  (let ((beg (point))
        (non-essential t))
    (when (eshell-syntax-highlighting--enabled-p)
      (with-silent-modifications
        (save-excursion
          (goto-char (point-max))
          (eshell-previous-prompt 0)
          (eshell-syntax-highlighting--parse-and-highlight 'command (point-max))))
      ;; save-excursion marker is deleted when highlighting elisp,
      ;; so explicitly pop back to initial point.
      (goto-char beg))))


;;;###autoload
(define-minor-mode eshell-syntax-highlighting-mode
  "Toggle syntax highlighting for Eshell."
  :lighter nil
  :keymap nil
  (if (and eshell-syntax-highlighting-mode
           (eq major-mode 'eshell-mode)
           (not eshell-non-interactive-p))
      (add-hook 'post-command-hook
                #'eshell-syntax-highlighting--enable-highlighting nil t)
    (remove-hook 'post-command-hook
                 #'eshell-syntax-highlighting--enable-highlighting t)))

;;;###autoload
(define-globalized-minor-mode eshell-syntax-highlighting-global-mode
  eshell-syntax-highlighting-mode eshell-syntax-highlighting--global-on)

(defun eshell-syntax-highlighting--global-on ()
  "Enable eshell-syntax-highlighting only in appropriate buffers."
  (when (and (eq major-mode 'eshell-mode)
             (not eshell-non-interactive-p))
    (eshell-syntax-highlighting-mode +1)))

(provide 'eshell-syntax-highlighting)
;;; eshell-syntax-highlighting.el ends here
