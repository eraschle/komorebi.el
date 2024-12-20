;;; komorebi-help.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Erich Raschle
;;
;; Author: Erich Raschle <erichraschle@gmail.com>
;; Maintainer: Erich Raschle <erichraschle@gmail.com>
;; Created: Oktober 07, 2024
;; Modified: Oktober 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elyo/komorebi
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'komorebi-web)
(require 'komorebi-api)
(require 'cl-lib)
(require 'view)
(require 'subr-x)
(require 'org)


(defvar komorebi-help-buffer-name "*komorebi-help*"
  "The name of the buffer to display command help.")


(defun komorebi-help--clean-control-char (value)
  "Return VALUE cleaned of control characters and double quotes."
  (string-trim value "[ \t\n\r\"]*" "[ \t\n\r\"]*"))


(defun komorebi-help--messages (help-message)
  "Return a list of help messages from HELP-MESSAGE."
  (split-string help-message "\n" t "[ \t\n\r\']*"))

(defun komorebi-help--error (help-message)
  "Insert HELP-MESSAGE into the current buffer."
  (let* ((tip-regex ".*\\(tip: some similar sub commands exist:\\)")
         (lines (komorebi-help--messages help-message))
         (title (capitalize (cl-first lines)))
         (tips (cl-second lines))
         (tip-info (if (string-match tip-regex tips)
                       (string-trim-left (match-string 0 tips)) ""))
         (tips-command (string-remove-prefix tip-info tips)))
    (insert (format "* %s\n" title))
    (insert (format "%s\n\n" tip-info))
    (dolist (command (split-string tips-command "," t "[ \t\n\r\']*"))
      (let ((command-help (komorebi-api--execute command "--help")))
        (komorebi-help--success command command-help 2)
        (insert "\n")))))


(defun komorebi-help--insert-src-block (command)
  "Insert SRC-BLOCK of COMMAND into current buffer."
  (let ((exe (file-name-nondirectory komorebi-api-executable)))
    (insert "#+BEGIN_SRC sh\n")
    (when (string-prefix-p exe command)
      (setq command (string-trim (string-remove-prefix exe command))))
    (insert (format "%s %s\n" komorebi-api-executable command))
    (insert "#+END_SRC\n")))


(defun komorebi-help--success (command help-message indent)
  "Insert successful HELP-MESSAGE of COMMAND into current buffer.
INDENT is the level of org-heading."
  (insert (format "%s %s\n" (make-string indent ?*)
                  (capitalize command)))
  (let ((cmd-url (command-url (komorebi-web-cli-command command))))
    (insert (format "\n[[%s][Documentation]]\n\n" cmd-url)))
  (let ((option-found nil)
        (argument-found nil)
        (arg-regex "<\\([a-zA-Z]*\\)>"))
    (dolist (line (komorebi-help--messages help-message))
      (cond ((string-prefix-p "Usage:" line)
             (insert "\nUsage:\n")
             (let ((cmd (string-remove-prefix "Usage:" line)))
               (komorebi-help--insert-src-block (string-trim cmd)))
             (insert "\n"))
            ((and (not option-found) (string-prefix-p "Arguments:" line))
             (setq argument-found t)
             (insert (format "\n%s %s\n" (make-string (1+ indent) ?*) line)))
            ((and (not option-found) argument-found (string-match arg-regex line))
             (let* ((argument (match-string-no-properties 0 line))
                    (description (string-remove-prefix argument line)))
               (insert (format "%s %s\n" (make-string (+ 2 indent) ?*) argument))
               (insert (format "%s\n" (string-trim description)))))
            ((string-prefix-p "Options:" line)
             (setq option-found t)
             (insert (format "\n%s %s\n" (make-string (1+ indent) ?*) line)))
            ((and option-found (string-prefix-p "-" line))
             (let ((option-text (split-string line "  " t "[ \t\n\r]*")))
               (insert (format "%s %s\n" (make-string (+ 2 indent) ?*) (cl-first option-text)))
               (insert (format "%s\n" (string-join (cdr option-text) " ")))))
            (t (insert (format "%s\n" line)))))))


;;;###autoload
(defun komorebi-help-command (command)
  "Return help of komorebi COMMAND."
  (interactive (list (completing-read "Command: " (seq-map (lambda (cmd) (oref cmd name))
                                                           (komorebi-web-cli-commands)))))
  (let* ((help-message (komorebi-api--execute command "--help"))
         (cmd-buffer (get-buffer-create komorebi-help-buffer-name))
         (is-error (string-match "error" help-message))
         (org-startup-folded (if is-error 'show2levels 'showall)))
    (with-current-buffer cmd-buffer
      (view-mode-exit)
      (read-only-mode 0)
      (erase-buffer)
      (if is-error
          (komorebi-help--error help-message)
        (komorebi-help--success command help-message 1))
      (org-mode)
      (read-only-mode 'toggle)
      (view-mode-enter)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))


(defun komorebi-help--command-at-point ()
  "Return komorebi command at point."
  (let ((command (thing-at-point 'symbol t)))
    (when (null command)
      (user-error "No komorebi command found at point"))
    (string-remove-prefix "komorebi-help-" command)))


;;;###autoload
(defun komorebi-help-command-at-point ()
  "Return help of komorebi command at point."
  (interactive)
  (let ((command (komorebi-help--command-at-point)))
    (komorebi-help-command command)))

(provide 'komorebi-help)
;;; komorebi-help.el ends here
