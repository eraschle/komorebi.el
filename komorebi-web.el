;;; komorebi-web.el --- Functions for Komorebi website -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Erich Raschle
;;
;; Author: Erich Raschle <erichraschle@gmail.com>
;; Maintainer: Erich Raschle <erichraschle@gmail.com>
;; Created: Oktober 08, 2024
;; Modified: Oktober 08, 2024
;; Version: 0.0.1
;; Keywords: abbrev extensions faces languages lisp local processes terminals tools
;; Homepage: https://github.com/elyo/komorebi-web
;; Package-Requires: ((emacs "29.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; some
;;
;;
;;; Code:

(require 'json)
(require 'url)
(require 's)


(defcustom komorebi-web-docs-url "https://lgug2z.github.io/komorebi"
  "The version of komorebi."
  :type 'string
  :group 'komorebi-web)


(defcustom komorebi-web-search-index
  "search/search_index.json"
  "URL of the search index for the komorebi website."
  :type 'string
  :group 'komorebi-web)


(defun komorebi-web-search-replace (regex replaccement)
  "Replace all occurrences of REGEX with REPLACCEMENT in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward regex nil t)
    (replace-match replaccement)))


(defun komorebi-web-load (url)
  "Read JSON CONTENT from the URL and return the corresponding ELISP object."
  (with-current-buffer (url-retrieve-synchronously url)
    (komorebi-web-search-replace "<[/]?\\(code\\|pre\\)>" "")
    (komorebi-web-search-replace "&lt;" "<")
    (komorebi-web-search-replace "&gt;" ">")
    (goto-char (point-min))
    (goto-char url-http-end-of-headers)
    (json-parse-buffer :object-type 'plist :array-type 'list)))


(defun komorebi-web--select-doc (docs)
  "Return the selected element from DOCS."
  (let ((answer (completing-read
                 "Select doc: "
                 (sort (mapcar (lambda (doc) (plist-get doc :title)) docs)
                       'string-lessp))))
    (seq-find (lambda (doc) (string= answer (plist-get doc :title))) docs)))

(defvar komorebi-web--content nil
  "Plist of the search index from the komorebi website.")


(defun komorebi-web-search-index-get ()
  "Return a plist of the search index from the komorebi website."
  (unless komorebi-web--content
    (let ((url (concat komorebi-web-docs-url "/" komorebi-web-search-index)))
      (setq komorebi-web--content (komorebi-web-load url))))
  komorebi-web--content)


(defun komorebi-web-docs ()
  "Return a list of all the docs from the komorebi website."
  (plist-get (komorebi-web-search-index-get) :docs))


(defun komorebi-web--args-doc (arg)
  "Return the LISP documentation for ARG."
  ;; (format "%s (:%s):"
  ;;         (upcase (plist-get arg :name))
  ;;         (upcase (plist-get arg :name))))
  (upcase (plist-get arg :name)))

(defun komorebi-web--args-doc-length (args)
  "Return the maximum length of the LISP documentation for ARGS."
  (let ((max-length (seq-max
                     (seq-map (lambda (arg)
                                (length (komorebi-web--args-doc arg)))
                              args))))
    (concat "%-" (number-to-string max-length) "s")))


(defclass komorebi-web-command ()
  ((name :initarg :name :type string)
   (location :initarg :location :type string)
   (text :initarg :text :type string)
   (description :initarg :description :type string)
   (usage :initarg :usage :type string)
   (arguments :initarg :arguments :type sequence)
   (options :initarg :options :type sequence))
  :documentation "Class for a komorebi command.")

(cl-defmethod lisp-description ((cmd komorebi-web-command))
  "Return the LISP function description for CMD."
  (let ((desc (oref cmd description)))
    (when (function-args-p cmd)
      (let ((max-name (komorebi-web--args-doc-length
                       (function-args-plist cmd))))
        (setq desc (format "%s\n%s"
                           desc
                           (string-join
                            (seq-map
                             (lambda (arg)
                               (format (concat max-name "\n   %s")
                                       (komorebi-web--args-doc arg)
                                       (plist-get arg :description)))
                             (function-args-plist cmd))
                            "\n")))))
    ;; (setq desc (format "%s\n\nUsage: %s"
    ;;                    desc
    ;;                    (oref cmd usage)))
    desc))


(cl-defmethod function-name ((cmd komorebi-web-command))
  "Return the URL for CMD."
  (let ((prefix (if (function-args-p cmd) "komorebi-api-" "komorebi-")))
    (concat prefix (oref cmd name))))

(cl-defmethod function-args-p ((cmd komorebi-web-command))
  "Return non-nil if CMD has arguments."
  (length> (function-args-plist cmd) 0))

(cl-defmethod function-args-description ((cmd komorebi-web-command))
  "Return the URL for CMD."
  (seq-map (lambda (arg)
             (format "%s: %s" (upcase (plist-get arg :name)) (plist-get arg :description)))
           (function-args-plist cmd)))

(cl-defmethod attribute-list ((cmd komorebi-web-command))
  "Return the URL for CMD."
  (if (function-args-p cmd)
      (format "&key %s"
              (string-join
               (seq-map
                (lambda (arg)
                  (plist-get arg :name))
                (function-args-plist cmd))
               " "))
    ""))

(cl-defmethod attribute-names ((cmd komorebi-web-command))
  "Return the URL for CMD."
  (if (function-args-p cmd)
      (format " %s" (string-join (seq-map (lambda (arg) (plist-get arg :name))
                                          (function-args-plist cmd)) " "))
    ""))


(cl-defmethod function-args-plist ((cmd komorebi-web-command))
  "Return the URL for CMD."
  (append (seq-map (lambda (arg)
                     (list :name (plist-get arg :name)
                           :description (plist-get arg :description)))
                   (oref cmd arguments))
          (seq-map (lambda (opt)
                     (list :name (plist-get opt :name)
                           :description (plist-get opt :description)))
                   (seq-filter (lambda (option) (not (equal (plist-get option :name) "help")))
                               (oref cmd options)))))

(cl-defmethod command-url ((cmd komorebi-web-command))
  "Return the URL for CMD."
  (concat komorebi-web-docs-url "/" (oref cmd location)))

(defun komorebi-web--ensure-not-empty-line ()
  "Delete all empty lines at the beginning of the current buffer."
  (goto-char (point-min))
  (while (and (not (eobp))
              (string-empty-p (string-trim (buffer-substring-no-properties (point-min) (line-end-position)))))
    (delete-line))
  (goto-char (point-min)))

(defun komorebi-web--arg-description (description)
  "Return the DESCRIPTION of ARGUMENT or OPTION."
  (let ((desc (replace-regexp-in-string
               "[ ]\{2,\}" "\n"
               (string-trim description))))
    (if (length> desc 80)
        (if (and (s-contains? "," desc) (string-match "\\(.*\\):\\(.*\\)" desc))
            (format "%s:\n%s"
                    (string-replace "[possible values:" "possible values:"(match-string 1 desc))
                    (string-join (seq-map (lambda (val)
                                            (if (string-prefix-p "- " val)
                                                val
                                              (format "- %s" val)))
                                          (string-split (match-string 2 desc) "," t "[ \t\n]*"))
                                 "\n"))
          ;; No possible values
          (string-join (string-split desc "\." t "[ \t\n]*") "\n"))
      desc)))


(defvar komorebi-web-cli-option-regex "\\(?:\\(?1:-[^ ,]\\),\\)[ ]+\\(?2:--[^ ]+\\)[ ]+\\(?3:<\\(?4:.*\\)>\\)?\\(?5:.*\\)"
  "Regex to match a CLI OPTION from text on the komorebi website.")

(defun komorebi-web--cli-options-p ()
  "Return non-nil if the current buffer contain a list of options."
  (goto-char (point-min))
  (search-forward "Options:" nil t))

(defun komorebi-web--cli-option-create (text)
  "Return a plist for a CLI OPTION from TEXT."
  (unless (string-empty-p (string-trim text))
    (let* ((lines (string-split text "\n" t "[ \t\n]*"))
           (desc-lines (seq-map
                        #'string-trim
                        (seq-filter
                         (lambda (line)
                           (if (s-contains? "help" (car lines)) t
                             (not (s-contains? "help" line t))))
                         (cdr lines)))))
      (when (string-match komorebi-web-cli-option-regex (car lines))
        (list :name (string-remove-prefix "--" (match-string 2 (car lines)))
              :short (if (match-string 1 (car lines)) (string-trim (match-string 1 (car lines))) "")
              :long (if (match-string 2 (car lines)) (string-trim (match-string 2 (car lines))) "")
              :description (komorebi-web--arg-description
                            (string-join (seq-map (lambda (line)
                                                    (string-trim
                                                     (replace-regexp-in-string
                                                      "[ \\[]*\\(possible values:\\)"
                                                      "\\1"
                                                      (if (and (not (s-contains? "[" line))
                                                               (string-suffix-p "]" (string-trim line)))
                                                          (string-remove-suffix "]" (string-trim-right line))
                                                        line))
                                                     "[\[\t\n]*" "[ \t\n\]]*"))
                                                  desc-lines)
                                         "\n")))))))

(defun komorebi-web--cli-options-plist ()
  "Return plist with command options from the komorebi website."
  (when (komorebi-web--cli-options-p)
    (narrow-to-region (match-beginning 0) (point-max))
    (goto-char (point-min))
    (delete-line)
    (komorebi-web--ensure-not-empty-line)
    (forward-char 1)
    (let ((options nil)
          (cur-end-of-line (pos-eol)))
      (while (re-search-forward komorebi-web-cli-option-regex (point-max) t)
        (goto-char (match-beginning 0))
        (when (< (point) cur-end-of-line)
          (goto-char (point-max)))
        (push (komorebi-web--cli-option-create
               (buffer-substring-no-properties (point-min) (point)))
              options)
        (delete-region (point-min) (point))
        (komorebi-web--ensure-not-empty-line)
        (setq cur-end-of-line (pos-eol))
        (unless (eobp)
          (forward-char 1)))
      (widen)
      (reverse (seq-filter 'identity options)))))


(defvar komorebi-web-cli-argument-regex "\\(?:[ ]*<\\(?1:.*\\)>\\)[ ]*\\(?2:.*\\)"
  "Regex to match a CLI ARGUMENT from text on the komorebi website.")

(defun komorebi-web--cli-arguments-p ()
  "Return non-nil if the current buffer contain a list of options."
  (goto-char (point-min))
  (search-forward "Arguments:" nil t))

(defun komorebi-web--cli-argument-create (text)
  "Return a plist for a CLI ARGUMENT from TEXT."
  (unless (string-empty-p (string-trim text))
    (with-temp-buffer
      (insert (string-trim
               (string-replace
                "\\" "" (string-replace
                         "\"" "'" (string-replace
                                   "\\\"" "'" (string-replace
                                               "\n" "" text))))))
      (goto-char (point-min))
      (re-search-forward komorebi-web-cli-argument-regex nil t)
      (when (match-string 1)
        (list :name (string-replace "_" "-" (downcase (string-trim (match-string 1))))
              :description (komorebi-web--arg-description (match-string 2)))))))

(defun komorebi-web--cli-arguments-plist ()
  "Return plist with COMMAND ARGUMENTS from the komorebi website."
  (let ((option-point (save-excursion
                        (komorebi-web--cli-options-p)
                        (goto-char (match-beginning 0))
                        (forward-line -1)
                        (point)))
        (arguments nil))
    (when (komorebi-web--cli-arguments-p)
      (narrow-to-region (match-beginning 0) option-point)
      (goto-char (point-min))
      (delete-line)
      (komorebi-web--ensure-not-empty-line)
      (goto-char (point-max))
      (while (re-search-backward komorebi-web-cli-argument-regex nil t)
        (goto-char (match-beginning 0))
        (push (komorebi-web--cli-argument-create
               (buffer-substring-no-properties (point) (point-max)))
              arguments)
        (delete-region (point) (point-max))
        (goto-char (point-max))))
    (when (buffer-narrowed-p)
      (widen))
    arguments))


(defun komorebi-web--cli-command-usage ()
  "Return the usage string of the command in the current buffer."
  (goto-char (point-min))
  (if (search-forward "Usage:" nil t)
      (let ((usage (buffer-substring-no-properties (point) (pos-eol))))
        (delete-line)
        (string-trim usage "[ :]*"))
    ""))


(defun komorebi-web--cli-command-description ()
  "Return the description of the command in the current buffer."
  (goto-char (point-min))
  (let* ((descriptions (string-split
                        (buffer-substring-no-properties
                         (point-min) (point-max))
                        "\n" t "[ \t\n\r]*"))
         (desc (string-join descriptions ".\n")))
    (setq desc (string-replace "\\\"" "'" desc))
    (unless (string-suffix-p "." desc)
      (setq desc (concat desc ".")))
    (if (length> desc 80)
        (string-join (string-split desc "." t "[ ]*") ".\n")
      desc)))


(defun komorebi-web--cli-command-create (command)
  "Return plist for COMMAND from the komorebi website."
  (when (plist-get command :text)
    (with-temp-buffer
      (insert (plist-get command :text))
      (goto-char (point-min))
      (make-instance 'komorebi-web-command
                     :name (plist-get command :title)
                     :location (plist-get command :location)
                     :text (plist-get command :text)
                     :arguments (komorebi-web--cli-arguments-plist)
                     :options (komorebi-web--cli-options-plist)
                     :usage (komorebi-web--cli-command-usage)
                     :description (komorebi-web--cli-command-description)))))


(defun komorebi-web--cli-commands-plist ()
  "Return a list of the CLI commands from the komorebi website."
  (seq-filter (lambda (doc)
                (string-prefix-p "cli" (plist-get doc :location)))
              (komorebi-web-docs)))


(defun komorebi-web-cli-commands ()
  "Return a list of the CLI commands from the komorebi website."
  (seq-filter 'identity
              (seq-map (lambda (command)
                         (komorebi-web--cli-command-create command))
                       (komorebi-web--cli-commands-plist))))


(defun komorebi-web-cli-command (command)
  "Return plist for COMMAND from the komorebi website."
  (seq-find (lambda (cmd) (or (string= command (oref cmd name))
                              (string= command (function-name cmd))))
            (komorebi-web-cli-commands)))


(defun komorebi-web--select-command ()
  "Return the selected COMMAND instance from the komorebi website."
  (let ((command (komorebi-web--select-doc
                  (komorebi-web--cli-commands-plist))))
    (komorebi-web--cli-command-create command)))


;;;###autoload
(defun komorebi-web-browse-cli (command)
  "Browse the komorebi documentation for COMMAND."
  (interactive (list (komorebi-web--select-command)))
  (browse-url (command-url command)))


;;;###autoload
(defun komorebi-web-browse-cli-at-point ()
  "Browse the komorebi documentation for the command at point."
  (interactive)
  (let ((command (thing-at-point 'symbol t)))
    (when (null command)
      (user-error "No symbol at point"))
    (komorebi-web-browse-cli (komorebi-web-cli-command command))))

(provide 'komorebi-web)
;;; komorebi-web.el ends here
