;;; komorebi.el --- Description -*- lexical-binding: t; -*-
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

(require 'komorebi-api)

(defcustom komorebi-keymap-prefix "C-c k"
  "Prefix for Komorebi keymap."
  :type 'string
  :group 'komorebi)

(defun komorebi--kdb-get (&rest keys)
  "Return Emacs key representation of KEYS."
  (kbd (format "%s %s" komorebi-keymap-prefix
               (string-join (ensure-list keys) " "))))

(defun komorebi-key-map-create ()
  "Define keymap for Komorebi."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "C-M-m") #'komorebi-minimize)
    (define-key key-map (kbd "M-n") #'komorebi-cycle-focused-next)
    (define-key key-map (kbd "M-p") #'komorebi-cycle-focused-previous)
    (define-key key-map (kbd "C-M-u") #'komorebi-unstack)
    (define-key key-map (kbd "C-M-n") #'komorebi-cycle-stacked-next)
    (define-key key-map (kbd "C-M-p") #'komorebi-cycle-stacked-previous)
    (define-key key-map (kbd "C-M-m") #'komorebi-toggle-monocle)
    key-map))

;; (add-to-list 'minor-mode-alist '(pydyn-python-mode " pydyn-python"))
;; (add-to-list 'minor-mode-map-alist (cons 'pydyn-python-mode pydyn-python-mode-map));;

(defvar komorebi-cycle-directions
  '("next" "previous")
  "Cycle directions for Komorebi.")

(defun komorebi--select-direction ()
  "Return selected direction."
  (completing-read "Direction: " komorebi-cycle-directions))


;;;###autoload
(defun komorebi-minimize ()
  "Minimize EMACS window."
  (interactive)
  (komorebi-api-minimize))

;;;###autoload
(defun komorebi-cycle-focused (direction)
  "Cycle through the focused window in DIRECTION."
  (interactive
   (list (komorebi--select-direction)))
  (komorebi-api-cycle-focus direction))

;;;###autoload
(defun komorebi-cycle-focused-next ()
  "Cycle through the focused window in order."
  (interactive)
  (komorebi-cycle-focused "next"))

;;;###autoload
(defun komorebi-cycle-focused-previous ()
  "Cycle through the focused window in reverse order."
  (interactive)
  (komorebi-cycle-focused "previous"))

;;;###autoload
(defun komorebi-unstack ()
  "Split the stack of windows in which EMACS is currently focused."
  (interactive)
  (komorebi-api-unstack))

;;;###autoload
(defun komorebi-cycle-stacked-next ()
  "Cycle through the stacked windows in order."
  (interactive)
  (komorebi-api-cycle-stack "next"))

;;;###autoload
(defun komorebi-cycle-stacked-previous ()
  "Cycle through the stacked windows in reverse order."
  (interactive)
  (komorebi-api-cycle-stack "previous"))

;;;###autoload
(defun komorebi-cycle-workspace-next ()
  "Cycle from the EMACS workspace to next workspace."
  (interactive)
  (komorebi-api-cycle-workspace "next"))

;;;###autoload
(defun komorebi-cycle-workspace-previous ()
  "Cycle from the EMACS workspace to previous workspace."
  (interactive)
  (komorebi-api-cycle-workspace "previous"))

(defun komorebi--workspace-select ()
  "Return selected workspace."
  (completing-read "Workspace: " (komorebi-api-workspaces)))

(defun komorebi-switch-to-workspace (workspace)
  "Switch from the current workspace to WORKSPACE."
  (interactive (list (komorebi--workspace-select)))
  (komorebi-api-move-to-workspace workspace))

(defun komorebi-send-to-workspace (workspace)
  "Move EMACS to WORKSPACE."
  (interactive (list (komorebi--workspace-select)))
  (komorebi-api-send-to-workspace workspace))

;;;###autoload
(defun komorebi-cycle-monitor-next ()
  "Cycle from monitor of EMACS to next monitor."
  (interactive)
  (komorebi-api-cycle-monitor "next"))

;;;###autoload
(defun komorebi-cycle-monitor-previous ()
  "Cycle from monitor of EMACS to previous monitor."
  (interactive)
  (komorebi-api-cycle-monitor "previous"))

;;;###autoload
(defun komorebi-toggle-monocle ()
  "Toggle monocle mode (full screen) of EMACS."
  (interactive)
  (komorebi-api-toggle-monocle))

(defcustom komorebi-config-files '("komorebi.json")
  "List of static configuration files for Komorebi."
  :type 'list
  :group 'komorebi)

(defun komorebi--select-config ()
  "Return selected direction."
  (unless komorebi-config-files
    (error "`komorebi-config-files' is not set"))
  (if (length= komorebi-config-files 1)
      (car komorebi-config-files)
    (completing-read "Config-File: " komorebi-config-files)))

;;;###autoload
(defun komorebi-replace-config (config)
  "Replace Komorebi configuration with CONFIG."
  (interactive (list (komorebi--select-config)))
  (komorebi-api-replace-configuration config))

;;;###autoload
(cl-defun komorebi-restart (&optional &key whkd ahk bar)
  "Restart Komorebi with option WHKD, AHK and/or BAR.
Only either WHKD or AHK can be set at a time."
  (interactive (list :whkd nil :ahk t :bar t))
  (let ((monitor (komorebi-api-current-monitor))
        (workspace (komorebi-api-current-workspace)))
    (komorebi-api-stop)
    (komorebi-api-start :whkd whkd :ahk ahk :bar bar)))


;;;###autoload
(define-minor-mode komorebi-mode
  "Toggle Komorebi mode."
  :init-value nil
  :global t
  :group 'komorebi
  :lighter " Komorebi"
  :keymap (komorebi-key-map-create))


(defun komorebi-turn-on-unless ()
  "Toggle Komorebi mode."
  (if komorebi-mode
      (komorebi-mode -1)
    (komorebi-mode 1)))


;;;###autoload
(define-global-minor-mode global-komorebi-mode
  komorebi-mode komorebi-turn-on-unless
  :group 'komorebi)


(provide 'komorebi)
;;; komorebi.el ends here

;; Local Variables:
;; jinx-local-words: "Unmanage Unmanaged behaviour colour"
;; End:
