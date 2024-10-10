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
;; Package-Requires: ((emacs "29.1"))
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
    (define-key key-map (komorebi--kdb-get "i") #'komorebi-manage)
    (define-key key-map (komorebi--kdb-get "i") #'komorebi-unmanage)
    (define-key key-map (komorebi--kdb-get "u") #'komorebi-unstack)
    (define-key key-map (komorebi--kdb-get "i") #'komorebi-minimize)
    (define-key key-map (komorebi--kdb-get "f") #'komorebi-switch-focus)
    (define-key key-map (komorebi--kdb-get "s") #'komorebi-switch-stack)
    (define-key key-map (komorebi--kdb-get "w") #'komorebi-switch-workspace)
    (define-key key-map (komorebi--kdb-get "m") #'komorebi-switch-monitor)
    (define-key key-map (komorebi--kdb-get "o") #'komorebi-toggle-monocle)
    (define-key key-map (kbd "M-n") #'komorebi-focus-next)
    (define-key key-map (kbd "M-'") #'komorebi-horizontally-increase)
    (define-key key-map (kbd "M-?") #'komorebi-horizontally-decrease)
    (define-key key-map (kbd "M-^") #'komorebi-vertically-increase)
    (define-key key-map (kbd "M-`") #'komorebi-vertically-decrease)
    (define-key key-map (kbd "C-M-u") #'komorebi-unstack)
    (define-key key-map (kbd "C-M-n") #'komorebi-stack-next)
    (define-key key-map (kbd "C-M-r") #'komorebi-retile)
    (define-key key-map (kbd "C-M-f") #'komorebi-toggle-float)
    (define-key key-map (kbd "C-M-p") #'komorebi-toggle-pause)
    (define-key key-map (kbd "M-0") #'komorebi-workspace-next)
    key-map))

;; (add-to-list 'minor-mode-alist '(pydyn-python-mode " pydyn-python"))
;; (add-to-list 'minor-mode-map-alist
;; (cons 'pydyn-python-mode pydyn-python-mode-map));;

(defvar komorebi-cycle-directions '("Next" "Previous")
  "Cycle directions for Komorebi.")

(defun komorebi--is-cycle-direction (direction)
  "Return non-nil if DIRECTION is an operation direction."
  (seq-some (lambda (x) (string= (downcase x) direction))
            komorebi-cycle-directions))

(defvar komorebi-operation-directions '("Up" "Down" "Left" "Right")
  "Operation directions for Komorebi.")

(defun komorebi--is-operation-direction (direction)
  "Return non-nil if DIRECTION is an operation direction."
  (seq-some (lambda (x) (string= (downcase x) direction))
            komorebi-operation-directions))


(defun komorebi--seleced-direction ()
  "Return selected direction. Either operation or cycle direction.
PREFIX is used to specify the type of direction."
  (downcase (completing-read
             "Direction: "
             (append komorebi-operation-directions
                     komorebi-cycle-directions))))

;;;###autoload
(cl-defun komorebi-resize (direction)
  "Resize window in DIRECTION.
Horizontal (edge) is either left or right.
Vertical   direction (edge) is either up or down."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-api-resize
     direction  (if (equal direction "up") "increase" "decrease"))))

(defun komorebi--select-direction (prefix)
  "Return selected direction. Either operation or cycle direction.
PREFIX is used to specify the type of direction."
  (if (or (not (numberp prefix))
          (< prefix 4) (> prefix 9))
      (komorebi--seleced-direction)
    (cond ((= prefix 4) "next")
          ((= prefix 5) "previous")
          ((= prefix 6) "left")
          ((= prefix 7) "right")
          ((= prefix 8) "up")
          ((= prefix 9) "down")
          (t (komorebi--seleced-direction)))))


(defun komorebi-change-in (direction cycle-func operation-func)
  "Cycle or move in DIRECTION with CYCLE-FUNC or OPERATION-FUNC."
  (if (komorebi--is-cycle-direction direction)
      (funcall cycle-func :cycle-direction direction)
    (funcall operation-func :operation-direction direction)))


;;;###autoload
(defun komorebi-switch-focus (direction)
  "Cycle through the focus to window in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-focus
                        #'komorebi-api-focus)))

;;;###autoload
(defun komorebi-focus-next ()
  "Switch next focused window."
  (interactive)
  (komorebi-api-cycle-focus :cycle-direction "next"))


;;;###autoload
(defun komorebi-switch-stack (direction)
  "Stack the window with an other(s) in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-stack
                        #'komorebi-api-stack)))


;;;###autoload
(defun komorebi-switch-workspace (direction)
  "Cycle or move to the next workspace in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-workspace
                        #'komorebi-api-move-to-workspace)))

;;;###autoload
(defun komorebi-workspace-next ()
  "Switch to the next workspace."
  (interactive)
  (komorebi-api-cycle-workspace :cycle-direction "next"))


;;;###autoload
(defun komorebi-switch-monitor (direction)
  "Cycle or move to monitor in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-monitor
                        #'komorebi-api-move-to-monitor)))

;;;###autoload
(defun komorebi-horizontally-decrease ()
  "Decrease Emacs window horizontally."
  (interactive)
  (komorebi-api-resize-axis :axis "horizontal" :sizing "decrease"))

;;;###autoload
(defun komorebi-horizontally-increase ()
  "Increase Emacs window horizontally."
  (interactive)
  (komorebi-api-resize-axis :axis "horizontal" :sizing "increase"))

;;;###autoload
(defun komorebi-vertically-decrease ()
  "Decrease Emacs window vertically."
  (interactive)
  (komorebi-api-resize-axis :axis "vertical" :sizing "decrease"))

;;;###autoload
(defun komorebi-vertically-increase ()
  "Increase Emacs window vertically."
  (interactive)
  (komorebi-api-resize-axis :axis "vertical" :sizing "increase"))


(defun komorebi-is-started ()
  "Return non-nil if Komorebi-server is started.
Determines if Komorebi is running is by checking if static configuration is set."
  (interactive)
  (komorebi-configuration))


(defcustom komorebi-configuration-files nil
  "List of Komorebi configuration files."
  :type 'list
  :group 'komorebi)


(defun komorebi--select-config ()
  "Return selected direction."
  (unless (komorebi-is-started)
    (error "Komorebi is not running"))
  (unless komorebi-configuration-files
    (push (komorebi-current-config) komorebi-configuration-files))
  (if (length= komorebi-configuration-files 1)
      (car komorebi-configuration-files)
    (completing-read "Select Config: " komorebi-configuration-files)))


;;;###autoload
(defun komorebi-replace-config (config)
  "Replace Komorebi static configuration with CONFIG."
  (interactive (list (komorebi--select-config)))
  (unless (file-exists-p config)
    (error "Configuration file %s does not exist" config))
  (if (equal (komorebi-current-config) config)
      (message "Current loaded path is equal... Configuration nor reload.")
    (komorebi-api-replace-configuration config)))


;;;###autoload
(cl-defun komorebi-restart (&optional &key whkd ahk bar)
  "Restart Komorebi with option WHKD, AHK and/or BAR.
Only either WHKD or AHK can be set at a time."
  (interactive (list :whkd nil :ahk t :bar t))
  (komorebi-stop)
  (komorebi-start))


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
