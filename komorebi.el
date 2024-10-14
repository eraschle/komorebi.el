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

(defcustom komorebi-keymap-prefix "C-c C-i"
  "Prefix for Komorebi keymap."
  :type 'string
  :group 'komorebi)

(defun komorebi--kdb (&rest keys)
  "Return Emacs key representation of KEYS."
  (kbd (format "%s %s" komorebi-keymap-prefix
               (string-join (ensure-list keys) " "))))


(defvar komorebi-mode-map (make-sparse-keymap)
  "Keymap for Komorebi.")


(defun komorebi-key-map-create ()
  "Define keymap for Komorebi."
  (define-key komorebi-mode-map (komorebi--kdb "M") #'komorebi-manage)
  (define-key komorebi-mode-map (komorebi--kdb "U") #'komorebi-unmanage)
  (define-key komorebi-mode-map (komorebi--kdb "i") #'komorebi-minimize)
  (define-key komorebi-mode-map (komorebi--kdb "b") #'komorebi-web-browse-cli-at-point)
  (define-key komorebi-mode-map (komorebi--kdb "h") #'komorebi-api-command-help-at-point)
  (define-key komorebi-mode-map (komorebi--kdb "H") #'komorebi-api-command-help)
  (define-key komorebi-mode-map (komorebi--kdb "F") #'komorebi-switch-focus)
  (define-key komorebi-mode-map (komorebi--kdb "M") #'komorebi-switch-move)
  (define-key komorebi-mode-map (komorebi--kdb "S") #'komorebi-switch-stack)
  ;; Flip layout
  (define-key komorebi-mode-map (komorebi--kdb "l h") #'komorebi-flip-layout-horizontal)
  (define-key komorebi-mode-map (komorebi--kdb "l v") #'komorebi-flip-layout-vertical)
  ;; Resize
  (define-key komorebi-mode-map (komorebi--kdb "r h") #'komorebi-horizontally-increase)
  (define-key komorebi-mode-map (komorebi--kdb "r H") #'komorebi-horizontally-decrease)
  (define-key komorebi-mode-map (komorebi--kdb "r v") #'komorebi-vertically-increase)
  (define-key komorebi-mode-map (komorebi--kdb "r V") #'komorebi-vertically-decrease)
  ;; Toggle
  (define-key komorebi-mode-map (komorebi--kdb "t f") #'komorebi-toggle-float)
  (define-key komorebi-mode-map (komorebi--kdb "t m") #'komorebi-toggle-monocle)
  (define-key komorebi-mode-map (komorebi--kdb "t p") #'komorebi-toggle-pause)
  (define-key komorebi-mode-map (komorebi--kdb "t t") #'komorebi-toggle-transparency)
  (define-key komorebi-mode-map (komorebi--kdb "t x") #'komorebi-toggle-maximize)
  (define-key komorebi-mode-map (komorebi--kdb "t f") #'komorebi-toggle-window-container-behaviour)
  ;; Focus
  (define-key komorebi-mode-map (komorebi--kdb "f n") #'komorebi-focus-next)
  (define-key komorebi-mode-map (komorebi--kdb "f p") #'komorebi-focus-previous)
  (define-key komorebi-mode-map (komorebi--kdb "f h") #'komorebi-focus-left)
  (define-key komorebi-mode-map (komorebi--kdb "f l") #'komorebi-focus-right)
  (define-key komorebi-mode-map (komorebi--kdb "f j") #'komorebi-focus-down)
  (define-key komorebi-mode-map (komorebi--kdb "f k") #'komorebi-focus-up)
  ;; Move
  (define-key komorebi-mode-map (komorebi--kdb "v n") #'komorebi-move-next)
  (define-key komorebi-mode-map (komorebi--kdb "v p") #'komorebi-move-previous)
  (define-key komorebi-mode-map (komorebi--kdb "v h") #'komorebi-move-left)
  (define-key komorebi-mode-map (komorebi--kdb "v l") #'komorebi-move-right)
  (define-key komorebi-mode-map (komorebi--kdb "v j") #'komorebi-move-down)
  (define-key komorebi-mode-map (komorebi--kdb "v k") #'komorebi-move-up)
  ;; Stack
  (define-key komorebi-mode-map (komorebi--kdb "s n") #'komorebi-stack-next)
  (define-key komorebi-mode-map (komorebi--kdb "s p") #'komorebi-stack-previous)
  (define-key komorebi-mode-map (komorebi--kdb "s h") #'komorebi-stack-left)
  (define-key komorebi-mode-map (komorebi--kdb "s l") #'komorebi-stack-right)
  (define-key komorebi-mode-map (komorebi--kdb "s j") #'komorebi-stack-down)
  (define-key komorebi-mode-map (komorebi--kdb "s k") #'komorebi-stack-up)
  (define-key komorebi-mode-map (komorebi--kdb "s u") #'komorebi-unstack)
  (define-key komorebi-mode-map (komorebi--kdb "s a") #'komorebi-stack-all)
  ;; Workspace and Monitor
  (define-key komorebi-mode-map (komorebi--kdb "o w") #'komorebi-workspace-next)
  (define-key komorebi-mode-map (komorebi--kdb "o W") #'komorebi-workspace-previous)
  (define-key komorebi-mode-map (komorebi--kdb "o m") #'komorebi-monitor-next)
  (define-key komorebi-mode-map (komorebi--kdb "o M") #'komorebi-monitor-previous))


(add-to-list 'minor-mode-alist '(komorebi-mode " komorebi"))
(komorebi-key-map-create)
(add-to-list 'minor-mode-map-alist
             (cons 'komorebi-mode komorebi-mode-map));;


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
                     komorebi-cycle-directions)
             nil 'require-match)))

(defun komorebi--select-direction (prefix)
  "Return selected direction. Either operation or cycle direction.
PREFIX is used to specify the type of direction."
  (cond ((or (komorebi--is-cycle-direction prefix)
             (komorebi--is-operation-direction prefix))
         prefix)
        ((or (not (numberp prefix))
             (< prefix 4) (> prefix 9))
         (komorebi--seleced-direction))
        ((= prefix 4) "next")
        ((= prefix 5) "previous")
        ((= prefix 6) "left")
        ((= prefix 7) "right")
        ((= prefix 8) "up")
        ((= prefix 9) "down")
        (t (komorebi--seleced-direction))))


(defun komorebi-change-in (direction cycle-func operation-func)
  "Cycle or move in DIRECTION with CYCLE-FUNC or OPERATION-FUNC."
  (if (komorebi--is-cycle-direction direction)
      (funcall cycle-func :cycle-direction direction)
    (funcall operation-func :operation-direction direction)))


;;
;;; Layout
;;;###autoload
(defun komorebi-flip-layout-horizontal ()
  "Flip layout horizontally."
  (interactive)
  (komorebi-api-flip-layout :axis "horizontal"))

;;;###autoload
(defun komorebi-flip-layout-vertical ()
  "Flip layout vertically."
  (interactive)
  (komorebi-api-flip-layout :axis "vertical"))

;;
;;; Focus

;;;###autoload
(cl-defun komorebi-switch-focus (&key direction)
  "Cycle through the focus to window in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-focus
                        #'komorebi-api-focus)))

;;;###autoload
(defun komorebi-focus-next ()
  "Focus next window."
  (interactive)
  (komorebi-switch-focus :direction "next"))

;;;###autoload
(defun komorebi-focus-previous ()
  "Focus previous window."
  (interactive)
  (komorebi-switch-focus :direction "previous"))

;;;###autoload
(defun komorebi-focus-left ()
  "Focus window to the left."
  (interactive)
  (komorebi-switch-focus :direction "left"))

;;;###autoload
(defun komorebi-focus-right ()
  "Focus window to the right."
  (interactive)
  (komorebi-switch-focus :direction "right"))

;;;###autoload
(defun komorebi-focus-up ()
  "Focus window up."
  (interactive)
  (komorebi-switch-focus :direction "up"))

;;;###autoload
(defun komorebi-focus-down ()
  "Focus window down."
  (interactive)
  (komorebi-switch-focus :direction "down"))


;;
;;; Move

;;;###autoload
(defun komorebi-switch-move (&key direction)
  "Move window in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-move
                        #'komorebi-api-move)))

;;;###autoload
(defun komorebi-move-next ()
  "Move next window."
  (interactive)
  (komorebi-switch-move :direction "next"))

;;;###autoload
(defun komorebi-move-previous ()
  "Move previous window."
  (interactive)
  (komorebi-switch-move :direction "previous"))

;;;###autoload
(defun komorebi-move-left ()
  "Move window to the left."
  (interactive)
  (komorebi-switch-move :direction "left"))

;;;###autoload
(defun komorebi-move-right ()
  "Move window to the right."
  (interactive)
  (komorebi-switch-move :direction "right"))

;;;###autoload
(defun komorebi-move-up ()
  "Move window up."
  (interactive)
  (komorebi-switch-move :direction "up"))

;;;###autoload
(defun komorebi-move-down ()
  "Move window down."
  (interactive)
  (komorebi-switch-move :direction "down"))


;;
;;; Stack

;;;###autoload
(cl-defun komorebi-switch-stack (&key direction)
  "Stack the window with an other(s) in DIRECTION."
  (interactive "p")
  (let ((direction (komorebi--select-direction direction)))
    (komorebi-change-in direction
                        #'komorebi-api-cycle-stack
                        #'komorebi-api-stack)))

;;;###autoload
(defun komorebi-stack-next ()
  "Stack next window."
  (interactive)
  (komorebi-switch-stack :direction "next"))

;;;###autoload
(defun komorebi-stack-previous ()
  "Stack previous window."
  (interactive)
  (komorebi-switch-stack :direction "previous"))

;;;###autoload
(defun komorebi-stack-left ()
  "Stack window to the left."
  (interactive)
  (komorebi-switch-stack :direction "left"))

;;;###autoload
(defun komorebi-stack-right ()
  "Stack window to the right."
  (interactive)
  (komorebi-switch-stack :direction "right"))

;;;###autoload
(defun komorebi-stack-up ()
  "Stack window up."
  (interactive)
  (komorebi-switch-stack :direction "up"))

;;;###autoload
(defun komorebi-stack-down ()
  "Stack window down."
  (interactive)
  (komorebi-switch-stack :direction "down"))

;;
;;; Workspace and Monitor

;;;###autoload
(defun komorebi-workspace-next ()
  "Cycle to next workspace."
  (interactive)
  (komorebi-api-cycle-workspace :cycle-direction "next"))

;;;###autoload
(defun komorebi-workspace-previous ()
  "Cycle to previous workspace."
  (interactive)
  (komorebi-api-cycle-workspace :cycle-direction "previous"))

;;;###autoload
(defun komorebi-monitor-next ()
  "Cycle to next monitor."
  (interactive)
  (komorebi-api-cycle-monitor :cycle-direction "next"))

;;;###autoload
(defun komorebi-monitor-previous ()
  "Cycle to previous monitor."
  (interactive)
  (komorebi-api-cycle-monitor :cycle-direction "previous"))

;;
;;; Resize

;;;###autoload
(defun komorebi-horizontally-decrease ()
  "Decrease Emacs window horizontally."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis :axis "horizontal" :sizing "decrease"))))

;;;###autoload
(defun komorebi-horizontally-increase ()
  "Increase Emacs window horizontally."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis :axis "horizontal" :sizing "increase"))))

;;;###autoload
(defun komorebi-vertically-decrease ()
  "Decrease Emacs window vertically."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis :axis "vertical" :sizing "decrease"))))

;;;###autoload
(defun komorebi-vertically-increase ()
  "Increase Emacs window vertically."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis :axis "vertical" :sizing "increase"))))
;;
;;; Configuration

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
    (push (komorebi-configuration) komorebi-configuration-files))
  (if (length= komorebi-configuration-files 1)
      (car komorebi-configuration-files)
    (let* ((configs (seq-map (lambda (cfg) (string-remove-prefix (file-name-directory cfg) cfg))
                             komorebi-configuration-files))
           (answer (completing-read "Select Config: " configs)))
      (seq-find (lambda (cfg) (string-suffix-p answer cfg))
                komorebi-configuration-files))))


(defun komorebi--as-win-path (path)
  "Return PATH as Windows path."
  (if (string-prefix-p "/mnt/c/" path)
      (concat "C:/" (string-remove-prefix "/mnt/c/" path))
    path))


;;;###autoload
(defun komorebi-replace-config (config)
  "Replace Komorebi static configuration with CONFIG."
  (interactive (list (komorebi--select-config)))
  (unless (file-exists-p config)
    (error "Configuration file %s does not exist" config))
  (if (equal (komorebi-configuration) config)
      (message "Current loaded path is equal... Configuration nor reload.")
    (komorebi-api-replace-configuration :path (komorebi--as-win-path config))))


;;;###autoload
(cl-defun komorebi-restart (&optional &key whkd ahk bar)
  "Restart Komorebi with option WHKD, AHK and/or BAR.
Only either WHKD or AHK can be set at a time."
  (interactive (list :whkd nil :ahk t :bar t))
  (komorebi-stop)
  (komorebi-start :whkd whkd :ahk ahk :bar bar))

;;
;;; Minor Mode

;;;###autoload
(define-minor-mode komorebi-mode
  "Toggle Komorebi mode."
  :init-value nil
  :global t
  :group 'komorebi
  :lighter " Komorebi"
  :keymap komorebi-mode-map)


(defun komorebi-turn-on ()
  "Toggle Komorebi mode."
  (komorebi-mode 1))


(defun komorebi-turn-off ()
  "Toggle Komorebi mode."
  (komorebi-mode -1))


;;;###autoload
(define-global-minor-mode global-komorebi-mode
  komorebi-mode komorebi-turn-on
  :group 'komorebi)


(provide 'komorebi)
;;; komorebi.el ends here

;; Local Variables:
;; jinx-local-words: "Unmanage Unmanaged behaviour colour"
;; End:
