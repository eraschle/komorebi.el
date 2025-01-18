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
(require 'komorebi-help)


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
  (define-key komorebi-mode-map (komorebi--kdb "M") #'komorebi-api-manage)
  (define-key komorebi-mode-map (komorebi--kdb "U") #'komorebi-api-unmanage)
  (define-key komorebi-mode-map (komorebi--kdb "i") #'komorebi-api-minimize)
  (define-key komorebi-mode-map (komorebi--kdb "b") #'komorebi-web-browse-cli-at-point)
  (define-key komorebi-mode-map (komorebi--kdb "h") #'komorebi-help-command-at-point)
  (define-key komorebi-mode-map (komorebi--kdb "H") #'komorebi-help-command)
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
  (define-key komorebi-mode-map (komorebi--kdb "t f") #'komorebi-api-toggle-float)
  (define-key komorebi-mode-map (komorebi--kdb "t m") #'komorebi-api-toggle-monocle)
  (define-key komorebi-mode-map (komorebi--kdb "t p") #'komorebi-api-toggle-pause)
  (define-key komorebi-mode-map (komorebi--kdb "t t") #'komorebi-api-toggle-transparency)
  (define-key komorebi-mode-map (komorebi--kdb "t x") #'komorebi-api-toggle-maximize)
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
  (define-key komorebi-mode-map (komorebi--kdb "s u") #'komorebi-api-unstack)
  (define-key komorebi-mode-map (komorebi--kdb "s a") #'komorebi-api-stack-all)
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


;;
;;; Layout


;;;###autoload
(defun komorebi-flip-layout-horizontal ()
  "Flip layout horizontally."
  (interactive)
  (komorebi-api-flip-layout "horizontal"))


;;;###autoload
(defun komorebi-flip-layout-vertical ()
  "Flip layout vertically."
  (interactive)
  (komorebi-api-flip-layout "vertical"))


;;
;;; Focus


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
  "Focus next window."
  (interactive)
  (komorebi-switch-focus "next"))


;;;###autoload
(defun komorebi-focus-previous ()
  "Focus previous window."
  (interactive)
  (komorebi-switch-focus "previous"))


;;;###autoload
(defun komorebi-focus-left ()
  "Focus window to the left."
  (interactive)
  (komorebi-switch-focus "left"))


;;;###autoload
(defun komorebi-focus-right ()
  "Focus window to the right."
  (interactive)
  (komorebi-switch-focus "right"))


;;;###autoload
(defun komorebi-focus-up ()
  "Focus window up."
  (interactive)
  (komorebi-switch-focus "up"))


;;;###autoload
(defun komorebi-focus-down ()
  "Focus window down."
  (interactive)
  (komorebi-switch-focus "down"))


;;
;;; Move


(defun komorebi-change-in (direction cycle-func operation-func)
  "Cycle or move in DIRECTION with CYCLE-FUNC or OPERATION-FUNC."
  (if (komorebi--is-cycle-direction direction)
      (funcall cycle-func direction)
    (funcall operation-func direction)))


;;;###autoload
(defun komorebi-switch-move (direction)
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
  (komorebi-switch-move "next"))


;;;###autoload
(defun komorebi-move-previous ()
  "Move previous window."
  (interactive)
  (komorebi-switch-move "previous"))


;;;###autoload
(defun komorebi-move-left ()
  "Move window to the left."
  (interactive)
  (komorebi-switch-move "left"))


;;;###autoload
(defun komorebi-move-right ()
  "Move window to the right."
  (interactive)
  (komorebi-switch-move "right"))


;;;###autoload
(defun komorebi-move-up ()
  "Move window up."
  (interactive)
  (komorebi-switch-move "up"))


;;;###autoload
(defun komorebi-move-down ()
  "Move window down."
  (interactive)
  (komorebi-switch-move "down"))


;;
;;; Stack


;;;###autoload
(defun komorebi-switch-stack (direction)
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
  (komorebi-switch-stack "next"))


;;;###autoload
(defun komorebi-stack-previous ()
  "Stack previous window."
  (interactive)
  (komorebi-switch-stack "previous"))


;;;###autoload
(defun komorebi-stack-left ()
  "Stack window to the left."
  (interactive)
  (komorebi-switch-stack "left"))


;;;###autoload
(defun komorebi-stack-right ()
  "Stack window to the right."
  (interactive)
  (komorebi-switch-stack "right"))


;;;###autoload
(defun komorebi-stack-up ()
  "Stack window up."
  (interactive)
  (komorebi-switch-stack "up"))


;;;###autoload
(defun komorebi-stack-down ()
  "Stack window down."
  (interactive)
  (komorebi-switch-stack "down"))


;;
;;; Workspace and Monitor


;;;###autoload
(defun komorebi-workspace-next ()
  "Cycle to next workspace."
  (interactive)
  (komorebi-api-cycle-workspace "next"))


;;;###autoload
(defun komorebi-workspace-previous ()
  "Cycle to previous workspace."
  (interactive)
  (komorebi-api-cycle-workspace "previous"))


;;;###autoload
(defun komorebi-monitor-next ()
  "Cycle to next monitor."
  (interactive)
  (komorebi-api-cycle-monitor "next"))


;;;###autoload
(defun komorebi-monitor-previous ()
  "Cycle to previous monitor."
  (interactive)
  (komorebi-api-cycle-monitor "previous"))


;;
;;; Resize


;;;###autoload
(defun komorebi-horizontally-decrease ()
  "Decrease Emacs window horizontally."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis "horizontal" "decrease"))))


;;;###autoload
(defun komorebi-horizontally-increase ()
  "Increase Emacs window horizontally."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis "horizontal" "increase"))))


;;;###autoload
(defun komorebi-vertically-decrease ()
  "Decrease Emacs window vertically."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis "vertical" "decrease"))))


;;;###autoload
(defun komorebi-vertically-increase ()
  "Increase Emacs window vertically."
  (interactive)
  (let ((prefix-no (if current-prefix-arg current-prefix-arg 1)))
    (cl-dotimes (_ prefix-no)
      (komorebi-api-resize-axis "vertical" "increase"))))


;;
;;; Configuration

(defun komorebi-is-wsl ()
  "Return non-nil if OS is WSL."
  (and (featurep :system 'linux)
       (string-match "Linux.*[Mm]icrosoft.*Linux"
                     (shell-command-to-string "uname -a"))))


(defvar komorebi--windows-path-regex "^\\(\\([a-zA-Z]+\\):\\).*"
  "Regex to match start of Windows path.")


(defun komorebi-is-win-path-p (path)
  "Return non-nil if PATH is a Windows path."
  (string-match komorebi--windows-path-regex path))


(defun komorebi-to-wsl-path (path)
  "Convert Windows PATH to Unix path."
  (if (komorebi-is-win-path-p path)
      (replace-match
       (concat "/mnt/" (downcase (match-string 2 path)))
       t nil path 1)
    path))


(defvar komorebi--wsl-path-regex "^\\(/mnt/\\([a-zA-Z]+\\)\\)/.*"
  "Regex to match start of WSL path.")


(defun komorebi-is-wsl-path-p (path)
  "Return non-nil if PATH is a WSL path."
  (string-match komorebi--wsl-path-regex path))


(defun komorebi-to-win-path (path)
  "Convert Windows PATH to Unix path."
  (if (komorebi-is-wsl-path-p path)
      (replace-match
       (concat (match-string 2 path) ":")
       nil nil path 1)
    path))


(defun komorebi-path-exists (path)
  "Return non-nil if PATH exists."
  (if (komorebi-is-wsl)
      (setq path (komorebi-to-wsl-path path))
    (setq path (komorebi-to-win-path path)))
  (file-exists-p path))


;;;###autoload
(defun komorebi-configuration-edit ()
  "Create buffer with configuration file."
  (interactive)
  (let ((config (komorebi-current-config)))
    (when (komorebi-is-wsl)
      (setq config (komorebi-to-wsl-path config)))
    (find-file config)))


(defcustom komorebi-configuration-files nil
  "List of Komorebi configuration files."
  :type '(repeat string)
  :group 'komorebi)


(defun komorebi--valid-config (file-path exclude-current)
  "Return non-nil if FILE-PATH is a valid configuration file.
If EXCLUDE-CURRENT is non-nil, exclude the config file
of the current process."
  (and (komorebi-path-exists file-path)
       (not (and exclude-current
                 (equal file-path (komorebi--current-config))))))


(defun komorebi--config-files (&optional exclude-current)
  "Return list of configuration files for `komorebi-read-config'.
If EXCLUDE-CURRENT is non-nil, the list does not include the config file
of the current process."
  (seq-map (lambda (cfg) (file-name-base cfg))
           (seq-filter (lambda (cfg)
                         (unless (komorebi-path-exists cfg)
                           (message "Komorebi: Not found %s" cfg))
                         (komorebi--valid-config cfg exclude-current))
                       komorebi-configuration-files)))


(defun komorebi-read-config (&optional exclude-current)
  "Return configuration file selected by the user.
If EXCLUDE-CURRENT is non-nil, exclude the current configuration file."
  (let ((answer (completing-read "Select configuration file: "
                                 (komorebi--config-files exclude-current))))
    (seq-find (lambda (cfg) (equal (file-name-base cfg) answer))
              komorebi-configuration-files)))


(defun komorebi--current-config ()
  "Return configuration file from the current komorebi process."
  (string-trim (string-replace "\\" "/" (komorebi-api-configuration))))


(defun komorebi-is-started ()
  "Return non-nil if Komorebi-server is started.
Determines if Komorebi is running is by checking if static configuration is set."
  (komorebi-api-configuration))


(defun komorebi-current-config ()
  "Return current configuration file."
  (cond ((komorebi-is-started)
         (komorebi--current-config))
        ((unless komorebi-configuration-files
           (error "No configuration files set")))
        ((length= komorebi-configuration-files 1)
         (seq-first komorebi-configuration-files))
        (t (komorebi-read-config))))


;;;###autoload
(defun komorebi-replace-config (config)
  "Replace Komorebi static configuration with CONFIG."
  (interactive (list (komorebi-read-config
                      (komorebi-is-started))))
  (unless (file-exists-p config)
    (error "Configuration file %s does not exist" config))
  (komorebi-api-replace-configuration config))


;;;###autoload
(defmacro komorebi-go-to! (name exe)
  "Move the focus with komorebi to the window with EXE.
NAME is as the suffix of the function name."
  `(defun ,(intern (format "komorebi-go-to-%s" name)) ()
     (interactive)
     (komorebi-api-eager-focus ,exe)))

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
  (interactive)
  (komorebi-mode 1))


(defun komorebi-turn-off ()
  "Toggle Komorebi mode."
  (interactive)
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
