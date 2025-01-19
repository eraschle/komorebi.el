;;; komorebi-path.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Erich Raschle
;;
;; Author: Erich Raschle <erichraschle@gmail.com>
;; Maintainer: Erich Raschle <erichraschle@gmail.com>
;; Created: Januar 19, 2025
;; Modified: Januar 19, 2025
;; Version: 0.0.1
;; Keywords: extensions help languages local processes
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


(defun komorebi-path--replace-drive (path drive)
  "Replace the drive part in PATH with DRIVE."
  (replace-match drive t nil path 1))


(defcustom komorebi-path-windows-regex "^\\(\\([a-zA-Z]+\\):\\).*"
  "Regex to match start of Windows path."
  :type 'string
  :group 'komorebi)


(defun komorebi-path-is-win (path)
  "Return non-nil if PATH is a Windows path."
  (string-match komorebi-path-windows-regex path))


(defun komorebi-path-to-win (path)
  "Convert Windows PATH to Unix path."
  (if (komorebi-path-is-wsl path)
      (komorebi-path--replace-drive
       path (concat (upcase (match-string 2 path)) ":"))
    path))


(defcustom komorebi-path-wsl-regex "^\\(/mnt/\\([a-zA-Z]+\\)\\)/.*"
  "Regex to match start of WSL path."
  :type 'string
  :group 'komorebi)


(defun komorebi-path-is-wsl (path)
  "Return non-nil if PATH is a WSL path."
  (string-match komorebi-path-wsl-regex path))


(defun komorebi-path-to-wsl (path)
  "Convert Windows PATH to Unix path."
  (if (komorebi-path-is-win path)
      (komorebi-path--replace-drive
       path (concat "/mnt/" (downcase (match-string 2 path))))
    path))


(defun komorebi-path-exists (path)
  "Return non-nil if PATH exists."
  (if (featurep :system 'linux)
      (file-exists-p (komorebi-path-to-wsl path))
    (file-exists-p (komorebi-path-to-win path))))


(defun komorebi-path--ensure-slash (current-path)
  "Return the CURRENT-PATH with slashes instead of backslashes."
  (let ((path (string-replace "\\" "/" current-path)))
    (unless (string-suffix-p "/" path)
      (setq path (concat path "/")))
    path))


(defun komorebi-path-config-home ()
  "Return the path to the komorebi configuration folder or nil."
  (let ((config-path (getenv "KOMOREBI_CONFIG_HOME")))
    (if (and config-path (file-directory-p config-path))
        (komorebi-path--ensure-slash config-path)
      (setq config-path
            (string-join (list (getenv "USERPROFILE") ".config" "komorebi")
                         "/"))
      (if (file-directory-p config-path)
          (komorebi-path--ensure-slash config-path)
        nil))))

(provide 'komorebi-path)
;;; komorebi-path.el ends here

;; Local Variables:
;; jinx-local-words: "mnt"
;; End:
