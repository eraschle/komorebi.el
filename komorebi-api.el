;;; komorebi-api.el --- Description -*- lexical-binding: t; -*-
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
(require 'cl-lib)
(require 'view)
(require 'subr-x)
(require 'org)

(defcustom komorebi-api-executable "/mnt/c/workspace/komorebi/target/release/komorebic.exe"
  "The path to the komorebi executable."
  :type 'string
  :group 'komorebi-api)

(defcustom komorebi-api-version "v0.1.29"
  "The version of komorebi."
  :type 'string
  :group 'komorebi-api)


(defvar komorebi-api-help-buffer-name "*komorebi-api-help*"
  "The name of the buffer to display command help.")


(defun komorebi-api--clean-control-char (value)
  "Return VALUE cleaned of control characters and double quotes."
  (string-trim value "[ \t\n\r\"]*" "[ \t\n\r\"]*"))


(defun komorebi-api--args-get (args)
  "Return string of ARGS."
  (string-join
   (seq-filter (lambda (arg) (not (or (null arg) (string-empty-p arg)))) args)
   " "))

(defun komorebi-api--help-messages (help-message)
  "Return a list of help messages from HELP-MESSAGE."
  (split-string help-message "\n" t "[ \t\n\r\']*"))

(defun komorebi-api--help-error (help-message)
  "Insert HELP-MESSAGE into the current buffer."
  (let* ((tip-regex ".*\\(tip: some similar subcommands exist:\\)")
         (lines (komorebi-api--help-messages help-message))
         (title (capitalize (cl-first lines)))
         (tips (cl-second lines))
         (tip-info (if (string-match tip-regex tips)
                       (string-trim-left (match-string 0 tips)) ""))
         (tips-command (string-remove-prefix tip-info tips)))
    (insert (format "* %s\n" title))
    (insert (format "%s\n\n" tip-info))
    (dolist (command (split-string tips-command "," t "[ \t\n\r\']*"))
      (let ((command-help (komorebi-api--execute command "--help")))
        (komorebi-api--help-success command command-help 2)
        (insert "\n")))))


(defun komorebi-api--help-insert-src-block (command)
  "Insert SRC-BLOCK of COMMAND into current buffer."
  (let ((exe (file-name-nondirectory komorebi-api-executable)))
    (insert "#+BEGIN_SRC sh\n")
    (when (string-prefix-p exe command)
      (setq command (string-trim (string-remove-prefix exe command))))
    (insert (format "%s %s\n" komorebi-api-executable command))
    (insert "#+END_SRC\n")))


(defun komorebi-api--help-success (command help-message indent)
  "Insert successful HELP-MESSAGE of COMMAND into current buffer.
INDENT is the level of org-heading."
  (insert (format "%s %s\n" (make-string indent ?*)
                  (capitalize command)))
  (let ((cmd-url (command-url (komorebi-web-cli-command command))))
    (insert (format "\n[[%s][Documentation]]\n\n" cmd-url)))
  (let ((option-found nil)
        (argument-found nil)
        (arg-regex "<\\([a-zA-Z]*\\)>"))
    (dolist (line (komorebi-api--help-messages help-message))
      (cond ((string-prefix-p "Usage:" line)
             (insert "\nUsage:\n")
             (let ((cmd (string-remove-prefix "Usage:" line)))
               (komorebi-api--help-insert-src-block (string-trim cmd)))
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


(defun komorebi-api-command-help (command)
  "Return help of komorebi COMMAND."
  (interactive (list (completing-read "Command: " (seq-map (lambda (cmd) (oref cmd name))
                                                           (komorebi-web-cli-commands)))))
  (let* ((help-message (komorebi-api--execute command "--help"))
         (cmd-buffer (get-buffer-create komorebi-api-help-buffer-name))
         (is-error (string-match "error" help-message))
         (org-startup-folded (if is-error 'show2levels 'showall)))
    (with-current-buffer cmd-buffer
      (view-mode-exit)
      (read-only-mode 0)
      (erase-buffer)
      (if is-error
          (komorebi-api--help-error help-message)
        (komorebi-api--help-success command help-message 1))
      (org-mode)
      (read-only-mode 'toggle)
      (view-mode-enter)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))


(defun komorebi-api--command-at-point ()
  "Return komorebi command at point."
  (let ((command (thing-at-point 'symbol t)))
    (when (null command)
      (user-error "No komorebi command found at point"))
    (string-remove-prefix "komorebi-api-" command)))


(defun komorebi-api-command-help-at-point ()
  "Return help of komorebi command at point."
  (interactive)
  (let ((command (komorebi-api--command-at-point)))
    (komorebi-api-command-help command)))


(defun komorebi-api--execute (command &rest args)
  "Execute komorebi COMMAND with ARGS in shell."
  (let ((shell-cmd (format "%s %s %s"
                           (shell-quote-argument komorebi-api-executable)
                           command
                           (komorebi-api--args-get args))))
    (let ((result (shell-command-to-string shell-cmd)))
      (message "Executing: %s\n%s" shell-cmd result)
      result)))


(defun komorebi-api-create ()
  "Create the function for each komorebi API command."
  (dolist (command (komorebi-web-cli-commands))
    (let ((args (function-args command)))
      (if (length= args 0)
          (defalias (intern (function-name command))
            (lambda ()
              (interactive)
              (apply #'komorebi-api--execute (oref command name)))
            (lisp-description command))
        (defalias (intern (function-name command))
          (lambda (&rest args)
            (apply #'komorebi-api--execute (oref command name) args))
          (lisp-description command))))))


(defun komorebi-api-function-lines (command)
  "Return the function lines of komorebi COMMAND."
  (seq-filter
   #'identity
   (list (unless (function-args-p command)
           ";;;###autoload")
         (format
          "(cl-defun %s (%s)\n  \"%s\""
          (function-name command)
          (attribute-list command)
          (lisp-description command))
         (unless (function-args-p command)
           "  (interactive)")
         (format "  (komorebi-api--execute \"%s\"%s))"
                 (oref command name)
                 (attribute-names command)))))


(defun komorebi-api--commands ()
  "Return sorted list of komorebi API commands."
  (sort (komorebi-web-cli-commands)
        (lambda (cmd oth)
          (string-lessp (function-name cmd)
                        (function-name oth)))))

(defun komorebi-api-create-func ()
  "Create the function for each komorebi API command."
  (interactive)
  (save-excursion
    (forward-line 1)
    (narrow-to-region (point)
                      (save-excursion
                        (search-forward "(provide 'komorebi-api)")
                        (forward-line -1)
                        (point)))
    (delete-region (point-min) (point-max))
    (insert ";;;\n")
    (insert ";;; Generated functions\n")
    (dolist (command (komorebi-api--commands))
      (insert "\n")
      (insert (string-join (komorebi-api-function-lines command) "\n"))
      (insert "\n"))
    (insert ";;; End generated functions\n")
    (widen)))


;;;
;;; Generated functions

(cl-defun komorebi-api-adjust-container-padding (&key sizing adjustment)
  "Adjust container padding on the focused workspace.
SIZING
   [possible values: increase, decrease]
ADJUSTMENT
   Pixels to adjust by as an integer"
  (komorebi-api--execute "adjust-container-padding" sizing adjustment))

(cl-defun komorebi-api-adjust-workspace-padding (&key sizing adjustment)
  "Adjust workspace padding on the focused workspace.
SIZING
   [possible values: increase, decrease]
ADJUSTMENT
   Pixels to adjust by as an integer"
  (komorebi-api--execute "adjust-workspace-padding" sizing adjustment))

(cl-defun komorebi-api-ahk-app-specific-configuration (&key path)
  "Generate common app-specific configurations and fixes to use in komorebi.ahk.
PATH
   "
  (komorebi-api--execute "ahk-app-specific-configuration" path))

(cl-defun komorebi-api-animation (&key boolean-state)
  "Enable or disable movement animations.
BOOLEAN-STATE
   [possible values: enable, disable]"
  (komorebi-api--execute "animation" boolean-state))

(cl-defun komorebi-api-animation-duration (&key duration)
  "Set the duration for movement animations in ms.
DURATION
   Desired animation durations in ms"
  (komorebi-api--execute "animation-duration" duration))

(cl-defun komorebi-api-animation-fps (&key fps)
  "Set the frames per second for movement animations.
FPS
   Desired animation frames per second"
  (komorebi-api--execute "animation-fps" fps))

(cl-defun komorebi-api-animation-style (&key style)
  "Set the ease function for movement animations.
STYLE
   default:
- linear]"
  (komorebi-api--execute "animation-style" style))

(cl-defun komorebi-api-border (&key boolean-state)
  "Enable or disable borders.
BOOLEAN-STATE
   [possible values: enable, disable]"
  (komorebi-api--execute "border" boolean-state))

(cl-defun komorebi-api-border-colour (&key r g b window-kind)
  "Set the colour for a window border kind.
R
   Red
G
   Green
B
   Blue
WINDOW-KIND
   default: single]
possible values: single, stack, monocle, unfocused]"
  (komorebi-api--execute "border-colour" r g b window-kind))

(cl-defun komorebi-api-border-implementation (&key style)
  "Set the border implementation.
STYLE
   "
  (komorebi-api--execute "border-implementation" style))

(cl-defun komorebi-api-border-offset (&key offset)
  "Set the border offset.
OFFSET
   Desired offset of the window border"
  (komorebi-api--execute "border-offset" offset))

(cl-defun komorebi-api-border-style (&key style)
  "Set the border style.
STYLE
   "
  (komorebi-api--execute "border-style" style))

(cl-defun komorebi-api-border-width (&key width)
  "Set the border width.
WIDTH
   Desired width of the window border"
  (komorebi-api--execute "border-width" width))

(cl-defun komorebi-api-change-layout (&key default-layout)
  "Set the layout on the focused workspace.
DEFAULT-LAYOUT
   [possible values:
- bsp
- columns
- rows
- vertical-stack
- horizontal-stack
- ultrawide-vertical-stack
- grid
- right-main-vertical-stack]"
  (komorebi-api--execute "change-layout" default-layout))

(cl-defun komorebi-api-clear-named-workspace-layout-rules (&key workspace)
  "Clear all dynamic layout rules for the specified workspace.
WORKSPACE
   Target workspace name"
  (komorebi-api--execute "clear-named-workspace-layout-rules" workspace))

(cl-defun komorebi-api-clear-named-workspace-rules (&key workspace)
  "Remove all application association rules for a named workspace.
WORKSPACE
   Name of a workspace"
  (komorebi-api--execute "clear-named-workspace-rules" workspace))

(cl-defun komorebi-api-clear-workspace-layout-rules (&key monitor workspace)
  "Clear all dynamic layout rules for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)"
  (komorebi-api--execute "clear-workspace-layout-rules" monitor workspace))

(cl-defun komorebi-api-clear-workspace-rules (&key monitor workspace)
  "
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)"
  (komorebi-api--execute "clear-workspace-rules" monitor workspace))

(cl-defun komorebi-api-container-padding (&key monitor workspace size)
  "Set the container padding for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
SIZE
   Pixels to pad with as an integer"
  (komorebi-api--execute "container-padding" monitor workspace size))

(cl-defun komorebi-api-cross-monitor-move-behaviour (&key move-behaviour)
  "Set the behaviour when moving windows across monitor boundaries.
MOVE-BEHAVIOUR
   "
  (komorebi-api--execute "cross-monitor-move-behaviour" move-behaviour))

(cl-defun komorebi-api-cycle-focus (&key cycle-direction)
  "Change focus to the window in the specified cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-focus" cycle-direction))

(cl-defun komorebi-api-cycle-layout (&key cycle-direction)
  "Cycle between available layouts.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-layout" cycle-direction))

(cl-defun komorebi-api-cycle-monitor (&key cycle-direction)
  "Focus the monitor in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-monitor" cycle-direction))

(cl-defun komorebi-api-cycle-move (&key cycle-direction)
  "Move the focused window in the specified cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-move" cycle-direction))

(cl-defun komorebi-api-cycle-move-to-monitor (&key cycle-direction)
  "Move the focused window to the monitor in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-move-to-monitor" cycle-direction))

(cl-defun komorebi-api-cycle-move-to-workspace (&key cycle-direction)
  "Move the focused window to the workspace in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-move-to-workspace" cycle-direction))

(cl-defun komorebi-api-cycle-move-workspace-to-monitor (&key cycle-direction)
  "Move the focused workspace monitor in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-move-workspace-to-monitor" cycle-direction))

(cl-defun komorebi-api-cycle-send-to-monitor (&key cycle-direction)
  "Send the focused window to the monitor in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-send-to-monitor" cycle-direction))

(cl-defun komorebi-api-cycle-send-to-workspace (&key cycle-direction)
  "Send the focused window to the workspace in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-send-to-workspace" cycle-direction))

(cl-defun komorebi-api-cycle-stack (&key cycle-direction)
  "Cycle the focused stack in the specified cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-stack" cycle-direction))

(cl-defun komorebi-api-cycle-workspace (&key cycle-direction)
  "Focus the workspace in the given cycle direction.
CYCLE-DIRECTION
   [possible values: previous, next]"
  (komorebi-api--execute "cycle-workspace" cycle-direction))

(cl-defun komorebi-api-display-index-preference (&key index-preference display)
  "
INDEX-PREFERENCE
   Preferred monitor index (zero-indexed)
DISPLAY
   Display name as identified in komorebic state"
  (komorebi-api--execute "display-index-preference" index-preference display))

(cl-defun komorebi-api-enable-autostart (&key config)
  "Generates the komorebi.lnk shortcut in shell:startup to autostart komorebi.
CONFIG
   "
  (komorebi-api--execute "enable-autostart" config))

(cl-defun komorebi-api-ensure-named-workspaces (&key monitor)
  "Create these many named workspaces for the specified monitor.
MONITOR
   Monitor index (zero-indexed)  [NAMES]...          Names of desired workspaces"
  (komorebi-api--execute "ensure-named-workspaces" monitor))

(cl-defun komorebi-api-ensure-workspaces (&key monitor workspace-count)
  "Create at least this many workspaces for the specified monitor.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE-COUNT
   Number of desired workspaces"
  (komorebi-api--execute "ensure-workspaces" monitor workspace-count))

(cl-defun komorebi-api-flip-layout (&key axis)
  "Flip the layout on the focused workspace (BSP only).
AXIS
   [possible values: horizontal, vertical, horizontal-and-vertical]"
  (komorebi-api--execute "flip-layout" axis))

(cl-defun komorebi-api-float-rule (&key identifier id)
  "Add a rule to always float the specified application.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string"
  (komorebi-api--execute "float-rule" identifier id))

(cl-defun komorebi-api-focus (&key operation-direction)
  "Change focus to the window in the specified direction.
OPERATION-DIRECTION
   [possible values: left, right, up, down]"
  (komorebi-api--execute "focus" operation-direction))

(cl-defun komorebi-api-focus-follows-mouse (&key boolean-state implementation)
  "Enable or disable focus follows mouse for the operating system.
BOOLEAN-STATE
   [possible values: enable, disable]
IMPLEMENTATION
   "
  (komorebi-api--execute "focus-follows-mouse" boolean-state implementation))

(cl-defun komorebi-api-focus-monitor (&key target)
  "Focus the specified monitor.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "focus-monitor" target))

(cl-defun komorebi-api-focus-monitor-workspace (&key target-monitor target-workspace)
  "Focus the specified workspace on the target monitor.
TARGET-MONITOR
   Target monitor index (zero-indexed)
TARGET-WORKSPACE
   Workspace index on the target monitor (zero-indexed)"
  (komorebi-api--execute "focus-monitor-workspace" target-monitor target-workspace))

(cl-defun komorebi-api-focus-named-workspace (&key workspace)
  "Focus the specified workspace.
WORKSPACE
   Target workspace name"
  (komorebi-api--execute "focus-named-workspace" workspace))

(cl-defun komorebi-api-focus-stack-window (&key target)
  "Focus the specified window index in the focused stack.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "focus-stack-window" target))

(cl-defun komorebi-api-focus-workspace (&key target)
  "Focus the specified workspace on the focused monitor.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "focus-workspace" target))

(cl-defun komorebi-api-focus-workspaces (&key target)
  "Focus the specified workspace on all monitors.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "focus-workspaces" target))

(cl-defun komorebi-api-focused-workspace-container-padding (&key size)
  "Set container padding on the focused workspace.
SIZE
   Pixels size to set as an integer"
  (komorebi-api--execute "focused-workspace-container-padding" size))

(cl-defun komorebi-api-focused-workspace-padding (&key size)
  "Set workspace padding on the focused workspace.
SIZE
   Pixels size to set as an integer"
  (komorebi-api--execute "focused-workspace-padding" size))

(cl-defun komorebi-api-format-app-specific-configuration (&key path)
  "Format a YAML file for use with the 'ahk-app-specific-configuration' command.
PATH
   YAML file from which the application-specific configurations should be loaded"
  (komorebi-api--execute "format-app-specific-configuration" path))

(cl-defun komorebi-api-global-work-area-offset (&key left top right bottom)
  "Set offsets to exclude parts of the work area from tiling.
LEFT

TOP

RIGHT
   Size of the right work area offset
BOTTOM
   Size of the bottom work area offset"
  (komorebi-api--execute "global-work-area-offset" left top right bottom))

(cl-defun komorebi-api-identify-layered-application (&key identifier id)
  "Identify an application that has WS_EX_LAYERED, but should still be managed.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string"
  (komorebi-api--execute "identify-layered-application" identifier id))

(cl-defun komorebi-api-identify-object-name-change-application (&key identifier id)
  "Identify an application that sends EVENT_OBJECT_NAMECHANGE on launch.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string"
  (komorebi-api--execute "identify-object-name-change-application" identifier id))

(cl-defun komorebi-api-identify-tray-application (&key identifier id)
  "Identify an application that closes to the system tray.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string"
  (komorebi-api--execute "identify-tray-application" identifier id))

(cl-defun komorebi-api-initial-named-workspace-rule (&key identifier id workspace)
  "Add a rule to associate an application with a named workspace on first show.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string
WORKSPACE
   Name of a workspace"
  (komorebi-api--execute "initial-named-workspace-rule" identifier id workspace))

(cl-defun komorebi-api-initial-workspace-rule (&key identifier id monitor workspace)
  "Add a rule to associate an application with a workspace on first show.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)"
  (komorebi-api--execute "initial-workspace-rule" identifier id monitor workspace))

(cl-defun komorebi-api-invisible-borders (&key left top right bottom)
  "Set the invisible border dimensions around each window.
LEFT
   Size of the left invisible border
TOP
   Size of the top invisible border (usually 0)
RIGHT
   Size of the right invisible border (usually left * 2)
BOTTOM
   Size of the bottom invisible border (usually the same as left)"
  (komorebi-api--execute "invisible-borders" left top right bottom))

(cl-defun komorebi-api-load-custom-layout (&key path)
  "Load a custom layout from file for the focused workspace.
PATH
   JSON or YAML file from which the custom layout definition should be loaded"
  (komorebi-api--execute "load-custom-layout" path))

(cl-defun komorebi-api-load-resize (&key path)
  "Load the resize layout dimensions from a file.
PATH
   File from which the resize layout dimensions should be loaded"
  (komorebi-api--execute "load-resize" path))

(cl-defun komorebi-api-manage-rule (&key identifier id)
  "Add a rule to always manage the specified application.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string"
  (komorebi-api--execute "manage-rule" identifier id))

(cl-defun komorebi-api-monitor-index-preference (&key index-preference left top right bottom)
  "Set the monitor index preference for a monitor identified using its size.
INDEX-PREFERENCE
   Preferred monitor index (zero-indexed)
LEFT
   Left value of the monitor's size Rect
TOP
   Top value of the monitor's size Rect
RIGHT
   Right value of the monitor's size Rect
BOTTOM
   Bottom value of the monitor's size Rect"
  (komorebi-api--execute "monitor-index-preference" index-preference left top right bottom))

(cl-defun komorebi-api-monitor-work-area-offset (&key monitor left top right bottom)
  "Set offsets for a monitor to exclude parts of the work area from tiling.
MONITOR
   Monitor index (zero-indexed)
LEFT

TOP

RIGHT
   Size of the right work area offset
BOTTOM
   Size of the bottom work area offset"
  (komorebi-api--execute "monitor-work-area-offset" monitor left top right bottom))

(cl-defun komorebi-api-mouse-follows-focus (&key boolean-state)
  "Enable or disable mouse follows focus on all workspaces.
BOOLEAN-STATE
   [possible values: enable, disable]"
  (komorebi-api--execute "mouse-follows-focus" boolean-state))

(cl-defun komorebi-api-move (&key operation-direction)
  "Move the focused window in the specified direction.
OPERATION-DIRECTION
   [possible values: left, right, up, down]"
  (komorebi-api--execute "move" operation-direction))

(cl-defun komorebi-api-move-to-monitor (&key target)
  "Move the focused window to the specified monitor.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "move-to-monitor" target))

(cl-defun komorebi-api-move-to-monitor-workspace (&key target-monitor target-workspace)
  "Move the focused window to the specified monitor workspace.
TARGET-MONITOR
   Target monitor index (zero-indexed)
TARGET-WORKSPACE
   Workspace index on the target monitor (zero-indexed)"
  (komorebi-api--execute "move-to-monitor-workspace" target-monitor target-workspace))

(cl-defun komorebi-api-move-to-named-workspace (&key workspace)
  "Move the focused window to the specified workspace.
WORKSPACE
   Target workspace name"
  (komorebi-api--execute "move-to-named-workspace" workspace))

(cl-defun komorebi-api-move-to-workspace (&key target)
  "Move the focused window to the specified workspace.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "move-to-workspace" target))

(cl-defun komorebi-api-move-workspace-to-monitor (&key target)
  "Move the focused workspace to the specified monitor.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "move-workspace-to-monitor" target))

(cl-defun komorebi-api-named-workspace-container-padding (&key workspace size)
  "Set the container padding for the specified workspace.
WORKSPACE
   Target workspace name
SIZE
   Pixels to pad with as an integer"
  (komorebi-api--execute "named-workspace-container-padding" workspace size))

(cl-defun komorebi-api-named-workspace-custom-layout (&key workspace path)
  "Set a custom layout for the specified workspace.
WORKSPACE
   Target workspace name
PATH
   JSON or YAML file from which the custom layout definition should be loaded"
  (komorebi-api--execute "named-workspace-custom-layout" workspace path))

(cl-defun komorebi-api-named-workspace-custom-layout-rule (&key workspace at-container-count path)
  "Add a dynamic custom layout for the specified workspace.
WORKSPACE
   Target workspace name
AT-CONTAINER-COUNT
   The number of window containers on-screen required to trigger this layout rule
PATH
   JSON or YAML file from which the custom layout definition should be loaded"
  (komorebi-api--execute "named-workspace-custom-layout-rule" workspace at-container-count path))

(cl-defun komorebi-api-named-workspace-layout (&key workspace value)
  "Set the layout for the specified workspace.
WORKSPACE
   Target workspace name
VALUE
   [possible values:
- bsp
- columns
- rows
- vertical-stack
- horizontal-stack
- ultrawide-vertical-stack
- grid
- right-main-vertical-stack]"
  (komorebi-api--execute "named-workspace-layout" workspace value))

(cl-defun komorebi-api-named-workspace-layout-rule (&key workspace at-container-count layout)
  "Add a dynamic layout rule for the specified workspace.
WORKSPACE
   Target workspace name
AT-CONTAINER-COUNT
   The number of window containers on-screen required to trigger this layout rule
LAYOUT
   [possible values:
- bsp
- columns
- rows
- vertical-stack
- horizontal-stack
- ultrawide-vertical-stack
- grid
- right-main-vertical-stack]"
  (komorebi-api--execute "named-workspace-layout-rule" workspace at-container-count layout))

(cl-defun komorebi-api-named-workspace-padding (&key workspace size)
  "Set the workspace padding for the specified workspace.
WORKSPACE
   Target workspace name
SIZE
   Pixels to pad with as an integer"
  (komorebi-api--execute "named-workspace-padding" workspace size))

(cl-defun komorebi-api-named-workspace-rule (&key identifier id workspace)
  "Add a rule to associate an application with a named workspace.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string
WORKSPACE
   Name of a workspace"
  (komorebi-api--execute "named-workspace-rule" identifier id workspace))

(cl-defun komorebi-api-named-workspace-tiling (&key workspace value)
  "Enable or disable window tiling for the specified workspace.
WORKSPACE
   Target workspace name
VALUE
   [possible values: enable, disable]"
  (komorebi-api--execute "named-workspace-tiling" workspace value))

(cl-defun komorebi-api-promote-window (&key operation-direction)
  "Promote the window in the specified direction.
OPERATION-DIRECTION
   [possible values: left, right, up, down]"
  (komorebi-api--execute "promote-window" operation-direction))

(cl-defun komorebi-api-pwsh-app-specific-configuration (&key path)
  "Generate common app-specific configurations and fixes in a PowerShell script.
PATH
   "
  (komorebi-api--execute "pwsh-app-specific-configuration" path))

(cl-defun komorebi-api-query (&key state-query)
  "Query the current window manager state.
STATE-QUERY
   [possible values:
- focused-monitor-index
- focused-workspace-index
- focused-container-index
- focused-window-index]"
  (komorebi-api--execute "query" state-query))

(cl-defun komorebi-api-remove-title-bar (&key identifier id)
  "Whitelist an application for title bar removal.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string"
  (komorebi-api--execute "remove-title-bar" identifier id))

(cl-defun komorebi-api-replace-configuration (&key path)
  "
PATH
   Static configuration JSON file from which the configuration should be loaded"
  (komorebi-api--execute "replace-configuration" path))

(cl-defun komorebi-api-resize-axis (&key axis sizing)
  "Resize the focused window or primary column along the specified axis.
AXIS
   [possible values: horizontal, vertical, horizontal-and-vertical]
SIZING
   [possible values: increase, decrease]"
  (komorebi-api--execute "resize-axis" axis sizing))

(cl-defun komorebi-api-resize-delta (&key pixels)
  "Set the resize delta (used by resize-edge and resize-axis).
PIXELS
   "
  (komorebi-api--execute "resize-delta" pixels))

(cl-defun komorebi-api-resize-edge (&key edge sizing)
  "Resize the focused window in the specified direction.
EDGE
   [possible values: left, right, up, down]
SIZING
   [possible values: increase, decrease]"
  (komorebi-api--execute "resize-edge" edge sizing))

(cl-defun komorebi-api-save-resize (&key path)
  "Save the current resize layout dimensions to a file.
PATH
   File to which the resize layout dimensions should be saved"
  (komorebi-api--execute "save-resize" path))

(cl-defun komorebi-api-send-to-monitor (&key target)
  "Send the focused window to the specified monitor.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "send-to-monitor" target))

(cl-defun komorebi-api-send-to-monitor-workspace (&key target-monitor target-workspace)
  "Send the focused window to the specified monitor workspace.
TARGET-MONITOR
   Target monitor index (zero-indexed)
TARGET-WORKSPACE
   Workspace index on the target monitor (zero-indexed)"
  (komorebi-api--execute "send-to-monitor-workspace" target-monitor target-workspace))

(cl-defun komorebi-api-send-to-named-workspace (&key workspace)
  "Send the focused window to the specified workspace.
WORKSPACE
   Target workspace name"
  (komorebi-api--execute "send-to-named-workspace" workspace))

(cl-defun komorebi-api-send-to-workspace (&key target)
  "Send the focused window to the specified workspace.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "send-to-workspace" target))

(cl-defun komorebi-api-stack (&key operation-direction)
  "Stack the focused window in the specified direction.
OPERATION-DIRECTION
   [possible values: left, right, up, down]"
  (komorebi-api--execute "stack" operation-direction))

(cl-defun komorebi-api-subscribe-pipe (&key named-pipe)
  "Subscribe to komorebi events using a Named Pipe.
NAMED-PIPE
   Name of the pipe to send event notifications to (without '.pipe' prepended)"
  (komorebi-api--execute "subscribe-pipe" named-pipe))

(cl-defun komorebi-api-subscribe-socket (&key socket)
  "Subscribe to komorebi events using a Unix Domain Socket.
SOCKET
   Name of the socket to send event notifications to"
  (komorebi-api--execute "subscribe-socket" socket))

(cl-defun komorebi-api-swap-workspaces-with-monitor (&key target)
  "Swap focused monitor workspaces with specified monitor.
TARGET
   Target index (zero-indexed)"
  (komorebi-api--execute "swap-workspaces-with-monitor" target))

(cl-defun komorebi-api-toggle-focus-follows-mouse (&key implementation)
  "Toggle focus follows mouse for the operating system.
IMPLEMENTATION
   "
  (komorebi-api--execute "toggle-focus-follows-mouse" implementation))

(cl-defun komorebi-api-transparency (&key boolean-state)
  "Enable or disable transparency for unfocused windows.
BOOLEAN-STATE
   [possible values: enable, disable]"
  (komorebi-api--execute "transparency" boolean-state))

(cl-defun komorebi-api-transparency-alpha (&key alpha)
  "Set the alpha value for unfocused window transparency.
ALPHA
   Alpha"
  (komorebi-api--execute "transparency-alpha" alpha))

(cl-defun komorebi-api-unmanaged-window-operation-behaviour (&key operation-behaviour)
  "Set the operation behaviour when the focused window is not managed.
OPERATION-BEHAVIOUR
   "
  (komorebi-api--execute "unmanaged-window-operation-behaviour" operation-behaviour))

(cl-defun komorebi-api-unsubscribe-pipe (&key named-pipe)
  "Unsubscribe from komorebi events.
NAMED-PIPE
   "
  (komorebi-api--execute "unsubscribe-pipe" named-pipe))

(cl-defun komorebi-api-unsubscribe-socket (&key socket)
  "Unsubscribe from komorebi events.
SOCKET
   Name of the socket to stop sending event notifications to"
  (komorebi-api--execute "unsubscribe-socket" socket))

(cl-defun komorebi-api-watch-configuration (&key boolean-state)
  "
BOOLEAN-STATE
   [possible values: enable, disable]"
  (komorebi-api--execute "watch-configuration" boolean-state))

(cl-defun komorebi-api-window-hiding-behaviour (&key hiding-behaviour)
  "Set the window behaviour when switching workspaces / cycling stacks.
HIDING-BEHAVIOUR
   "
  (komorebi-api--execute "window-hiding-behaviour" hiding-behaviour))

(cl-defun komorebi-api-workspace-custom-layout (&key monitor workspace path)
  "Set a custom layout for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
PATH
   JSON or YAML file from which the custom layout definition should be loaded"
  (komorebi-api--execute "workspace-custom-layout" monitor workspace path))

(cl-defun komorebi-api-workspace-custom-layout-rule (&key monitor workspace at-container-count path)
  "Add a dynamic custom layout for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
AT-CONTAINER-COUNT
   The number of window containers on-screen required to trigger this layout rule
PATH
   JSON or YAML file from which the custom layout definition should be loaded"
  (komorebi-api--execute "workspace-custom-layout-rule" monitor workspace at-container-count path))

(cl-defun komorebi-api-workspace-layout (&key monitor workspace value)
  "Set the layout for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
VALUE
   [possible values:
- bsp
- columns
- rows
- vertical-stack
- horizontal-stack
- ultrawide-vertical-stack
- grid
- right-main-vertical-stack]"
  (komorebi-api--execute "workspace-layout" monitor workspace value))

(cl-defun komorebi-api-workspace-layout-rule (&key monitor workspace at-container-count layout)
  "Add a dynamic layout rule for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
AT-CONTAINER-COUNT
   The number of window containers on-screen required to trigger this layout rule
LAYOUT
   [possible values:
- bsp
- columns
- rows
- vertical-stack
- horizontal-stack
- ultrawide-vertical-stack
- grid
- right-main-vertical-stack]"
  (komorebi-api--execute "workspace-layout-rule" monitor workspace at-container-count layout))

(cl-defun komorebi-api-workspace-name (&key monitor workspace value)
  "Set the workspace name for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
VALUE
   Name of the workspace as a String"
  (komorebi-api--execute "workspace-name" monitor workspace value))

(cl-defun komorebi-api-workspace-padding (&key monitor workspace size)
  "Set the workspace padding for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
SIZE
   Pixels to pad with as an integer"
  (komorebi-api--execute "workspace-padding" monitor workspace size))

(cl-defun komorebi-api-workspace-rule (&key identifier id monitor workspace)
  "Add a rule to associate an application with a workspace.
IDENTIFIER
   [possible values: exe, class, title, path]
ID
   Identifier as a string
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)"
  (komorebi-api--execute "workspace-rule" identifier id monitor workspace))

(cl-defun komorebi-api-workspace-tiling (&key monitor workspace value)
  "Enable or disable window tiling for the specified workspace.
MONITOR
   Monitor index (zero-indexed)
WORKSPACE
   Workspace index on the specified monitor (zero-indexed)
VALUE
   [possible values: enable, disable]"
  (komorebi-api--execute "workspace-tiling" monitor workspace value))

;;;###autoload
(cl-defun komorebi-application-specific-configuration-schema ()
  "Generate a JSON Schema for applications.yaml."
  (interactive)
  (komorebi-api--execute "application-specific-configuration-schema"))

;;;###autoload
(cl-defun komorebi-bar-configuration ()
  "Show the path to komorebi.bar.json."
  (interactive)
  (komorebi-api--execute "bar-configuration"))

;;;###autoload
(cl-defun komorebi-check ()
  "Check komorebi configuration and related files for common errors."
  (interactive)
  (komorebi-api--execute "check"))

;;;###autoload
(cl-defun komorebi-clear-all-workspace-rules ()
  "Remove all application association rules for all workspaces."
  (interactive)
  (komorebi-api--execute "clear-all-workspace-rules"))

;;;###autoload
(cl-defun komorebi-close ()
  "Close the focused window."
  (interactive)
  (komorebi-api--execute "close"))

;;;###autoload
(cl-defun komorebi-complete-configuration ()
  ""
  (interactive)
  (komorebi-api--execute "complete-configuration"))

;;;###autoload
(cl-defun komorebi-configuration ()
  "Show the path to komorebi.json."
  (interactive)
  (komorebi-api--execute "configuration"))

;;;###autoload
(cl-defun komorebi-disable-autostart ()
  "Deletes the komorebi.lnk shortcut in shell:startup to disable autostart."
  (interactive)
  (komorebi-api--execute "disable-autostart"))

;;;###autoload
(cl-defun komorebi-fetch-app-specific-configuration ()
  ""
  (interactive)
  (komorebi-api--execute "fetch-app-specific-configuration"))

;;;###autoload
(cl-defun komorebi-focus-last-workspace ()
  "Focus the last focused workspace on the focused monitor."
  (interactive)
  (komorebi-api--execute "focus-last-workspace"))

;;;###autoload
(cl-defun komorebi-force-focus ()
  "Forcibly focus the window at the cursor with a left mouse click."
  (interactive)
  (komorebi-api--execute "force-focus"))

;;;###autoload
(cl-defun komorebi-generate-static-config ()
  ""
  (interactive)
  (komorebi-api--execute "generate-static-config"))

;;;###autoload
(cl-defun komorebi-global-state ()
  "Show a JSON representation of the current global state."
  (interactive)
  (komorebi-api--execute "global-state"))

;;;###autoload
(cl-defun komorebi-gui ()
  "Launch the komorebi-gui debugging tool."
  (interactive)
  (komorebi-api--execute "gui"))

;;;###autoload
(cl-defun komorebi-log ()
  "Tail komorebi.exe's process logs (cancel with Ctrl-C)."
  (interactive)
  (komorebi-api--execute "log"))

;;;###autoload
(cl-defun komorebi-manage ()
  "Force komorebi to manage the focused window."
  (interactive)
  (komorebi-api--execute "manage"))

;;;###autoload
(cl-defun komorebi-minimize ()
  "Minimize the focused window."
  (interactive)
  (komorebi-api--execute "minimize"))

;;;###autoload
(cl-defun komorebi-monitor-information ()
  "Show information about connected monitors."
  (interactive)
  (komorebi-api--execute "monitor-information"))

;;;###autoload
(cl-defun komorebi-new-workspace ()
  "Create and append a new workspace on the focused monitor."
  (interactive)
  (komorebi-api--execute "new-workspace"))

;;;###autoload
(cl-defun komorebi-notification-schema ()
  "Generate a JSON Schema of subscription notifications."
  (interactive)
  (komorebi-api--execute "notification-schema"))

;;;###autoload
(cl-defun komorebi-promote ()
  "Promote the focused window to the top of the tree."
  (interactive)
  (komorebi-api--execute "promote"))

;;;###autoload
(cl-defun komorebi-promote-focus ()
  "Promote the user focus to the top of the tree."
  (interactive)
  (komorebi-api--execute "promote-focus"))

;;;###autoload
(cl-defun komorebi-quick-load-resize ()
  "Load the last quicksaved resize layout dimensions."
  (interactive)
  (komorebi-api--execute "quick-load-resize"))

;;;###autoload
(cl-defun komorebi-quick-save-resize ()
  "Quicksave the current resize layout dimensions."
  (interactive)
  (komorebi-api--execute "quick-save-resize"))

;;;###autoload
(cl-defun komorebi-quickstart ()
  "Gather example configurations for a new-user quickstart."
  (interactive)
  (komorebi-api--execute "quickstart"))

;;;###autoload
(cl-defun komorebi-reload-configuration ()
  "Reload legacy komorebi.ahk or komorebi.ps1 configurations (if they exist)."
  (interactive)
  (komorebi-api--execute "reload-configuration"))

;;;###autoload
(cl-defun komorebi-restore-windows ()
  "Restore all hidden windows (debugging command)."
  (interactive)
  (komorebi-api--execute "restore-windows"))

;;;###autoload
(cl-defun komorebi-retile ()
  "Force the retiling of all managed windows."
  (interactive)
  (komorebi-api--execute "retile"))

;;;###autoload
(cl-defun komorebi-socket-schema ()
  "Generate a JSON Schema of socket messages."
  (interactive)
  (komorebi-api--execute "socket-schema"))

;;;###autoload
(cl-defun komorebi-stack-all ()
  "Stack all windows on the focused workspace."
  (interactive)
  (komorebi-api--execute "stack-all"))

;;;###autoload
(cl-defun komorebi-start ()
  "Start komorebi.exe as a background process."
  (interactive)
  (komorebi-api--execute "start"))

;;;###autoload
(cl-defun komorebi-state ()
  "Show a JSON representation of the current window manager state."
  (interactive)
  (komorebi-api--execute "state"))

;;;###autoload
(cl-defun komorebi-static-config-schema ()
  "Generate a JSON Schema of the static configuration file."
  (interactive)
  (komorebi-api--execute "static-config-schema"))

;;;###autoload
(cl-defun komorebi-stop ()
  "
.

"
  (interactive)
  (komorebi-api--execute "stop"))

;;;###autoload
(cl-defun komorebi-toggle-cross-monitor-move-behaviour ()
  "Toggle the behaviour when moving windows across monitor boundaries."
  (interactive)
  (komorebi-api--execute "toggle-cross-monitor-move-behaviour"))

;;;###autoload
(cl-defun komorebi-toggle-float ()
  "Toggle floating mode for the focused window."
  (interactive)
  (komorebi-api--execute "toggle-float"))

;;;###autoload
(cl-defun komorebi-toggle-maximize ()
  "Toggle native maximization for the focused window."
  (interactive)
  (komorebi-api--execute "toggle-maximize"))

;;;###autoload
(cl-defun komorebi-toggle-monocle ()
  "Toggle monocle mode for the focused container."
  (interactive)
  (komorebi-api--execute "toggle-monocle"))

;;;###autoload
(cl-defun komorebi-toggle-mouse-follows-focus ()
  "Toggle mouse follows focus on all workspaces."
  (interactive)
  (komorebi-api--execute "toggle-mouse-follows-focus"))

;;;###autoload
(cl-defun komorebi-toggle-pause ()
  "Toggle window tiling on the focused workspace."
  (interactive)
  (komorebi-api--execute "toggle-pause"))

;;;###autoload
(cl-defun komorebi-toggle-tiling ()
  "Toggle window tiling on the focused workspace."
  (interactive)
  (komorebi-api--execute "toggle-tiling"))

;;;###autoload
(cl-defun komorebi-toggle-title-bars ()
  "Toggle title bars for whitelisted applications."
  (interactive)
  (komorebi-api--execute "toggle-title-bars"))

;;;###autoload
(cl-defun komorebi-toggle-transparency ()
  "Toggle transparency for unfocused windows."
  (interactive)
  (komorebi-api--execute "toggle-transparency"))

;;;###autoload
(cl-defun komorebi-toggle-window-container-behaviour ()
  "Toggle the behaviour for new windows (stacking or dynamic tiling)."
  (interactive)
  (komorebi-api--execute "toggle-window-container-behaviour"))

;;;###autoload
(cl-defun komorebi-unmanage ()
  "Unmanage a window that was forcibly managed."
  (interactive)
  (komorebi-api--execute "unmanage"))

;;;###autoload
(cl-defun komorebi-unstack ()
  "Unstack the focused window."
  (interactive)
  (komorebi-api--execute "unstack"))

;;;###autoload
(cl-defun komorebi-unstack-all ()
  "Unstack all windows in the focused container."
  (interactive)
  (komorebi-api--execute "unstack-all"))

;;;###autoload
(cl-defun komorebi-visible-windows ()
  "Show a JSON representation of visible windows."
  (interactive)
  (komorebi-api--execute "visible-windows"))

;;;###autoload
(cl-defun komorebi-whkdrc ()
  "Show the path to whkdrc."
  (interactive)
  (komorebi-api--execute "whkdrc"))
;;; End generated functions

(provide 'komorebi-api)
;;; komorebi-api.el ends here

   ;; Local Variables:
   ;; jinx-local-words: "Quicksave Rect Unstack api behaviour bsp prepended pwsh quicksaved quickstart retile retiling ultrawide unmanage unmanaged unstack whkdrc"
   ;; End:
