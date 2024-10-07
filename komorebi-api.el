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

(defcustom komorebi-api-docs-url "https://lgug2z.github.io/komorebi/cli"
  "The version of komorebi."
  :type 'string
  :group 'komorebi-api)


(defun komorebi-api-browse-documentation (command)
  "Browse the online documentation for COMMAND at `komorebi-api-docs-url'."
  (interactive "sCommand: ")
  (browse-url (format "%s/%s.html" komorebi-api-docs-url command)))


(defvar komorebi-api-help-buffer-name "*komorebi-api-help*"
  "The name of the buffer to display command help.")

(defun komorebi-api--args-get (args)
  "Return string of ARGS."
  (string-join (seq-filter (lambda (arg)
                             (not (or (null arg) (string-empty-p arg))))
                           args)
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

(defun komorebi-api--help-success (command help-message indent)
  "Insert successful HELP-MESSAGE of COMMAND into current buffer.
INDENT is the level of org-heading."
  (insert (format "%s %s\n" (make-string indent ?*)
                  (capitalize command)))
  (let ((option-found nil)
        (argument-found nil)
        (arg-regex "<\\([a-zA-Z]*\\)>"))
    (dolist (line (komorebi-api--help-messages help-message))
      (cond ((and (not option-found) (string-prefix-p "Arguments:" line))
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
  (interactive "sCommand: ")
  (let* ((help-message (komorebi-api--execute command "--help"))
         (cmd-buffer (get-buffer-create komorebi-api-help-buffer-name))
         (is-error (string-match "error" help-message))
         (org-startup-folded (if is-error 'show3levels 'showall)))
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
      (pop-to-buffer (current-buffer)))))


(defun komorebi-api--execute (command &rest args)
  "Execute komorebi COMMAND with ARGS in shell."
  (let ((shell-cmd (format "%s %s %s"
                           (shell-quote-argument komorebi-api-executable)
                           command
                           (komorebi-api--args-get args))))
    (shell-command-to-string (string-trim shell-cmd))))


(cl-defun komorebi-api-start (&key ffm await-config tcp-port whkd ahk bar config)
  "Start komorebi with the given options.
FFM: allow `focus-follows-mouse'. BAR: enable the komorebi-bar.
AWAIT-CONFIG: await configuration before processing events.
TCP-PORT: the port to listen on for TCP connections.
WHKD: enable the WHKD library. AHK: enable the AHK library."
  (komorebi-api--execute "start" (if ffm "--ffm" nil)
                         (if await-config "--await-configuration" nil)
                         (if tcp-port (format "--tcp-port %s" tcp-port) nil)
                         (if whkd "--whkd" nil)
                         (if ahk "--ahk" nil)
                         (if bar "--bar" nil)
                         (if config (format "--config %s" config) nil)))

(defun komorebi-api-stop ()
  "Stop komorebi."
  (komorebi-api--execute "stop"))

(defun komorebi-api-configuration ()
  "Stop komorebi."
  (komorebi-api--execute "configuration"))

(defun komorebi-api-state ()
  "State."
  (komorebi-api--execute "state"))

(defun komorebi-api-query (state-query)
  "Query komorebi with STATE-QUERY."
  (komorebi-api--execute "query" state-query))

(defun komorebi-api-subscribe (named-pipe)
  "Subscribe to komorebi with NAMED-PIPE."
  (komorebi-api--execute "subscribe" named-pipe))

(defun komorebi-api-unsubscribe (named-pipe)
  "Unsubscribe to komorebi with NAMED-PIPE."
  (komorebi-api--execute "unsubscribe" named-pipe))

(defun komorebi-api-log ()
  "Log komorebi with LOG-LEVEL and MESSAGE."
  (komorebi-api--execute "log"))

(defun komorebi-api-quick-save-resize ()
  "Quick save resize."
  (komorebi-api--execute "quick-save-resize"))

(defun komorebi-api-quick-load-resize ()
  "Quick load resize."
  (komorebi-api--execute "quick-load-resize"))

(defun komorebi-api-save-resize (path)
  "Save resize with PATH."
  (komorebi-api--execute "save-resize" path))

(defun komorebi-api-load-resize (path)
  "Load resize with PATH."
  (komorebi-api--execute "load-resize" path))

(defun komorebi-api-focus (operation-direction)
  "Focus with OPERATION-DIRECTION."
  (komorebi-api--execute "focus" operation-direction))

(defun komorebi-api-move (operation-direction)
  "Move with OPERATION-DIRECTION."
  (komorebi-api--execute "move" operation-direction))

(defun komorebi-api-minimize ()
  "Minimize."
  (komorebi-api--execute "minimize"))

(defun komorebi-api-close ()
  "Close."
  (komorebi-api--execute "close"))

(defun komorebi-api-force-focus ()
  "Force focus."
  (komorebi-api--execute "force-focus"))

(defun komorebi-api-cycle-focus (cycle-direction)
  "Cycle focus with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-focus" cycle-direction))

(defun komorebi-api-cycle-move (cycle-direction)
  "Cycle move with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-move" cycle-direction))

(defun komorebi-api-stack (operation-direction)
  "Stack with OPERATION-DIRECTION."
  (komorebi-api--execute "stack" operation-direction))

(defun komorebi-api-resize (edge sizing)
  "Resize with EDGE and SIZING."
  (komorebi-api--execute "resize" edge sizing))

(defun komorebi-api-resize-axis (axis sizing)
  "Resize axis with AXIS and SIZING."
  (komorebi-api--execute "resize-axis" axis sizing))

(defun komorebi-api-unstack ()
  "Unstack the current stack of windows into separated windows."
  (komorebi-api--execute "unstack"))

(defun komorebi-api-cycle-stack (cycle-direction)
  "Cycle stack with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-stack" cycle-direction))

(defun komorebi-api-cycle-move-to-monitor (cycle-direction)
  "Cycle move to monitor with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-move-to-monitor" cycle-direction))

(defun komorebi-api-move-to-monitor (target)
  "Move to monitor with TARGET."
  (komorebi-api--execute "move-to-monitor" target))

(defun komorebi-api-move-to-workspace (target)
  "Move to workspace with TARGET."
  (komorebi-api--execute "move-to-workspace" target))

(defun komorebi-api-move-to-named-workspace (workspace)
  "Move to named workspace with WORKSPACE."
  (komorebi-api--execute "move-to-named-workspace" workspace))

(defun komorebi-api-cycle-move-to-workspace (cycle-direction)
  "Cycle move to workspace with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-move-to-workspace" cycle-direction))

(defun komorebi-api-send-to-monitor (target)
  "Send to monitor with TARGET."
  (komorebi-api--execute "send-to-monitor" target))

(defun komorebi-api-cycle-send-to-monitor (cycle-direction)
  "Cycle send to monitor with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-send-to-monitor" cycle-direction))

(defun komorebi-api-send-to-workspace (target)
  "Send to workspace with TARGET."
  (komorebi-api--execute "send-to-workspace" target))

(defun komorebi-api-send-to-named-workspace (workspace)
  "Send to named workspace with WORKSPACE."
  (komorebi-api--execute "send-to-named-workspace" workspace))

(defun komorebi-api-cycle-send-to-workspace (cycle-direction)
  "Cycle send to workspace with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-send-to-workspace" cycle-direction))

(defun komorebi-api-send-to-monitor-workspace (target-monitor target-workspace)
  "Send to monitor workspace with TARGET-MONITOR and TARGET-WORKSPACE."
  (komorebi-api--execute "send-to-monitor-workspace" target-monitor target-workspace))

(defun komorebi-api-focus-monitor (target)
  "Focus monitor with TARGET."
  (komorebi-api--execute "focus-monitor" target))

(defun komorebi-api-focus-workspace (target)
  "Focus workspace with TARGET."
  (komorebi-api--execute "focus-workspace" target))

(defun komorebi-api-focus-monitor-workspace (target-monitor target-workspace)
  "Focus monitor workspace with TARGET-MONITOR and TARGET-WORKSPACE."
  (komorebi-api--execute "focus-monitor-workspace" target-monitor target-workspace))

(defun komorebi-api-focus-named-workspace (workspace)
  "Focus named workspace with WORKSPACE."
  (komorebi-api--execute "focus-named-workspace" workspace))

(defun komorebi-api-cycle-monitor (cycle-direction)
  "Cycle monitor with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-monitor" cycle-direction))

(defun komorebi-api-cycle-workspace (cycle-direction)
  "Cycle workspace with CYCLE-DIRECTION."
  (komorebi-api--execute "cycle-workspace" cycle-direction))

(defun komorebi-api-move-workspace-to-monitor (target)
  "Move workspace to monitor with TARGET."
  (komorebi-api--execute "move-workspace-to-monitor" target))

(defun komorebi-api-new-workspace ()
  "New workspace."
  (komorebi-api--execute "new-workspace"))

(defun komorebi-api-resize-delta (pixels)
  "Resize delta with PIXELS."
  (komorebi-api--execute "resize-delta" pixels))

(defun komorebi-api-invisible-borders (left top right bottom)
  "Invisible borders with LEFT, TOP, RIGHT and BOTTOM."
  (komorebi-api--execute "invisible-borders" left top right bottom))

(defun komorebi-api-global-work-area-offset (left top right bottom)
  "Global work area offset with LEFT, TOP, RIGHT and BOTTOM."
  (komorebi-api--execute "global-work-area-offset" left top right bottom))

(defun komorebi-api-monitor-work-area-offset (monitor left top right bottom)
  "Monitor work area offset with MONITOR, LEFT, TOP, RIGHT and BOTTOM."
  (komorebi-api--execute "monitor-work-area-offset" monitor left top right bottom))

(defun komorebi-api-adjust-container-padding (sizing adjustment)
  "Adjust container padding with SIZING and ADJUSTMENT."
  (komorebi-api--execute "adjust-container-padding" sizing adjustment))

(defun komorebi-api-adjust-workspace-padding (sizing adjustment)
  "Adjust workspace padding with SIZING and ADJUSTMENT."
  (komorebi-api--execute "adjust-workspace-padding" sizing adjustment))

(defun komorebi-api-change-layout (default-layout)
  "Change layout with DEFAULT-LAYOUT."
  (komorebi-api--execute "change-layout" default-layout))

(defun komorebi-api-cycle-layout (operation-direction)
  "Cycle layout with OPERATION-DIRECTION."
  (komorebi-api--execute "cycle-layout" operation-direction))

(defun komorebi-api-load-custom-layout (path)
  "Load custom layout with PATH."
  (komorebi-api--execute "load-custom-layout" path))

(defun komorebi-api-flip-layout (axis)
  "Flip layout with AXIS."
  (komorebi-api--execute "flip-layout" axis))

(defun komorebi-api-promote ()
  "Promote."
  (komorebi-api--execute "promote"))

(defun komorebi-api-promote-focus ()
  "Promote focus."
  (komorebi-api--execute "promote-focus"))

(defun komorebi-api-retile ()
  "Retile."
  (komorebi-api--execute "retile"))

(defun komorebi-api-monitor-index-preference (index-preference left top right bottom)
  "Monitor index preference with INDEX-PREFERENCE, LEFT, TOP, RIGHT and BOTTOM."
  (komorebi-api--execute "monitor-index-preference" index-preference left top right bottom))

(defun komorebi-api-ensure-workspaces (monitor workspace-count)
  "Ensure workspaces with MONITOR and WORKSPACE-COUNT."
  (komorebi-api--execute "ensure-workspaces" monitor workspace-count))

(defun komorebi-api-ensure-named-workspaces (monitor names)
  "Ensure named workspaces with MONITOR and NAMES."
  (komorebi-api--execute "ensure-named-workspaces" monitor names))

(defun komorebi-api-container-padding (monitor workspace size)
  "Container padding with MONITOR, WORKSPACE and SIZE."
  (komorebi-api--execute "container-padding" monitor workspace size))

(defun komorebi-api-named-workspace-container-padding (workspace size)
  "Named workspace container padding with WORKSPACE and SIZE."
  (komorebi-api--execute "named-workspace-container-padding" workspace size))

(defun komorebi-api-workspace-padding (monitor workspace size)
  "Workspace padding with MONITOR, WORKSPACE and SIZE."
  (komorebi-api--execute "workspace-padding" monitor workspace size))

(defun komorebi-api-named-workspace-padding (workspace size)
  "Named workspace padding with WORKSPACE and SIZE."
  (komorebi-api--execute "named-workspace-padding" workspace size))

(defun komorebi-api-workspace-layout (monitor workspace value)
  "Workspace layout with MONITOR, WORKSPACE and VALUE."
  (komorebi-api--execute "workspace-layout" monitor workspace value))

(defun komorebi-api-named-workspace-layout (workspace value)
  "Named workspace layout with WORKSPACE and VALUE."
  (komorebi-api--execute "named-workspace-layout" workspace value))

(defun komorebi-api-workspace-custom-layout (monitor workspace path)
  "Workspace custom layout with MONITOR, WORKSPACE and PATH."
  (komorebi-api--execute "workspace-custom-layout" monitor workspace path))

(defun komorebi-api-named-workspace-custom-layout (workspace path)
  "Named workspace custom layout with WORKSPACE and PATH."
  (komorebi-api--execute "named-workspace-custom-layout" workspace path))

(defun komorebi-api-workspace-layout-rule (monitor workspace at-container-count layout)
  "Workspace layout rule with MONITOR, WORKSPACE, AT-CONTAINER-COUNT and LAYOUT."
  (komorebi-api--execute "workspace-layout-rule" monitor workspace at-container-count layout))

(defun komorebi-api-named-workspace-layout-rule (workspace at-container-count layout)
  "Named workspace layout rule with WORKSPACE, AT-CONTAINER-COUNT and LAYOUT."
  (komorebi-api--execute "named-workspace-layout-rule" workspace at-container-count layout))

(defun komorebi-api-workspace-custom-layout-rule (monitor workspace at-container-count path)
  "Workspace custom layout rule with MONITOR, WORKSPACE, AT-CONTAINER-COUNT and PATH."
  (komorebi-api--execute "workspace-custom-layout-rule" monitor workspace at-container-count path))

(defun komorebi-api-named-workspace-custom-layout-rule (workspace at-container-count path)
  "Named workspace custom layout rule with WORKSPACE, AT-CONTAINER-COUNT and PATH."
  (komorebi-api--execute "named-workspace-custom-layout-rule" workspace at-container-count path))

(defun komorebi-api-clear-workspace-layout-rules (monitor workspace)
  "Clear workspace layout rules with MONITOR and WORKSPACE."
  (komorebi-api--execute "clear-workspace-layout-rules" monitor workspace))

(defun komorebi-api-clear-named-workspace-layout-rules (workspace)
  "Clear named workspace layout rules with WORKSPACE."
  (komorebi-api--execute "clear-named-workspace-layout-rules" workspace))

(defun komorebi-api-workspace-tiling (monitor workspace value)
  "Workspace tiling with MONITOR, WORKSPACE and VALUE."
  (komorebi-api--execute "workspace-tiling" monitor workspace value))

(defun komorebi-api-named-workspace-tiling (workspace value)
  "Named workspace tiling with WORKSPACE and VALUE."
  (komorebi-api--execute "named-workspace-tiling" workspace value))

(defun komorebi-api-workspace-name (monitor workspace value)
  "Workspace name with MONITOR, WORKSPACE and VALUE."
  (komorebi-api--execute "workspace-name" monitor workspace value))

(defun komorebi-api-toggle-window-container-behaviour ()
  "Toggle window container behaviour."
  (komorebi-api--execute "toggle-window-container-behaviour"))

(defun komorebi-api-toggle-pause ()
  "Toggle pause."
  (komorebi-api--execute "toggle-pause"))

(defun komorebi-api-toggle-tiling ()
  "Toggle tiling."
  (komorebi-api--execute "toggle-tiling"))

(defun komorebi-api-toggle-float ()
  "Toggle float."
  (komorebi-api--execute "toggle-float"))

(defun komorebi-api-toggle-monocle ()
  "Toggle monocle."
  (komorebi-api--execute "toggle-monocle"))

(defun komorebi-api-toggle-maximize ()
  "Toggle maximize."
  (komorebi-api--execute "toggle-maximize"))

(defun komorebi-api-toggle-transparency ()
  "Toggle transparency."
  (komorebi-api--execute "toggle-transparency"))

(defun komorebi-api-transparency (boolean-state)
  "Set Transparency state to BOOLEAN-STATE."
  (komorebi-api--execute "transparency" boolean-state))

(defun komorebi-api-transparency-alpha (alpha)
  "Set Transparency alpha to ALPHA."
  (komorebi-api--execute "transparency-alpha" alpha))

(defun komorebi-api-restore-windows ()
  "Restore windows."
  (komorebi-api--execute "restore-windows"))

(defun komorebi-api-manage ()
  "Manage."
  (komorebi-api--execute "manage"))

(defun komorebi-api-unmanage ()
  "Unmanage."
  (komorebi-api--execute "unmanage"))

(defun komorebi-api-reload-configuration ()
  "Reload configuration."
  (komorebi-api--execute "reload-configuration"))

(defun komorebi-api-replace-configuration (config)
  "Replace configuration with CONFIG."
  (komorebi-api--execute "replace-configuration" config))

(defun komorebi-api-watch-configuration (boolean-state)
  "Watch configuration with BOOLEAN-STATE."
  (komorebi-api--execute "watch-configuration" boolean-state))

(defun komorebi-api-complete-configuration ()
  "Complete configuration."
  (komorebi-api--execute "complete-configuration"))

(defun komorebi-api-alt-focus-hack (boolean-state)
  "Alt focus hack with BOOLEAN-STATE."
  (komorebi-api--execute "alt-focus-hack" boolean-state))

(defun komorebi-api-window-hiding-behaviour (hiding-behaviour)
  "Window hiding behaviour with HIDING-BEHAVIOUR."
  (komorebi-api--execute "window-hiding-behaviour" hiding-behaviour))

(defun komorebi-api-cross-monitor-move-behaviour (move-behaviour)
  "Cross monitor move behaviour with MOVE-BEHAVIOUR."
  (komorebi-api--execute "cross-monitor-move-behaviour" move-behaviour))

(defun komorebi-api-toggle-cross-monitor-move-behaviour ()
  "Toggle cross monitor move behaviour."
  (komorebi-api--execute "toggle-cross-monitor-move-behaviour"))

(defun komorebi-api-unmanaged-window-operation-behaviour (operation-behaviour)
  "Unmanaged window operation behaviour with OPERATION-BEHAVIOUR."
  (komorebi-api--execute "unmanaged-window-operation-behaviour" operation-behaviour))

(defun komorebi-api-float-rule (identifier id)
  "Float rule with IDENTIFIER and ID."
  (komorebi-api--execute "float-rule" identifier id))

(defun komorebi-api-manage-rule (identifier id)
  "Manage rule with IDENTIFIER and ID."
  (komorebi-api--execute "manage-rule" identifier id))

(defun komorebi-api-workspace-rule (identifier id monitor workspace)
  "Workspace rule with IDENTIFIER, ID, MONITOR and WORKSPACE."
  (komorebi-api--execute "workspace-rule" identifier id monitor workspace))

(defun komorebi-api-named-workspace-rule (identifier id workspace)
  "Named workspace rule with IDENTIFIER, ID and WORKSPACE."
  (komorebi-api--execute "named-workspace-rule" identifier id workspace))

(defun komorebi-api-identify-object-name-change-application (identifier id)
  "Identify object name change application with IDENTIFIER and ID."
  (komorebi-api--execute "identify-object-name-change-application" identifier id))

(defun komorebi-api-identify-tray-application (identifier id)
  "Identify tray application with IDENTIFIER and ID."
  (komorebi-api--execute "identify-tray-application" identifier id))

(defun komorebi-api-identify-layered-application (identifier id)
  "Identify layered application with IDENTIFIER and ID."
  (komorebi-api--execute "identify-layered-application" identifier id))

(defun komorebi-api-identify-border-overflow-application (identifier id)
  "Identify border overflow application with IDENTIFIER and ID."
  (komorebi-api--execute "identify-border-overflow-application" identifier id))

(defun komorebi-api-active-window-border (boolean-state)
  "Active window border with BOOLEAN-STATE."
  (komorebi-api--execute "active-window-border" boolean-state))

(defun komorebi-api-active-window-border-colour (red green blue window-kind)
  "Active window border colour with RED, GREEN, BLUE and WINDOW-KIND."
  (komorebi-api--execute "active-window-border-colour" red green blue window-kind))

(defun komorebi-api-active-window-border-width (width)
  "Active window border width with WIDTH."
  (komorebi-api--execute "active-window-border-width" width))

(defun komorebi-api-active-window-border-offset (offset)
  "Active window border offset with OFFSET."
  (komorebi-api--execute "active-window-border-offset" offset))

(defun komorebi-api-focus-follows-mouse (boolean-state implementation)
  "Focus follows mouse with BOOLEAN-STATE and IMPLEMENTATION."
  (komorebi-api--execute "focus-follows-mouse" boolean-state implementation))

(defun komorebi-api-toggle-focus-follows-mouse (implementation)
  "Toggle focus follows mouse with IMPLEMENTATION."
  (komorebi-api--execute "toggle-focus-follows-mouse" implementation))

(defun komorebi-api-mouse-follows-focus (boolean-state)
  "Mouse follows focus with BOOLEAN-STATE."
  (komorebi-api--execute "mouse-follows-focus" boolean-state))

(defun komorebi-api-toggle-mouse-follows-focus ()
  "Toggle mouse follows focus."
  (komorebi-api--execute "toggle-mouse-follows-focus"))

(defun komorebi-api-ahk-library ()
  "Ahk library."
  (komorebi-api--execute "ahk-library"))

(defun komorebi-api-ahk-app-specific-configuration (path override-path)
  "Ahk app specific configuration with PATH and OVERRIDE-PATH."
  (komorebi-api--execute "ahk-app-specific-configuration" path override-path))

(defun komorebi-api-pwsh-app-specific-configuration (path override-path)
  "Pwsh app specific configuration with PATH and OVERRIDE-PATH."
  (komorebi-api--execute "pwsh-app-specific-configuration" path override-path))

(defun komorebi-api-format-app-specific-configuration (path)
  "Format app specific configuration with PATH."
  (komorebi-api--execute "format-app-specific-configuration" path))

(defun komorebi-api-notification-schema ()
  "Notification schema."
  (komorebi-api--execute "notification-schema"))

(defun komorebi-api-socket-schema ()
  "Socket schema."
  (komorebi-api--execute "socket-schema"))


(provide 'komorebi-api)
;;; komorebi-api.el ends here

;; Local Variables:
;; jinx-local-words: "behaviour"
;; End:
