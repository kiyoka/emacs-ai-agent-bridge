;;; emacs-ai-agent-bridge.el --- Bridge between Emacs and AI agents in tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, processes
;; URL: https://github.com/kiyoka/emacs-ai-agent-bridge

;; This file is not part of GNU Emacs.

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

;; This package provides a bridge between Emacs and AI coding agents running in tmux.
;; It monitors tmux console output every 5 seconds and detects
;; when Claude Code is at an input prompt, then copies the content to an
;; Emacs buffer.

;;; Code:

(require 'cl-lib)

(defgroup emacs-ai-agent-bridge nil
  "Bridge between Emacs and AI agents in tmux."
  :group 'tools
  :prefix "emacs-ai-agent-bridge-")

(defcustom emacs-ai-agent-bridge-tmux-session nil
  "Name of the tmux session to monitor.
If nil, will use the first available session."
  :type '(choice (const :tag "Auto-detect" nil)
                 string)
  :group 'emacs-ai-agent-bridge)

(defcustom emacs-ai-agent-bridge-tmux-pane "0"
  "ID of the tmux pane to monitor."
  :type 'string
  :group 'emacs-ai-agent-bridge)

(defcustom emacs-ai-agent-bridge-monitor-interval 5
  "Interval in seconds between tmux console checks."
  :type 'integer
  :group 'emacs-ai-agent-bridge)


(defvar emacs-ai-agent-bridge--monitor-timer nil
  "Timer object for periodic monitoring.")

(defvar emacs-ai-agent-bridge--ai-buffer-name "*ai*"
  "Name of the buffer to display AI agent output.")

(defvar emacs-ai-agent-bridge--last-capture nil
  "Previous tmux capture content for comparison.")

(defvar emacs-ai-agent-bridge--prompt-detected nil
  "Flag to track if prompt has already been detected and buffer shown.")

(defun emacs-ai-agent-bridge-get-first-tmux-session ()
  "Get the name of the first available tmux session."
  (let ((output (shell-command-to-string "tmux list-sessions -F '#{session_name}' 2>/dev/null")))
    (if (string-empty-p output)
        nil
      (car (split-string output "\n" t)))))

(defun emacs-ai-agent-bridge-send-region-to-tmux (start end)
  "Send the region between START and END to the first available tmux session."
  (interactive "r")
  (let ((session (emacs-ai-agent-bridge-get-first-tmux-session))
        (text (buffer-substring-no-properties start end)))
    (if session
        (progn
          ;; Send text first, then send Enter key separately
          (shell-command
           (format "tmux send-keys -t %s %s"
                   (shell-quote-argument session)
                   (shell-quote-argument text)))
          (shell-command
           (format "tmux send-keys -t %s C-m"
                   (shell-quote-argument session)))
          (message "Sent region to tmux session: %s" session))
      (message "No tmux sessions found"))))

(defun send-to-ai (start end)
  "Send the region between START and END to AI agent in tmux.
This is an alias for `emacs-ai-agent-bridge-send-region-to-tmux'."
  (interactive "r")
  (emacs-ai-agent-bridge-send-region-to-tmux start end))

(defun emacs-ai-agent-bridge-capture-tmux-pane ()
  "Capture the current content of the configured tmux pane."
  (let* ((session (or emacs-ai-agent-bridge-tmux-session
                      (emacs-ai-agent-bridge-get-first-tmux-session)))
         (cmd (format "tmux capture-pane -t %s:%s -p"
                      session
                      emacs-ai-agent-bridge-tmux-pane)))
    (if session
        (shell-command-to-string cmd)
      (error "No tmux session available"))))

(defun emacs-ai-agent-bridge-content-unchanged-p (content)
  "Check if CONTENT is unchanged from the last capture."
  (and emacs-ai-agent-bridge--last-capture
       content
       (string= emacs-ai-agent-bridge--last-capture content)))

(defun emacs-ai-agent-bridge-update-ai-buffer (content)
  "Update the *ai* buffer with CONTENT and display it without switching focus."
  (let* ((buffer (get-buffer-create emacs-ai-agent-bridge--ai-buffer-name))
         (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (goto-char (point-min)))
    ;; Only display buffer if it's not already visible
    (unless window
      (display-buffer buffer '(display-buffer-pop-up-window)))))

(defun emacs-ai-agent-bridge-monitor-tmux ()
  "Check tmux console and update buffer if content is unchanged."
  (condition-case err
      (let ((content (emacs-ai-agent-bridge-capture-tmux-pane)))
        (cond
         ;; Content unchanged and not yet detected - show buffer
         ((and (emacs-ai-agent-bridge-content-unchanged-p content)
               (not emacs-ai-agent-bridge--prompt-detected))
          (emacs-ai-agent-bridge-update-ai-buffer content)
          (setq emacs-ai-agent-bridge--prompt-detected t))
         ;; Content changed - reset detection flag
         ((not (emacs-ai-agent-bridge-content-unchanged-p content))
          (setq emacs-ai-agent-bridge--prompt-detected nil)))
        (setq emacs-ai-agent-bridge--last-capture content))
    (error
     (message "Error monitoring tmux: %s" (error-message-string err)))))

(defun emacs-ai-agent-bridge-start-monitoring ()
  "Start monitoring the tmux console."
  (interactive)
  (when emacs-ai-agent-bridge--monitor-timer
    (cancel-timer emacs-ai-agent-bridge--monitor-timer))
  (setq emacs-ai-agent-bridge--last-capture nil)  ; Reset last capture
  (setq emacs-ai-agent-bridge--prompt-detected nil)  ; Reset detection flag
  (setq emacs-ai-agent-bridge--monitor-timer
        (run-with-timer 0 emacs-ai-agent-bridge-monitor-interval 
                        #'emacs-ai-agent-bridge-monitor-tmux))
  (let ((session (or emacs-ai-agent-bridge-tmux-session
                     (emacs-ai-agent-bridge-get-first-tmux-session))))
    (message "Started monitoring tmux session %s, pane %s"
             session emacs-ai-agent-bridge-tmux-pane)))

(defun emacs-ai-agent-bridge-stop-monitoring ()
  "Stop monitoring the tmux console."
  (interactive)
  (when emacs-ai-agent-bridge--monitor-timer
    (cancel-timer emacs-ai-agent-bridge--monitor-timer)
    (setq emacs-ai-agent-bridge--monitor-timer nil))
  (message "Stopped monitoring tmux"))

(defun emacs-ai-agent-bridge-monitor-status ()
  "Display the current monitoring status."
  (interactive)
  (if emacs-ai-agent-bridge--monitor-timer
      (let ((session (or emacs-ai-agent-bridge-tmux-session
                         (emacs-ai-agent-bridge-get-first-tmux-session))))
        (message "Monitoring tmux session %s, pane %s (interval: %d seconds)"
                 session
                 emacs-ai-agent-bridge-tmux-pane
                 emacs-ai-agent-bridge-monitor-interval))
    (message "Not currently monitoring tmux")))

(provide 'emacs-ai-agent-bridge)
;;; emacs-ai-agent-bridge.el ends here