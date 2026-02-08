;;; emacs-ai-agent-bridge.el --- Bridge between Emacs and AI agents in tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1") (popup "0.5.3"))
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
;; It monitors tmux console output every 2 seconds and detects
;; when Claude Code is at an input prompt, then copies the content to an
;; Emacs buffer.

;;; Code:

(require 'cl-lib)
(require 'popup)

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

(defcustom emacs-ai-agent-bridge-monitor-interval 2
  "Interval in seconds between tmux console checks."
  :type 'integer
  :group 'emacs-ai-agent-bridge)

(defcustom emacs-ai-agent-bridge-scrollback-lines 3000
  "Number of scrollback lines to capture from tmux history.
A value of 0 captures only visible content.
Negative values capture from that many lines back in the scrollback buffer."
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

(defvar emacs-ai-agent-bridge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" 'emacs-ai-select-option-1)
    (define-key map "2" 'emacs-ai-select-option-2)
    (define-key map "3" 'emacs-ai-select-option-3)
    (define-key map "4" 'emacs-ai-select-option-4)
    (define-key map "5" 'emacs-ai-select-option-5)
    (define-key map (kbd "C-m") 'emacs-ai-agent-bridge-smart-return)
    map)
  "Keymap for *ai* buffer.")

(defun emacs-ai-agent-bridge-get-first-tmux-session ()
  "Get the name of the first available tmux session.
Sorts session names using natural sort order (0, 1, 2, ..., then alphabetically)."
  (let ((output (shell-command-to-string "tmux list-sessions -F '#{session_name}' 2>/dev/null | sort -V | head -1")))
    (if (string-empty-p output)
        nil
      (string-trim output))))

(defun emacs-ai-agent-bridge-get-all-tmux-sessions ()
  "Get a list of all available tmux sessions."
  (let ((output (shell-command-to-string "tmux list-sessions -F '#{session_name}' 2>/dev/null")))
    (if (string-empty-p output)
        nil
      (split-string output "\n" t))))

(defun emacs-ai-agent-bridge-select-session ()
  "Select a tmux session from available sessions and switch to it."
  (interactive)
  (let* ((sessions (emacs-ai-agent-bridge-get-all-tmux-sessions))
         (current-session (or emacs-ai-agent-bridge-tmux-session
                             (emacs-ai-agent-bridge-get-first-tmux-session)))
         (selected (completing-read
                    (format "Select tmux session (current: %s): " current-session)
                    sessions
                    nil t)))
    (when selected
      (setq emacs-ai-agent-bridge-tmux-session selected)
      ;; モニタリング中であれば再起動
      (when emacs-ai-agent-bridge--monitor-timer
        (emacs-ai-agent-bridge-stop-monitoring)
        (emacs-ai-agent-bridge-start-monitoring))
      (message "Switched to tmux session: %s" selected))))

(defun emacs-ai-agent-bridge-popup-select-session ()
  "Select a tmux session using popup menu and switch to it."
  (interactive)
  (let* ((sessions (emacs-ai-agent-bridge-get-all-tmux-sessions))
         (current-session (or emacs-ai-agent-bridge-tmux-session
                             (emacs-ai-agent-bridge-get-first-tmux-session))))
    (when sessions
      (let ((selected (popup-menu* sessions)))
        (when selected
          (setq emacs-ai-agent-bridge-tmux-session selected)
          ;; モニタリング中であれば再起動
          (when emacs-ai-agent-bridge--monitor-timer
            (emacs-ai-agent-bridge-stop-monitoring)
            (emacs-ai-agent-bridge-start-monitoring))
          (message "Switched to tmux session: %s" selected))))))

(defun emacs-ai-agent-bridge-send-to-tmux (session text)
  "Send TEXT to tmux SESSION line by line with 0.1 second delay.
This is a helper function to avoid code duplication."
  (let ((lines (split-string text "\n" t)))
    (dolist (line lines)
      (shell-command
       (format "tmux send-keys -t %s %s"
               (shell-quote-argument session)
               (shell-quote-argument line)))
      (sit-for 0.1))))

(defun emacs-ai-agent-bridge-send-key-to-tmux (session key)
  "Send KEY to tmux SESSION.
Common keys: C-m (Enter), Up, Down, etc."
  (shell-command
   (format "tmux send-keys -t %s %s"
           (shell-quote-argument session)
           key)))

(defun emacs-ai-agent-bridge-get-git-root ()
  "Get the git repository root directory.
Returns nil if not in a git repository."
  (let ((default-directory (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             default-directory)))
    (let ((git-root (shell-command-to-string "git rev-parse --show-toplevel 2>/dev/null")))
      (when (and git-root (not (string-empty-p git-root)))
        (string-trim git-root)))))

(defun emacs-ai-agent-bridge-get-display-path ()
  "Get the appropriate path to display for the current buffer.
Returns git-relative path if in a git repository, buffer name if no file, or absolute path otherwise."
  (if (buffer-file-name)
      (let* ((file-path (buffer-file-name))
             (file-dir (file-name-directory file-path)))
        ;; Check if we're in a git repository by running git status
        (if (= 0 (let ((default-directory file-dir))
                   (call-process "git" nil nil nil "status" "--porcelain")))
            ;; In git repo - calculate relative path from git root
            (let ((git-prefix (let ((default-directory file-dir))
                                (string-trim (shell-command-to-string "git rev-parse --show-prefix 2>/dev/null")))))
              (concat git-prefix (file-name-nondirectory file-path)))
          ;; Not in git repo - return just filename
          (file-name-nondirectory file-path)))
    ;; Return buffer name if no file associated
    (buffer-name)))

(defun emacs-ai-agent-bridge-send-region-to-tmux (start end)
  "Send the region between START and END to the first available tmux session."
  (interactive "r")
  (let* ((session (or emacs-ai-agent-bridge-tmux-session
                      (emacs-ai-agent-bridge-get-first-tmux-session)))
         (text (buffer-substring-no-properties start end))
         ;; Get display path and line number
         (display-path (emacs-ai-agent-bridge-get-display-path))
         (line-number (line-number-at-pos start))
         ;; Create annotation in English
         (annotation (format "This is from %s at line %d.\n\n"
                           display-path
                           line-number))
         ;; Prepend annotation to text
         (annotated-text (concat annotation text)))
    (if session
        (progn
          ;; Send annotated text first, then send Enter key separately
          (emacs-ai-agent-bridge-send-to-tmux session annotated-text)
          (emacs-ai-agent-bridge-send-key-to-tmux session "C-m")
          (message "Sent region to tmux session: %s" session))
      (message "No tmux sessions found"))))

(defun send-to-ai (start end)
  "Send the region between START and END to AI agent in tmux.
This is an alias for `emacs-ai-agent-bridge-send-region-to-tmux'."
  (interactive "r")
  (emacs-ai-agent-bridge-send-region-to-tmux start end)
  ;; Save buffer if it has unsaved changes to ensure AI agent sees latest content
  (when (and (buffer-modified-p)
             (buffer-file-name))
    (save-buffer)))

(defun emacs-ai-agent-bridge-get-consecutive-lines-block ()
  "Get consecutive lines before cursor until blank line or beginning of buffer.
Returns (START . END) of the block."
  (save-excursion
    (let ((end-pos (line-end-position))
          (start-pos nil))
      ;; Move to beginning of current line
      (beginning-of-line)
      ;; Keep moving backward while lines are non-empty
      (while (and (not (bobp))
                  (not (looking-at "^[[:space:]]*$")))
        (forward-line -1))
      ;; If we stopped at a blank line, move forward one line
      (when (and (not (bobp))
                 (looking-at "^[[:space:]]*$"))
        (forward-line 1))
      (setq start-pos (point))
      (cons start-pos end-pos))))

(defun emacs-ai-agent-bridge-send-block-to-ai ()
  "Send consecutive lines before cursor to AI agent in tmux.
Stops at blank line or beginning of buffer."
  (interactive)
  (let ((block-bounds (emacs-ai-agent-bridge-get-consecutive-lines-block)))
    (when block-bounds
      (let ((start (car block-bounds))
            (end (cdr block-bounds)))
        (emacs-ai-agent-bridge-send-region-to-tmux start end)
        ;; Save buffer if it has unsaved changes
        (when (and (buffer-modified-p)
                   (buffer-file-name))
          (save-buffer))))))

(defun emacs-ai-agent-bridge-select-option (option-number)
  "Select an option from AI agent's choice prompt.
OPTION-NUMBER should be 1, 2, 3, 4, or 5.
Sends the number key directly to select the option (Claude Code 2.0.31+)."
  (let ((session (or emacs-ai-agent-bridge-tmux-session
                     (emacs-ai-agent-bridge-get-first-tmux-session))))
    (if session
        (progn
          ;; Send the number key directly (Claude Code 2.0.31+ supports direct number selection)
          (emacs-ai-agent-bridge-send-key-to-tmux session (number-to-string option-number))
          (message "Selected option %d" option-number))
      (message "No tmux sessions found"))))

(defun emacs-ai-select-option-1 ()
  "Select option 1 from AI agent's choice prompt."
  (interactive)
  (emacs-ai-agent-bridge-select-option 1))

(defun emacs-ai-select-option-2 ()
  "Select option 2 from AI agent's choice prompt."
  (interactive)
  (emacs-ai-agent-bridge-select-option 2))

(defun emacs-ai-select-option-3 ()
  "Select option 3 from AI agent's choice prompt."
  (interactive)
  (emacs-ai-agent-bridge-select-option 3))

(defun emacs-ai-select-option-4 ()
  "Select option 4 from AI agent's choice prompt."
  (interactive)
  (emacs-ai-agent-bridge-select-option 4))

(defun emacs-ai-select-option-5 ()
  "Select option 5 from AI agent's choice prompt."
  (interactive)
  (emacs-ai-agent-bridge-select-option 5))

(defun emacs-ai-agent-bridge-smart-return ()
  "Smart return key behavior for *ai* buffer.
If the buffer contains a choice prompt, select option 1.
If it's a text input prompt, send Enter to tmux.
Otherwise, do nothing."
  (interactive)
  (let ((content (buffer-string))
        (session (or emacs-ai-agent-bridge-tmux-session
                     (emacs-ai-agent-bridge-get-first-tmux-session))))
    (cond
     ;; Choice prompt - select option 1
     ((emacs-ai-agent-bridge-is-choice-prompt-p content)
      (emacs-ai-select-option-1))
     ;; Text input prompt or any other prompt - send Enter
     ((or (emacs-ai-agent-bridge-is-text-input-prompt-p content)
          ;; Always allow Enter if we're at a prompt (content unchanged)
          emacs-ai-agent-bridge--prompt-detected)
      (when session
        (emacs-ai-agent-bridge-send-key-to-tmux session "C-m")
        (message "Sent Enter to tmux session: %s" session)))
     ;; No prompt detected
     (t
      (message "No prompt detected in *ai* buffer")))))

(defun emacs-ai-agent-bridge-capture-tmux-pane ()
  "Capture the current content of the configured tmux pane.
Includes scrollback history based on `emacs-ai-agent-bridge-scrollback-lines'."
  (let* ((session (or emacs-ai-agent-bridge-tmux-session
                      (emacs-ai-agent-bridge-get-first-tmux-session)))
         (scrollback-option (if (> emacs-ai-agent-bridge-scrollback-lines 0)
                                (format " -S -%d" emacs-ai-agent-bridge-scrollback-lines)
                              ""))
         (cmd (format "tmux capture-pane -t %s:%s -p%s"
                      session
                      emacs-ai-agent-bridge-tmux-pane
                      scrollback-option)))
    (if session
        (shell-command-to-string cmd)
      (error "No tmux session available"))))

(defun emacs-ai-agent-bridge-content-unchanged-p (content)
  "Check if CONTENT is unchanged from the last capture."
  (and emacs-ai-agent-bridge--last-capture
       content
       (string= emacs-ai-agent-bridge--last-capture content)))

(defun emacs-ai-agent-bridge-adjust-box-lines (content window-width)
  "Adjust horizontal box drawing lines in CONTENT to fit within WINDOW-WIDTH."
  (let ((max-width (- window-width 4))) ; Leave some margin
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; First, replace ─ with - for consistent width
      (while (search-forward "─" nil t)
        (replace-match "-"))
      (goto-char (point-min))
      ;; Find if we need to adjust box width
      (let ((needs-adjustment nil)
            (new-line-length nil))
        (save-excursion
          (while (and (not needs-adjustment)
                      (re-search-forward "^\\([╭╰┌└├]\\)\\(-+\\)\\([╮╯┐┘┤]\\)$" nil t))
            (let* ((line-length (length (match-string 2)))
                   (total-length (+ 2 line-length)))
              (when (> total-length max-width)
                (setq needs-adjustment t)
                (setq new-line-length (- max-width 2))))))
        ;; If adjustment is needed, adjust both horizontal and vertical lines
        (when needs-adjustment
          (goto-char (point-min))
          ;; Adjust horizontal lines
          (while (re-search-forward "^\\([╭╰┌└├]\\)\\(-+\\)\\([╮╯┐┘┤]\\)$" nil t)
            (let* ((start-char (match-string 1))
                   (end-char (match-string 3))
                   (new-line (make-string new-line-length ?-)))
              (replace-match (concat start-char new-line end-char) t t)))
          ;; Adjust vertical lines with content
          (goto-char (point-min))
          (while (re-search-forward "^│\\(.+\\)│$" nil t)
            (let* ((content-str (match-string 1))
                   (content-length (length content-str))
                   (padding-needed (- new-line-length content-length))
                   (new-content (if (> padding-needed 0)
                                    (concat content-str (make-string padding-needed ?\s))
                                  ;; Truncate content if too long
                                  (substring content-str 0 new-line-length))))
              (replace-match (concat "│" new-content "│") t t)))))
      (buffer-string))))

(defun emacs-ai-agent-bridge-is-choice-prompt-p (content)
  "Check if CONTENT contains a choice prompt with numbered options."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    ;; Look for lines that start with number followed by a dot
    ;; More flexible pattern that works with or without box drawing characters
    (or (re-search-forward "[│|][[:space:]]*[❯]?[[:space:]]*[1-9]\\." nil t)
        (re-search-forward "^[[:space:]]*[❯]?[[:space:]]*[1-9]\\." nil t)
        ;; Also check for simple "1." pattern anywhere in the line
        (re-search-forward "[[:space:]][1-9]\\.[[:space:]]" nil t))))

(defun emacs-ai-agent-bridge-is-text-input-prompt-p (content)
  "Check if CONTENT is a simple text input prompt (contains only '>')."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    ;; Look for a line containing only '>' (with possible spaces)
    (and (re-search-forward "^[│|][[:space:]]*>[[:space:]]*[│|]$" nil t)
         ;; Make sure there are no numbered options
         (not (emacs-ai-agent-bridge-is-choice-prompt-p content)))))

(defun emacs-ai-agent-bridge-display-ai-buffer (buffer)
  "Display the AI BUFFER, ensuring only two windows are shown."
  ;; Only modify window configuration if buffer is not already visible
  (unless (get-buffer-window buffer)
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buffer)
    (other-window 1)))

(defun emacs-ai-agent-bridge-colorize-options (content)
  "Add color to option numbers (1., 2., 3.) in CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    ;; Find and colorize option numbers
    (while (re-search-forward "\\([1-5]\\.\\)" nil t)
      (let ((start (match-beginning 1))
            (end (match-end 1)))
        (put-text-property start end 'face 'font-lock-keyword-face)))
    (buffer-string)))

(defun emacs-ai-agent-bridge-update-ai-buffer (content)
  "Update the *ai* buffer with CONTENT and display it without switching focus."
  (let* ((buffer (get-buffer-create emacs-ai-agent-bridge--ai-buffer-name))
         (window (get-buffer-window buffer))
         ;; Get window width for adjusting box lines
         (window-width (if window
                          (window-width window)
                        80)) ; Default width if no window yet
         ;; Adjust box drawing lines first
         (adjusted-content (emacs-ai-agent-bridge-adjust-box-lines content window-width))
         ;; Then trim trailing empty lines
         (trimmed-content (replace-regexp-in-string "\\(\n\\s-*\\)+\\'" "" adjusted-content))
         ;; Finally colorize options if it's a choice prompt
         (final-content (if (emacs-ai-agent-bridge-is-choice-prompt-p trimmed-content)
                           (emacs-ai-agent-bridge-colorize-options trimmed-content)
                         trimmed-content)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert final-content)
        (goto-char (point-max)))
      ;; Apply the keymap BEFORE making buffer read-only
      (use-local-map emacs-ai-agent-bridge-mode-map)
      ;; Make buffer read-only
      (setq buffer-read-only t)
      ;; Force override C-m binding after setting read-only
      (local-set-key (kbd "C-m") 'emacs-ai-agent-bridge-smart-return)
      ;; Ensure buffer has no file association (like *scratch*)
      (setq buffer-file-name nil)
      ;; Add session display to mode-line via mode-line-misc-info
      (setq-local mode-line-misc-info
                  (list emacs-ai-agent-bridge-ai-buffer-mode-line-format)))
    ;; Only display buffer if it's not already visible
    (unless window
      (emacs-ai-agent-bridge-display-ai-buffer buffer))
    ;; If window exists, scroll to bottom
    (when window
      (with-selected-window window
        (goto-char (point-max))
        (recenter -1)))))

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
  ;; Fix session if not already set
  (unless emacs-ai-agent-bridge-tmux-session
    (setq emacs-ai-agent-bridge-tmux-session
          (emacs-ai-agent-bridge-get-first-tmux-session)))
  (setq emacs-ai-agent-bridge--monitor-timer
        (run-with-timer 0 emacs-ai-agent-bridge-monitor-interval
                        #'emacs-ai-agent-bridge-monitor-tmux))
  (message "Started monitoring tmux session %s, pane %s"
           emacs-ai-agent-bridge-tmux-session
           emacs-ai-agent-bridge-tmux-pane))

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

(defun emacs-ai-agent-bridge-debug-buffer ()
  "Debug information about the *ai* buffer."
  (interactive)
  (let ((buffer (get-buffer emacs-ai-agent-bridge--ai-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (message "Buffer: %s, Read-only: %s, Major mode: %s, Keymap: %s, C-m binding: %s, Choice prompt: %s, Text prompt: %s, Prompt detected: %s"
                   (buffer-name)
                   buffer-read-only
                   major-mode
                   (if (eq (current-local-map) emacs-ai-agent-bridge-mode-map) "correct" "incorrect")
                   (key-binding (kbd "C-m"))
                   (emacs-ai-agent-bridge-is-choice-prompt-p (buffer-string))
                   (emacs-ai-agent-bridge-is-text-input-prompt-p (buffer-string))
                   emacs-ai-agent-bridge--prompt-detected))
      (message "Buffer %s not found" emacs-ai-agent-bridge--ai-buffer-name))))

(defun emacs-ai-agent-bridge-find-ai-block ()
  "Find @ai-begin/@ai-end block boundaries.
Returns (BEGIN-POS . END-POS) or nil if not in a block."
  (save-excursion
    (let ((end-pos nil)
          (begin-pos nil)
          (current-line-start (line-beginning-position)))
      ;; Check if we're on @ai-end line
      (beginning-of-line)
      (when (looking-at "^@ai-end\\s-*$")
        ;; Look backwards for @ai-begin
        (save-excursion
          (if (re-search-backward "^@ai-begin\\s-*$" nil t)
              (setq begin-pos (line-beginning-position))
            (error "No matching @ai-begin found")))
        (setq end-pos (min (1+ (line-end-position)) (point-max)))
        (cons begin-pos end-pos)))))

(defun emacs-ai-agent-bridge-animate-line-deletion (start end)
  "Animate deletion of text between START and END over 1 second."
  (let* ((original-text (buffer-substring-no-properties start end))
         (animation-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
         (num-frames (length animation-frames))
         (delay 0.1))
    ;; Wrap animation in with-silent-modifications to prevent marking buffer as modified
    (with-silent-modifications
      (save-excursion
        ;; Replace original text with spinner animation
        (dotimes (i num-frames)
          (goto-char start)
          (delete-region start end)
          (insert (format "%s Sending to AI..." (nth i animation-frames)))
          (setq end (point))
          (redisplay)
          (sit-for delay))))
    ;; Complete deletion - this marks buffer as modified properly
    (delete-region start end)))

(defun emacs-ai-agent-bridge-process-ai-line ()
  "Process current line if it starts with @ai.
Send the text after @ai to tmux and delete the line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Single line @ai
     ((looking-at "^@ai\\s-+\\(.+\\)$")
      (let* ((prompt (match-string 1))
             (session (or emacs-ai-agent-bridge-tmux-session
                          (emacs-ai-agent-bridge-get-first-tmux-session)))
             (line-start (line-beginning-position))
             (line-end (min (1+ (line-end-position)) (point-max)))
             ;; Get display path and current line number
             (display-path (emacs-ai-agent-bridge-get-display-path))
             (current-line (line-number-at-pos))
             ;; Create annotation
             (annotation (format "This is from %s at line %d.\n\n"
                               display-path
                               current-line))
             ;; Prepend annotation to prompt
             (annotated-prompt (concat annotation prompt)))
        (if session
            (progn
              ;; Animate line deletion over 1 second
              (emacs-ai-agent-bridge-animate-line-deletion line-start line-end)
              ;; Send the annotated prompt text
              (emacs-ai-agent-bridge-send-to-tmux session annotated-prompt)
              ;; Send Enter key
              (emacs-ai-agent-bridge-send-key-to-tmux session "C-m")
              (message "✓ Sent to AI: %s" prompt)
              t)  ; Return t to indicate we processed the line
          (message "No tmux sessions found")
          nil)))
     ;; @ai-end (multi-line block)
     ((looking-at "^@ai-end\\s-*$")
      (let ((block-bounds (emacs-ai-agent-bridge-find-ai-block)))
        (if block-bounds
            (let* ((begin-pos (car block-bounds))
                   (end-pos (cdr block-bounds))
                   (session (or emacs-ai-agent-bridge-tmux-session
                                (emacs-ai-agent-bridge-get-first-tmux-session))))
              (if session
                  (save-excursion
                    (goto-char begin-pos)
                    ;; Skip @ai-begin line
                    (forward-line 1)
                    ;; Collect all lines into a single string
                    (let ((lines '()))
                      (while (< (point) end-pos)
                        (beginning-of-line)
                        (unless (looking-at "^@ai-end\\s-*$")
                          (let ((line (buffer-substring-no-properties 
                                       (line-beginning-position) 
                                       (line-end-position))))
                            (push line lines)))
                        (forward-line 1))
                      ;; Join lines with newlines and send as one command
                      (let* ((full-text (mapconcat 'identity (nreverse lines) "\n"))
                             ;; Get display path and line number of @ai-begin
                             (display-path (emacs-ai-agent-bridge-get-display-path))
                             (begin-line (save-excursion
                                          (goto-char begin-pos)
                                          (line-number-at-pos)))
                             ;; Create annotation
                             (annotation (format "This is from %s at line %d.\n\n"
                                               display-path
                                               begin-line))
                             ;; Prepend annotation to text
                             (annotated-text (concat annotation full-text)))
                        ;; Send the annotated text FIRST, then do animation
                        (emacs-ai-agent-bridge-send-to-tmux session annotated-text)
                        (emacs-ai-agent-bridge-send-key-to-tmux session "C-m")
                        ;; Animate the entire block deletion
                        (emacs-ai-agent-bridge-animate-line-deletion begin-pos end-pos)
                        (message "✓ Sent multi-line prompt to AI")))
                    t)
                (message "No tmux sessions found")
                nil))
          (message "Not inside an @ai-begin/@ai-end block")
          nil)))
     ;; Not an @ai line
     (t nil))))

(defvar emacs-ai-agent-bridge-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'emacs-ai-agent-bridge-smart-input-return)
    map)
  "Keymap for emacs-ai-agent-bridge-input-mode.")

(defvar emacs-ai-agent-bridge-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'emacs-ai-agent-bridge-popup-select-session)
    map)
  "Keymap for mode-line session display.")

(defvar emacs-ai-agent-bridge-mode-line-format
  '(:eval
    (when (and emacs-ai-agent-bridge-input-mode
               emacs-ai-agent-bridge-tmux-session)
      (propertize (format "[tmux:%s]" emacs-ai-agent-bridge-tmux-session)
                  'face 'font-lock-constant-face
                  'mouse-face 'mode-line-highlight
                  'help-echo "Click to switch tmux session"
                  'keymap emacs-ai-agent-bridge-mode-line-map)))
  "Mode-line format for tmux session display.")

(defvar emacs-ai-agent-bridge-ai-buffer-mode-line-format
  '(:eval
    (when emacs-ai-agent-bridge-tmux-session
      (propertize (format "[tmux:%s]" emacs-ai-agent-bridge-tmux-session)
                  'face 'font-lock-constant-face
                  'mouse-face 'mode-line-highlight
                  'help-echo "Click to switch tmux session"
                  'keymap emacs-ai-agent-bridge-mode-line-map)))
  "Mode-line format for tmux session display in *ai* buffer.")

(defun emacs-ai-agent-bridge-get-original-return-command ()
  "Get the original RET key command without our minor mode override.
This allows us to call the underlying mode's RET handler."
  (let ((emacs-ai-agent-bridge-input-mode nil))
    (key-binding (kbd "RET"))))

(defun emacs-ai-agent-bridge-call-original-return ()
  "Call the original RET key command from the underlying mode.
This preserves mode-specific RET behavior like markdown table alignment,
org-mode list continuation, etc."
  (let ((original-command (emacs-ai-agent-bridge-get-original-return-command)))
    (if original-command
        (call-interactively original-command)
      (newline))))

(defun emacs-ai-agent-bridge-smart-input-return ()
  "Smart return key for buffers with @ai input support.
If current line starts with @ai, process it.
Otherwise, call the original RET handler from the underlying mode."
  (interactive)
  (if (emacs-ai-agent-bridge-process-ai-line)
      ;; Line was processed, save buffer if it has unsaved changes
      (when (and (buffer-modified-p)
                 (buffer-file-name))
        (save-buffer))
    ;; Not an @ai line - call original RET handler
    (emacs-ai-agent-bridge-call-original-return)))

(define-minor-mode emacs-ai-agent-bridge-input-mode
  "Minor mode for @ai input support.
When enabled, lines starting with @ai followed by Enter will be sent to AI."
  :lighter " Agent-Bridge"
  :keymap emacs-ai-agent-bridge-input-mode-map
  (if emacs-ai-agent-bridge-input-mode
      ;; Mode enabled: add to global-mode-string
      (progn
        (unless global-mode-string
          (setq global-mode-string '("")))
        (unless (member emacs-ai-agent-bridge-mode-line-format global-mode-string)
          (setq global-mode-string
                (append global-mode-string (list emacs-ai-agent-bridge-mode-line-format)))))
    ;; Mode disabled: remove from global-mode-string
    (setq global-mode-string
          (remove emacs-ai-agent-bridge-mode-line-format global-mode-string))))

;; Global keybinding for C-c <return>
(global-set-key (kbd "C-c <return>") 'emacs-ai-agent-bridge-send-block-to-ai)

(provide 'emacs-ai-agent-bridge)
;;; emacs-ai-agent-bridge.el ends here
