# emacs-ai-agent-bridge

An Emacs extension that bridges an AI coding agent running in tmux with Emacs.

## Features

- **Automatic tmux monitoring**: Monitor tmux session every 5 seconds
- **Smart prompt detection**: Detect AI agent prompt state by monitoring content changes
- **Non-intrusive buffer display**: Show *ai* buffer without stealing focus
- **One-time notification**: Display buffer only once per prompt detection
- **Easy text sending**: Send selected region to AI agent with automatic execution

## Installation

1. Place `emacs-ai-agent-bridge.el` in your Emacs load path
2. Add to your Emacs configuration:
   ```elisp
   (require 'emacs-ai-agent-bridge)
   ```

### Quick Start Configuration

To automatically start monitoring when Emacs starts:
```elisp
(require 'emacs-ai-agent-bridge)
(emacs-ai-agent-bridge-start-monitoring)
```

### Full Configuration Example

```elisp
;; Load the package
(require 'emacs-ai-agent-bridge)

;; Optional: Configure settings
(setq emacs-ai-agent-bridge-tmux-session nil)  ; auto-detect session
(setq emacs-ai-agent-bridge-tmux-pane "0")     ; monitor pane 0
(setq emacs-ai-agent-bridge-monitor-interval 5) ; check every 5 seconds

;; Start monitoring automatically
(emacs-ai-agent-bridge-start-monitoring)

;; Optional: Set up key binding for sending text
(global-set-key (kbd "C-c a s") 'send-to-ai)
```

## Usage

### Start Monitoring

#### Manual Start
```
M-x emacs-ai-agent-bridge-start-monitoring
```

### Send Text to AI Agent
1. Select text region
2. Execute:
   ```
   M-x send-to-ai
   ```
   Or use the full function name:
   ```
   M-x emacs-ai-agent-bridge-send-region-to-tmux
   ```

### Stop Monitoring
```
M-x emacs-ai-agent-bridge-stop-monitoring
```

### Check Status
```
M-x emacs-ai-agent-bridge-monitor-status
```

## Configuration

```elisp
;; tmux session to monitor (nil for auto-detect)
(setq emacs-ai-agent-bridge-tmux-session nil)

;; Pane ID to monitor (default: "0")
(setq emacs-ai-agent-bridge-tmux-pane "0")

;; Monitoring interval in seconds (default: 5)
(setq emacs-ai-agent-bridge-monitor-interval 5)
```

## How It Works

1. **Monitoring**: The extension monitors tmux console content every 5 seconds
2. **Detection**: When content remains unchanged, it assumes the AI agent is waiting for input
3. **Notification**: The *ai* buffer is displayed once when a prompt is detected
4. **Focus**: Your current buffer remains focused while the *ai* buffer is shown
5. **Sending**: Selected text is sent to tmux followed by Enter key (C-m) for execution

## Requirements

- Emacs 25.1 or later
- tmux
- AI coding agent running in tmux (e.g., Claude Code)

## License

GPL v3
