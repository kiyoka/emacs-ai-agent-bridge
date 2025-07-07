# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs extension that bridges an AI coding agent running in tmux with Emacs. It provides automatic monitoring of tmux sessions to detect when Claude Code is at an input prompt, and facilitates sending text from Emacs to the AI agent.

## Development Setup

Since this is an Emacs extension project, development will involve:
- Writing Emacs Lisp code (`.el` files)
- Testing within Emacs
- Potentially implementing shell scripts or Python for tmux integration

## Architecture Considerations

When implementing this bridge, consider:
1. **Emacs-side implementation**: Commands, modes, and UI elements for Emacs users
2. **tmux communication**: How to send commands to and receive output from tmux sessions
3. **AI agent protocol**: The format for exchanging data between Emacs and the AI agent
4. **Asynchronous processing**: Emacs operations should remain responsive while communicating with the AI agent

## Current Status

- Repository is on the `prototyping-1` branch
- Main implementation is in `emacs-ai-agent-bridge.el`
- Core features implemented:
  - Automatic tmux session monitoring (5-second intervals)
  - Prompt detection based on unchanged content (no pattern matching)
  - Automatic buffer display when prompt is detected (focus remains on current buffer)
  - Buffer shown only once per prompt detection
  - Region sending functionality with automatic C-m (Enter key)

## Implemented Features

### 1. tmux Monitoring
- **Function**: `emacs-ai-agent-bridge-start-monitoring` - Starts monitoring tmux session every 5 seconds
- **Function**: `emacs-ai-agent-bridge-stop-monitoring` - Stops the monitoring timer
- **Function**: `emacs-ai-agent-bridge-monitor-status` - Shows current monitoring status

### 2. Prompt Detection
The system detects when the AI agent is waiting for input by monitoring if the tmux console content remains unchanged between checks. When the content doesn't change for 5 seconds, it's assumed the agent is at a prompt waiting for input.
- Detection is based solely on content changes, not pattern matching
- *ai* buffer is displayed only once when prompt is first detected
- Buffer is reused if already visible, only content is updated
- Focus remains on the current working buffer

### 3. Text Sending
- **Function**: `send-to-ai` - Sends selected region to tmux with automatic C-m (Enter key)
- **Function**: `emacs-ai-agent-bridge-send-region-to-tmux` - Core implementation
- Text is sent to tmux followed by C-m to execute the command

### 4. Configuration Variables
- `emacs-ai-agent-bridge-tmux-session` - tmux session to monitor (nil for auto-detect)
- `emacs-ai-agent-bridge-tmux-pane` - Pane ID (default: "0")
- `emacs-ai-agent-bridge-monitor-interval` - Check interval in seconds (default: 5)

## File Structure

- `emacs-ai-agent-bridge.el` - Main package file with all core functionality
- `CLAUDE.md` - This documentation file
- `README.md` - User-facing documentation
- `LICENSE` - GPL v3 license