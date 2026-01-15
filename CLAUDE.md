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
  - Automatic tmux session monitoring (2-second intervals)
  - Prompt detection based on unchanged content (no pattern matching)
  - Automatic buffer display when prompt is detected (focus remains on current buffer)
  - Buffer shown only once per prompt detection
  - Region sending functionality with automatic C-m (Enter key)

## Implemented Features

### 1. tmux Monitoring
- **Function**: `emacs-ai-agent-bridge-start-monitoring` - Starts monitoring tmux session every 2 seconds
- **Function**: `emacs-ai-agent-bridge-stop-monitoring` - Stops the monitoring timer
- **Function**: `emacs-ai-agent-bridge-monitor-status` - Shows current monitoring status

### 2. Prompt Detection
The system detects when the AI agent is waiting for input by monitoring if the tmux console content remains unchanged between checks. When the content doesn't change for 2 seconds, it's assumed the agent is at a prompt waiting for input.
- Detection is based solely on content changes, not pattern matching
- *ai* buffer is displayed only once when prompt is first detected
- Buffer is reused if already visible, only content is updated
- Focus remains on the current working buffer

### 3. Text Sending
- **Function**: `send-to-ai` - Sends selected region to tmux with automatic C-m (Enter key)
- **Function**: `emacs-ai-agent-bridge-send-region-to-tmux` - Core implementation
- **Function**: `emacs-ai-agent-bridge-send-block-to-ai` - Sends consecutive lines before cursor (bound to C-c <return>)
- Text is sent to tmux followed by C-m to execute the command

### 4. Configuration Variables
- `emacs-ai-agent-bridge-tmux-session` - tmux session to monitor (nil for auto-detect)
- `emacs-ai-agent-bridge-tmux-pane` - Pane ID (default: "0")
- `emacs-ai-agent-bridge-monitor-interval` - Check interval in seconds (default: 2)
- `emacs-ai-agent-bridge-scrollback-lines` - Number of scrollback lines to capture from tmux history (default: 3000)

## File Structure

- `emacs-ai-agent-bridge.el` - Main package file with all core functionality
- `CLAUDE.md` - This documentation file
- `README.md` - User-facing documentation
- `LICENSE` - GPL v3 license

## Issue #8 Fix

### Problem
After updating to Claude Code 2.0.31, the following issues occurred due to UI changes:
- Numeric key selection (1, 2, 3, etc.) stopped working
- Arrow key navigation was inverted (pressing up at the top would jump to the bottom, then cycle upward through options 3→2→1)

### Fix
Modified `emacs-ai-agent-bridge-select-option` function (emacs-ai-agent-bridge.el:176-186):
- For Claude Code 2.0.31+, changed to send numeric keys directly to tmux
- Previous implementation used a complex selection process with arrow keys and Enter, but the new version simplifies this by sending numeric keys directly

**Related Functions**:
- `emacs-ai-select-option-1` through `emacs-ai-select-option-5` - Directly select corresponding options
- Pressing keys 1-5 in the *ai* buffer allows direct selection of Claude Code options

## Issue #11 Implementation

### Feature Request
Display past messages that have scrolled off-screen in tmux within the *ai* buffer.

### Problem
Previously, the *ai* buffer only captured content visible in the tmux window, preventing access to messages that had scrolled off-screen. Users wanted to review past conversations comprehensively.

### Implementation
Modified `emacs-ai-agent-bridge-capture-tmux-pane` function (emacs-ai-agent-bridge.el:269-283):
- Added new configuration variable `emacs-ai-agent-bridge-scrollback-lines` (default: 3000)
- Enhanced `tmux capture-pane` command to include `-S` option for scrollback history
- When `emacs-ai-agent-bridge-scrollback-lines` > 0, captures from that many lines back in the scrollback buffer
- When set to 0, captures only visible content (previous behavior)

**Configuration**:
- Users can customize the number of scrollback lines by setting `emacs-ai-agent-bridge-scrollback-lines`
- Example: `(setq emacs-ai-agent-bridge-scrollback-lines 5000)` to capture 5000 lines of history

## Issue #12 Fix

### Problem
After enabling `emacs-ai-agent-bridge-input-mode`, the RET key binding was overridden to call `emacs-ai-agent-bridge-smart-input-return`, which only called `newline` for non-@ai lines. This prevented mode-specific RET key behaviors from working correctly, such as:
- markdown-mode's automatic table alignment feature
- org-mode's list continuation and other org-return behaviors
- Other major modes' custom RET key handlers

### Fix
Modified `emacs-ai-agent-bridge-smart-input-return` function to be mode-independent (emacs-ai-agent-bridge.el:612-638):
- Added `emacs-ai-agent-bridge-get-original-return-command` function to retrieve the original RET key binding by temporarily disabling the minor mode
- Added `emacs-ai-agent-bridge-call-original-return` function to call the underlying mode's RET handler via `call-interactively`
- Modified `emacs-ai-agent-bridge-smart-input-return` to call the original RET handler instead of just `newline` when not processing @ai lines

**Technical Details**:
- Uses `let` binding to temporarily set `emacs-ai-agent-bridge-input-mode` to nil
- Retrieves the original key binding with `(key-binding (kbd "RET"))`
- Calls the original command with `call-interactively` to preserve all mode-specific behavior
- Falls back to `newline` if no original command is found

**Benefits**:
- Works with any major mode without modification
- Preserves all mode-specific RET key behaviors (table alignment, list continuation, etc.)
- No dependencies on specific modes like markdown-mode or org-mode

### Implementation Changes

1. **New Helper Functions** (emacs-ai-agent-bridge.el:612-625):
   - `emacs-ai-agent-bridge-get-original-return-command`: Retrieve the original RET key binding by temporarily disabling the minor mode
   - `emacs-ai-agent-bridge-call-original-return`: Invoke the underlying mode's RET handler via `call-interactively`

2. **Modified Function** (emacs-ai-agent-bridge.el:627-638):
   - `emacs-ai-agent-bridge-smart-input-return`: Call `emacs-ai-agent-bridge-call-original-return` instead of simply calling `newline`

3. **Documentation Updates**:
   - Added Issue #12 fix details to CLAUDE.md
   - Version bumped from 0.2.0 to 0.3.0

### Verification

- ✓ Original RET handlers are correctly invoked in text-mode, org-mode, and other modes
- ✓ `org-return` is preserved in org-mode (list continuation and other features)
- ✓ markdown-mode table auto-alignment works (due to mode-independence)
- ✓ @ai line processing continues to work correctly

### Technical Highlights

- **Completely mode-independent**: Works with markdown-mode, org-mode, and any other mode without modifications
- **Preserves mode-specific features**: Each mode's special RET key functionality is maintained
- **High maintainability**: No changes needed when new modes are added