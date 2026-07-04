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
- `emacs-ai-agent-bridge-buffering-refresh-interval` - Seconds of continuous change before forcing a dimmed *ai* buffer refresh (default: 60, 0 disables)

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

## Issue #14 Implementation

### Feature Request
Support multiple tmux sessions and allow users to switch between them.

### Problem
The current implementation only supports monitoring a single tmux session. When multiple sessions are running, users cannot easily switch between them.

### Implementation
Added functions to list and switch between tmux sessions (emacs-ai-agent-bridge.el:97-120):

1. **New Function**: `emacs-ai-agent-bridge-get-all-tmux-sessions` - Returns a list of all available tmux sessions
2. **New Function**: `emacs-ai-agent-bridge-select-session` - Interactive command to select and switch to a different tmux session

**Behavior**:
- On Emacs startup, the first session (lowest number) is automatically selected
- Users can manually switch sessions using `M-x emacs-ai-agent-bridge-select-session`
- When switching sessions, monitoring is automatically restarted for the new session
- The `completing-read` interface shows the current session and allows selection from all available sessions

**Configuration**:
- The `emacs-ai-agent-bridge-tmux-session` variable stores the currently selected session
- Set to nil for automatic detection (uses first available session)

**Usage Example**:
```
M-x emacs-ai-agent-bridge-select-session
Select tmux session (current: 0): [1, 2, claude, dev]
```

### Mode-line Integration

**Added Function**: `emacs-ai-agent-bridge-popup-select-session` - Popup-based session selection using popup-el

**Mode-line Display**:
- Current tmux session is displayed in the mode-line as `[tmux:0]`
- Clicking on the session name opens a popup menu with all available sessions
- Session display is automatically added when `emacs-ai-agent-bridge-input-mode` is enabled
- Uses `global-mode-string` for display (typically appears on the right side of the mode-line)
- Also displayed in `*ai*` buffer mode-line for easy access

**Dependencies**:
- Requires `popup` package (version 0.5.3 or later)
- Added to Package-Requires for automatic installation

## Issue #15 Fix

### Problem
After implementing multiple tmux session support (Issue #14), switching sessions via the popup menu worked correctly (the Emacs Lisp side recognized the selected session), but text was still being sent to the wrong session.

### Root Cause
The following functions were always using `emacs-ai-agent-bridge-get-first-tmux-session` instead of respecting the user's selected session stored in `emacs-ai-agent-bridge-tmux-session`:
1. `emacs-ai-agent-bridge-send-region-to-tmux` (line 190)
2. `emacs-ai-agent-bridge-select-option` (line 256)
3. `emacs-ai-agent-bridge-smart-return` (line 296)
4. `emacs-ai-agent-bridge-process-ai-line` (lines 582 and 612)

These functions ignored the selected session and always sent commands to the first tmux session.

### Fix
Modified all text-sending functions to use the same pattern as `emacs-ai-agent-bridge-capture-tmux-pane`:
```elisp
(session (or emacs-ai-agent-bridge-tmux-session
             (emacs-ai-agent-bridge-get-first-tmux-session)))
```

This ensures the selected session is used first, and only falls back to the first session if no session has been explicitly selected.

**Modified Functions**:
1. `emacs-ai-agent-bridge-send-region-to-tmux` - Now respects selected session when sending regions
2. `emacs-ai-agent-bridge-select-option` - Now respects selected session when selecting numbered options
3. `emacs-ai-agent-bridge-smart-return` - Now respects selected session when pressing Enter in *ai* buffer
4. `emacs-ai-agent-bridge-process-ai-line` - Now respects selected session for both single-line and multi-line @ai commands

**Verification**:
- ✓ Switching sessions via popup menu now correctly routes all commands to the selected session
- ✓ Text sending, option selection, and @ai commands all use the correct session
- ✓ Auto-detection still works when no session is explicitly selected

## Issue #17 Fix

### Problem
Claude Code UI changed again, and numeric key selection (1, 2, 3, etc.) stopped working for option prompts. Users must now navigate with up/down cursor keys to select options.

### Fix
Added two new keybindings to the `*ai*` buffer keymap:
- `C-c C-n` - Send `Down` arrow key to tmux (move to next option)
- `C-c C-p` - Send `Up` arrow key to tmux (move to previous option)

**New Functions** (emacs-ai-agent-bridge.el):
- `emacs-ai-agent-bridge-send-down` - Sends `Down` key to the selected tmux session
- `emacs-ai-agent-bridge-send-up` - Sends `Up` key to the selected tmux session

**Usage**:
```
 Do you want to proceed?
   1. Yes
 ❯ 2. Yes, and don't ask again for: git push:*
   3. No
```
→ Use `C-c C-p` to move up to option 1, then `C-m` to confirm.

**Note**: `C-n` and `C-p` were intentionally NOT used, as they are needed for free cursor movement within the `*ai*` buffer (which displays tmux scrollback history).

**Verification**:
- ✓ `C-c C-n` correctly sends `Down` arrow key to tmux
- ✓ `C-c C-p` correctly sends `Up` arrow key to tmux
- ✓ Both keybindings respect the selected tmux session (Issue #15 pattern)
## Issue #21 Implementation

### Feature Request
When pressing Enter on a text input prompt (`❯`) in the `*ai*` buffer, accept input from the minibuffer and send it to tmux.

### Problem
There was no way to directly input text from the `*ai*` buffer to Claude Code's text input prompt (marked with `❯`).

### Implementation

**New Functions**:
- `emacs-ai-agent-bridge-is-cursor-on-input-prompt-p` - Checks if the cursor is on a text input prompt line. Conditions: line contains `❯`, does not contain number+dot (option numbers), and is surrounded by `----------` separator lines above and below
- `emacs-ai-agent-bridge-flash-region` - Briefly highlights sent text with a flash effect
- `emacs-ai-agent-bridge-read-and-send-input` - Reads text from minibuffer and sends it to the tmux session. After sending, displays the input text on the prompt line

**Modified Function**:
- `emacs-ai-agent-bridge-smart-return` - Extended to launch minibuffer input mode when prompt is detected and cursor is on an input prompt line

**Workflow**:
1. Place cursor on the `❯` prompt line in the `*ai*` buffer and press Enter
2. "AI input: " prompt appears in the minibuffer
3. Type text and press Enter to send
4. Input text is displayed on the prompt line with a flash highlight effect

**Prompt Detection Conditions**:
- Line contains `❯`
- Line does not contain number+dot (option numbers)
- Line above contains `-----------`
- Line below contains `-----------`

**Verification**:
- ✓ Pressing Enter on `❯` prompt line launches minibuffer input
- ✓ Input text is correctly sent to tmux session
- ✓ After sending, input text is displayed with flash effect on prompt line
- ✓ Does not misfire on option prompts (with numbers)

## Issue #25 Fix

### Problem
When using multiple tmux sessions, switching the target session from Emacs (via the
mode-line popup or `M-x emacs-ai-agent-bridge-select-session`) updated the captured
content correctly, but text/keys sent from Emacs were still delivered to the **wrong**
tmux session (typically the first / attached session). This is a recurrence of the
symptom originally addressed in Issue #15.

### Root Cause
The capture path and the send path built the tmux target differently:
- Capture (`emacs-ai-agent-bridge-capture-tmux-pane`) used `-t SESSION:PANE` (e.g. `1:0`).
- Send (`emacs-ai-agent-bridge-send-to-tmux` and
  `emacs-ai-agent-bridge-send-key-to-tmux`) used a bare `-t SESSION` (e.g. `1`).

A bare numeric target like `-t 1` is ambiguous in tmux's target resolution: when
sessions are named with plain numbers, tmux may interpret the token as a **window index
within the currently attached session** rather than the **session named "1"**. As a
result, `send-keys` landed in the wrong session even though
`emacs-ai-agent-bridge-tmux-session` was set correctly. Capture was unaffected because
it always included `:PANE`.

### Fix
Made the send helpers build the target the same way as the capture path
(emacs-ai-agent-bridge.el):
- `emacs-ai-agent-bridge-send-to-tmux` - now targets `SESSION:PANE` built from
  `session` and `emacs-ai-agent-bridge-tmux-pane`
- `emacs-ai-agent-bridge-send-key-to-tmux` - same `SESSION:PANE` target

Because every send caller routes through these two helpers, the fix is centralized and
covers region sending, option selection, Up/Down navigation, minibuffer input, and
`@ai` line processing.

**Verification**:
- ✓ After switching sessions, text and keys are delivered to the selected session
- ✓ Capture and send now target the same `SESSION:PANE`
- ✓ Works with numeric session names (`0`, `1`, ...) that previously misfired
- ✓ File byte-compiles cleanly (only the unrelated `popup` dependency require)

**Version**: bumped from 0.6.0 to 0.6.1

## Issue #27 Implementation

### Feature Request
Even when the tmux screen keeps changing continuously, after the change has
continued for 1 minute, reflect the current content into the `*ai*` buffer. In
that case, display the text in a faint color (e.g. light gray) so it is clear
the tmux screen is still changing.

### Problem
The `*ai*` buffer was only updated when the tmux content **stopped** changing
(a prompt was detected via `emacs-ai-agent-bridge-content-unchanged-p`). While
the AI agent kept producing output, the buffer never updated — only the
mode-line spinner (Issue #23) animated. For long-running output, the user could
not see any progress in the `*ai*` buffer until the output stabilized.

### Implementation

**New configuration variable**:
- `emacs-ai-agent-bridge-buffering-refresh-interval` (default: 60, 0 disables) -
  Seconds of continuous change before forcing a dimmed `*ai*` buffer refresh.

**New face**:
- `emacs-ai-agent-bridge-buffering-face` - Faint gray (background-aware:
  `gray50` on dark, `gray70` on light) used to display still-changing content.

**New state variable**:
- `emacs-ai-agent-bridge--change-start-time` - Records when the content started
  changing continuously; nil while the content is stable.

**Modified functions**:
- `emacs-ai-agent-bridge-update-ai-buffer` - Now takes an optional `buffering`
  argument. When non-nil, option colorization is skipped and the whole buffer is
  dimmed via `buffer-face-set` (remapping the default face to
  `emacs-ai-agent-bridge-buffering-face`); when nil it restores normal colors
  with `(buffer-face-set nil)`. Using `buffer-face-set` reliably tints every
  line regardless of per-line text properties.
- `emacs-ai-agent-bridge-monitor-tmux` - In the "content changed" branch, records
  `--change-start-time` on the first change, and once the elapsed time reaches
  `emacs-ai-agent-bridge-buffering-refresh-interval` it calls
  `emacs-ai-agent-bridge-update-ai-buffer` with `buffering` = t and restarts the
  timer (so it refreshes again every interval while still changing). The
  "content unchanged / prompt detected" branch clears `--change-start-time`, so
  the stabilized content is shown in the normal (non-dimmed) color.
- `emacs-ai-agent-bridge-start-monitoring` - Resets `--change-start-time` to nil.

### Behavior
- Output confirmed (prompt detected): content shown in normal color.
- Output changing for >= 60s: current (still-changing) content reflected into the
  `*ai*` buffer dimmed in faint gray; refreshes again every 60s while changing.
- Once the output stabilizes, the normal-color update replaces the dimmed view.

### Verification
- ✓ `buffering` = t dims the whole `*ai*` buffer content; `buffering` = nil leaves
  it in the normal color
- ✓ Forced dimmed refresh fires after the configured interval of continuous change
  and restarts the timer for subsequent refreshes
- ✓ When content stabilizes, the change timer clears and the buffer reverts to
  normal color
- ✓ `emacs-ai-agent-bridge-buffering-refresh-interval` = 0 disables the forced refresh
- ✓ File byte-compiles cleanly (only the unrelated `popup` dependency require)

**Version**: bumped from 0.6.1 to 0.7.0

## Issue #29 Fix

### Problem
After the monitored tmux session was killed (e.g. `tmux kill-session -t 0`),
switching to a new session from Emacs could take an extremely long time — in some
cases Emacs stayed unresponsive for 5 minutes or more, appearing frozen.

### Root Causes
Three structural problems combined to cause the freeze:

1. **Repeating timer starvation**: monitoring used `(run-with-timer 0 2 ...)`.
   If a single check ever took longer than the 2-second interval (slow tmux,
   slow filesystem, WSL interop hiccup), the repeating timer became permanently
   overdue, so Emacs ran checks back to back and user input was starved —
   Emacs appeared completely frozen.
2. **Shell commands inherited the current buffer's `default-directory`**:
   every tmux invocation went through `shell-command`/`shell-command-to-string`,
   which spawns a shell in the current buffer's directory. That directory can be
   remote (TRAMP), already deleted, or on a slow network mount (e.g. OneDrive
   via WSL's 9p `/mnt/c`); each spawn could then block for minutes, repeated
   every 2 seconds by the monitor timer.
3. **No dead-session detection**: `emacs-ai-agent-bridge-tmux-session` kept
   pointing at the killed session, every capture failed, and tmux's stderr
   error text ("can't find session") was even treated as pane content and
   displayed in the `*ai*` buffer as if it were a prompt.

### Fix
1. **Self-rescheduling one-shot timer** — new `emacs-ai-agent-bridge--monitor-tick`
   runs one check and only then schedules the next with a one-shot
   `run-with-timer`, guaranteeing idle time between checks; a slow check can no
   longer starve the event loop.  `emacs-ai-agent-bridge-stop-monitoring` still
   simply cancels the timer and clears the variable, which also stops rescheduling.
2. **Direct `call-process` tmux invocation** — new helper
   `emacs-ai-agent-bridge-call-tmux` runs tmux without a shell, with
   `default-directory` bound to `temporary-file-directory`, and returns
   `(EXIT-CODE . STDOUT)`; stderr is discarded so error text can never be
   mistaken for pane content.  All tmux calls (capture, send-keys,
   list-sessions) now go through it.
3. **Session liveness check and automatic failover** — new
   `emacs-ai-agent-bridge-session-exists-p` (exact-name match against
   `tmux list-sessions`) and `emacs-ai-agent-bridge--resolve-live-session`.
   On every tick the monitor verifies the selected session still exists; if it
   is gone it immediately switches to the first available session (message:
   "tmux session X is gone; switched to session Y") and resets detection state.
   If no session exists at all it waits quietly and adopts the first session
   that appears.  Send helpers (`emacs-ai-agent-bridge-send-to-tmux`,
   `emacs-ai-agent-bridge-send-key-to-tmux`) signal a `user-error` when the
   target session is dead, so nothing is silently sent to the wrong place.
4. **Misc hardening** — `emacs-ai-agent-bridge-capture-tmux-pane` returns nil
   on failure instead of raising or returning error text;
   `emacs-ai-agent-bridge-get-first-tmux-session` sorts in Lisp with
   `string-version-lessp` instead of shelling out to `sort -V`;
   `emacs-ai-agent-bridge-select-session` reports "No tmux sessions found"
   instead of offering an empty completion list.

### Verification
- ✓ Batch-mode scenario test on an isolated tmux server (socket `-L eabtest`):
  kill monitored session → next tick auto-switches to the surviving session in
  0.007s; all-sessions-gone → ticks stay fast and quiet; new session appears →
  adopted automatically; sends to a dead session raise `user-error`;
  timer is one-shot and reschedules itself; stop-monitoring cancels cleanly
  (23/23 checks passed)
- ✓ File byte-compiles cleanly (only pre-existing warnings)

**Version**: bumped from 0.7.0 to 0.7.1
