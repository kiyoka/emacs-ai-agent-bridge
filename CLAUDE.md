# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs extension that bridges an AI coding agent running in tmux with Emacs. The project is currently in the initial prototyping phase with no implementation yet.

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
- Only LICENSE (GPL v3) and minimal README.md exist
- No source code or configuration files implemented yet

## Expected File Structure

When implementing, typical Emacs package structure would include:
- `emacs-ai-agent-bridge.el` - Main package file with metadata and core functionality
- Additional `.el` files for modular functionality
- `Makefile` or similar for build/test automation
- Documentation in Commentary sections of Elisp files