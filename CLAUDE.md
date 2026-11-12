# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when
working with code in this repository.

## Overview

Personal Emacs configuration using `use-package` for package
management. Modular architecture with configuration split into
feature-specific files under `lisp/`.

## Architecture

**init.el** is the entry point. It:
1. Configures package archives (MELPA, MELPA Stable, JCS ELPA)
2. Bootstraps `use-package` and `quelpa`
3. Adds `lisp/` to load-path
4. Loads `init-env` first (environment variables), then remaining
   modules with `:demand t`
5. Contains `custom-set-variables` and `custom-set-faces` blocks at
   the end

**Module conventions** in `lisp/`:
- Files named `init-<feature>.el`, each `(provide 'init-<feature>)`
- Each file `(require 'use-package)` at the top
- Standard header/footer: `;;; init-<name> --- Description` / `;;;
  init-<name>.el ends here`

**Modules:**
- `init-env` — Imports shell environment variables (API keys for
  Anthropic, Gemini, Deepseek) via `exec-path-from-shell`
- `init-whit` — Personal constants (name, email)
- `init-customvar` — Custom variable definitions
- `init-general` — Development tools: Python (ruff, flymake,
  pdbtrack), Lua, editorconfig, shell-switcher, keybindings
- `init-ai` — Google Gemini integration
- `init-github` — Magit, GitHub CLI (`gh`), Codespaces
- `init-music` — Norns/SuperCollider/Lua music tooling

## Key Keybindings

- F4: shell-here, F5: rename-buffer, F6: magit-status
- C-c g: GitHub prefix, C-c S: Codespaces
- M-p M-p: Python debugger macros (break, postmortem, review)

## Development Notes

- Byte-compile with `emacs -batch -f batch-byte-compile init.el` or
  `M-x byte-compile-file`
- No test suite or Makefile; validate by loading in Emacs
- Git commits use conventional commit prefixes (feat:, fix:,
  refactor:)
- `.gitignore` excludes: `.env`, `*~`, `*.elc`
- Packages installed via `use-package :ensure t` or
  `package-vc-install` for VCS sources
