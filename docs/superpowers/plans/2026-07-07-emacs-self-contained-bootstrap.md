# Emacs Self-Contained Bootstrap Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the approved self-contained Emacs bootstrap so a fresh clone can validate and load itself with minimal manual work.

**Architecture:** Keep package bootstrap in `init.el`, keep feature modules under `lisp/`, add non-destructive shell diagnostics under `bin/`, and document the fresh-machine flow in `README.md`.

**Tech Stack:** Emacs Lisp, `package.el`, `use-package`, POSIX shell/Bash, Git.

## Global Constraints

- Do not commit secrets or local machine tokens.
- Do not require Homebrew, Nix, or devenv for basic startup.
- Keep optional feature tooling from blocking Emacs startup.
- Preserve the existing modular `lisp/init-*.el` structure.

---

### Task 1: Bootstrap helpers and local override support

**Files:**
- Modify: `init.el`
- Modify: `.gitignore`

**Interfaces:**
- Produces: `whit-package-install-if-missing`, `whit-package-vc-install-if-missing`, `whit-load-optional-file`, `whit-available-font-family`.
- Produces: optional `lisp/init-local.el` load point.

- [x] Write a failing shell check that confirms `init.el` references `init-local` and `.gitignore` ignores it.
- [x] Add package bootstrap helpers.
- [x] Enable `use-package-always-ensure`.
- [x] Replace direct `package-vc-install` with the helper.
- [x] Remove stale Custom entries that hard-code `nil`, git, and ssh.
- [x] Load `lisp/init-local.el` last if present.
- [x] Add ignored local override files to `.gitignore`.

### Task 2: External executable and font fallback cleanup

**Files:**
- Modify: `init.el`
- Modify: `lisp/init-general.el`
- Modify: `lisp/init-github.el`

**Interfaces:**
- Consumes: `whit-available-font-family`.
- Produces: dynamic git, ssh, and font selection.

- [x] Prefer `(executable-find "git")` for Magit.
- [x] Prefer `(executable-find "ssh")` for remote shell program.
- [x] Use Source Code Pro when installed, otherwise fall back to Menlo/Monaco when available.

### Task 3: Verification and doctor scripts

**Files:**
- Create: `bin/check`
- Create: `bin/doctor`

**Interfaces:**
- Produces: executable `bin/check` for batch-load verification.
- Produces: executable `bin/doctor` for non-destructive new-machine diagnostics.

- [x] Add `bin/check` wrapping the fresh-load Emacs command.
- [x] Add `bin/doctor` checks for Emacs, git, ssh, optional tools, env vars, font, local override status, and `bin/check`.
- [x] Mark scripts executable.

### Task 4: New-machine documentation

**Files:**
- Create: `README.md`

**Interfaces:**
- Produces: documented fresh-machine setup and local override process.

- [x] Document clone-and-launch setup.
- [x] Document scripts.
- [x] Document optional external tools.
- [x] Document local overrides and secret handling.
- [x] Include future devenv-awareness note.

### Task 5: Verification, commit, merge, push

**Files:**
- All changed files.

**Interfaces:**
- Consumes: `bin/check`, `bin/doctor`.
- Produces: pushed `main` containing the implementation.

- [x] Run red/green implementation checks.
- [x] Run `bin/check`.
- [x] Run `bin/doctor`.
- [x] Commit implementation.
- [x] Merge to `main`.
- [x] Push `main`.
