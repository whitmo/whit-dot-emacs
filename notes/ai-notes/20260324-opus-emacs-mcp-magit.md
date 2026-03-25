# Emacs MCP Analysis & Magit Fix

## Summary

- Analyzed `emacs-mcp-stdio.sh` for issues: `eval` fragility, missing temp file cleanup trap, no error handling in main loop, no Emacs server pre-flight check
- Saved a detailed plan for installing `emacs-mcp-stdio.sh` as a Claude Code MCP server (alongside existing WebSocket-based `emacs` MCP) at `.claude/planning/emacs-mcp-stdio-install.md`
- Fixed magit crash (`transient--set-layout` void function) by adding `(use-package transient :ensure t)` to `lisp/init-github.el` — magit 20260322 needs transient >= 0.12, Emacs 29.4 bundles an older version
- Clarified that `claude.ai Gmail` and `claude.ai Google Calendar` MCP entries are built-in Anthropic integrations, not injected — they do nothing without explicit OAuth authentication
- Reviewed the 11 tools available via the connected `emacs` MCP server (buffers, LSP, diffs, notifications)

## Session Details

### Magit Fix

**Root cause:** Magit `20260322` requires `transient >= 0.12` which defines `transient--set-layout`. Emacs 29.4 bundles an older transient without this function.

**Fix:** Added to `lisp/init-github.el`:
```elisp
(use-package transient
  :ensure t)
```
And `:after transient` to the magit use-package block. Transient `20260324.1304` installed from MELPA.

### MCP stdio Plan (saved for later)

Plan at `.claude/planning/emacs-mcp-stdio-install.md` covers:
- Creating `lisp/init-mcp-server.el` with `emacs-mcp-enable`/`emacs-mcp-disable` functions
- Registering tools via `mcp-server-lib-register-tool` mirroring existing claude-code MCP tools
- Running `claude mcp add -s user -t stdio emacs-mcp -- ~/.emacs.d/emacs-mcp-stdio.sh --init-function=emacs-mcp-enable --stop-function=emacs-mcp-disable`

### Script Issues Noted

1. `eval` usage (lines 122, 194) — fragile with special chars in paths
2. No `trap` for temp file cleanup on kill
3. `set -e` silently kills on emacsclient failure in main loop
4. No pre-flight check for Emacs server

---
*Session completed: 2026-03-24 16:45*
