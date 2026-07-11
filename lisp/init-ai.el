;;; init-ai --- AI Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; AI assistant integrations, including pi-coding-agent.

;;; Code:

(require 'use-package)

(use-package s)

(use-package google-gemini
  :init
  ;; The elisp package uses an API key.  Browser/subscription login is a CLI/app
  ;; concern, not something this package currently exposes.
  (setq google-gemini-key (getenv "GOOGLE_GEMINI_API_KEY")))

(use-package openai
  :defer t)

(use-package ai-code
  :defer t)

(use-package agent-shell
  :defer t)

(use-package websocket
  :defer t)

(use-package alert
  :defer t)

(use-package claude-code
  :commands (claude-code-transient
             claude-code-run
             claude-code-switch-to-buffer
             claude-code-close
             claude-code-quit
             claude-code-send-region
             claude-code-open-prompt-file
             claude-code-mcp-events-enable
             claude-code-mcp-events-disable)
  :bind (("C-c c" . claude-code-transient)
         ("C-c C" . claude-code-run)
         ("C-c M-c" . claude-code-switch-to-buffer))
  :init
  (setq claude-code-executable "claude"
        claude-code-vterm-buffer-multiline-output t
        claude-code-vterm-multiline-delay 0.016
        claude-code-mcp-host "localhost"
        claude-code-mcp-max-connection-attempts 12
        claude-code-mcp-connection-retry-delay 2
        claude-code-mcp-ping-interval 30
        claude-code-mcp-ping-timeout 10
        claude-code-mcp-events-change-delay 0.75
        claude-code-mcp-events-buffer-list-delay 1.0
        claude-code-mcp-events-diagnostics-delay 1.5)
  :config
  ;; This package's MCP support is Emacs-as-tool-provider: Claude Code's MCP
  ;; side starts/announces a per-project WebSocket server, and Emacs connects to
  ;; it to expose buffers, selection, diagnostics, definitions, references, and
  ;; diff-opening helpers.
  (claude-code-mcp-events-enable))

(defcustom whit-pi-coding-agent-install-grammars-on-load t
  "When non-nil, install missing pi-coding-agent Tree-sitter grammars on load.
The compiled grammar libraries are machine-specific build artifacts, so the
config rebuilds them from pi-coding-agent's pinned grammar recipes instead of
tracking the generated `tree-sitter/' directory in git."
  :type 'boolean
  :group 'convenience)

(declare-function pi-coding-agent--missing-essential-grammars
                  "pi-coding-agent-grammars")
(declare-function pi-coding-agent--missing-optional-grammars
                  "pi-coding-agent-grammars")
(declare-function pi-coding-agent--install-grammars
                  "pi-coding-agent-grammars")

(defun whit-pi-coding-agent-install-missing-grammars ()
  "Install missing Tree-sitter grammars from pi-coding-agent's recipes.
Returns the number of grammars installed.  This delegates recipe registration,
missing-grammar detection, and compilation to pi-coding-agent so fresh checkouts
rebuild the same grammar set pi uses for Markdown and embedded code rendering."
  (interactive)
  (if (require 'pi-coding-agent-grammars nil t)
      (let ((missing (delete-dups
                      (append (pi-coding-agent--missing-essential-grammars)
                              (pi-coding-agent--missing-optional-grammars)))))
        (if missing
            (pi-coding-agent--install-grammars missing)
          (message "All pi-coding-agent Tree-sitter grammars are installed. ✓")
          0))
    (display-warning 'whit
                     "pi-coding-agent-grammars could not be loaded; Tree-sitter grammars were not installed."
                     :warning)
    0))

(use-package pi-coding-agent
  :commands (pi-coding-agent
             pi-coding-agent-toggle
             pi-coding-agent-new-session
             pi-coding-agent-reload
             pi-coding-agent-install-grammars)
  :bind (("C-c p p" . pi-coding-agent)
         ("C-c p t" . pi-coding-agent-toggle)
         ("C-c p n" . pi-coding-agent-new-session)
         ("C-c p r" . pi-coding-agent-reload)
         ("C-c p g" . pi-coding-agent-install-grammars))
  :init
  ;; `pi-coding-agent-extra-args' can be used later for extension experiments,
  ;; e.g. (setq pi-coding-agent-extra-args '("-e" "/path/to/extension.ts")).
  (setq pi-coding-agent-executable '("pi")
        pi-coding-agent-input-window-height 12
        pi-coding-agent-tool-preview-lines 14
        pi-coding-agent-bash-preview-lines 8
        pi-coding-agent-preview-max-bytes (* 100 1024)
        pi-coding-agent-input-markdown-highlighting t
        pi-coding-agent-copy-raw-markdown t
        pi-coding-agent-thinking-display 'hidden
        pi-coding-agent-thinking-hidden-preview t
        pi-coding-agent-prettify-tables t
        pi-coding-agent-visit-file-other-window t)
  :config
  (defalias 'pi 'pi-coding-agent)
  (defalias 'pit 'pi-coding-agent-toggle)
  (when whit-pi-coding-agent-install-grammars-on-load
    (whit-pi-coding-agent-install-missing-grammars)))

(provide 'init-ai)
;;; init-ai.el ends here
