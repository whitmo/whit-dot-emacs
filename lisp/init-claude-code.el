;;; init-claude-code --- Claude Code integration

;;; Commentary:
;; Configuration for claude-code-emacs

;;; Code:
(require 'use-package)

(use-package vterm
  :ensure t)

(use-package claude-code
  :ensure t
  :after vterm
  :bind ("C-c c" . claude-code-transient)
  :init
  ;; Ensure full package loads when any autoloaded submodule triggers,
  ;; since claude-code-core uses declare-function for claude-code-ui symbols
  (with-eval-after-load 'claude-code-core
    (require 'claude-code))
  :config
  (define-key claude-code-vterm-mode-map (kbd "<S-return>")
    (lambda () (interactive)
      (vterm-send-string "\e[13;2u" t))))

(provide 'init-claude-code)
;;; init-claude-code.el ends here
