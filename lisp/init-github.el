;;; init-github.el --- GitHub configuration and utilities

;;; Commentary:
;; Configuration for GitHub integration and related tools

;;; Code:
(require 'use-package)

;; GitHub API client
(use-package gh
  :ensure t)

;; Magit configuration
(use-package magit
  :ensure t
  :bind (("<f6>" . magit-status)  ; Keep existing binding
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g c" . magit-checkout)
         ("C-c g p" . magit-push))
  :config
  (setq magit-display-buffer-function 
        #'magit-display-buffer-same-window-except-diff-v1))

;; GitHub Copilot
(use-package copilot
  :quelpa ((copilot :fetcher github
                    :repo "copilot-emacs/copilot.el"
                    :branch "main"
                    :files ("dist" "*.el"))
           :upgrade t)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("M-[" . copilot-previous-completion)
              ("M-]" . copilot-next-completion))
  :hook (prog-mode . copilot-mode))

;; GitHub Codespaces
(use-package codespaces
  :ensure t
  :after (gh)
  :config
  (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect))

(provide 'init-github)
;;; init-github.el ends here
