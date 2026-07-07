;;; init-github.el --- GitHub configuration and utilities

;;; Commentary:
;; Configuration for GitHub integration and related tools

;;; Code:
(require 'use-package)


;; GitHub API client
(use-package gh
  :ensure t
  :defer t)

;; Transient from MELPA (magit requires >= 0.12, Emacs 29.4 bundles older)
(use-package transient
  :ensure t)

;; Keep magit-section in sync with magit
(use-package magit-section
  :ensure t)

;; Magit configuration
(use-package magit
  :ensure t
  :after transient
  :init
  (setq magit-git-executable (or (executable-find "git") "/usr/bin/git"))
  :bind (("<f6>" . magit-status)  ; Keep existing binding
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g c" . magit-checkout)
         ("C-c g p" . magit-push))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

;; GitHub Codespaces
(use-package codespaces
  :ensure t
  :after gh
  :config
  (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect))

(provide 'init-github)
;;; init-github.el ends here
