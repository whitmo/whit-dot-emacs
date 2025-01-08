;;; hail the hub

(use-package codespaces
  :config (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect))
;;; init-github --- GitHub configuration and utilities

;;; Commentary:
;; Configuration for GitHub integration and related tools

;;; Code:
(require 'use-package)

(use-package gh
  :ensure t)

(provide 'init-github)
;;; init-github.el ends here
