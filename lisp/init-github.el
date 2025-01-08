;;; init-github.el --- GitHub configuration and utilities

;;; Commentary:
;; Configuration for GitHub integration and related tools

;;; Code:
(require 'use-package)

;; Ensure quelpa and quelpa-use-package are available
(package-initialize)
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(require 'quelpa)
(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))

;; GitHub API client
(use-package gh
  :ensure t
  :defer t)

;; Magit configuration
(use-package magit
  :ensure t
  :init
  (setq magit-git-executable "/usr/bin/git")
  :bind (("<f6>" . magit-status)  ; Keep existing binding
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g c" . magit-checkout)
         ("C-c g p" . magit-push))
  :config
  (setq magit-display-buffer-function 
        #'magit-display-buffer-same-window-except-diff-v1))

;; Dependencies for Copilot
(use-package dash :ensure t)
(use-package s :ensure t)
(use-package editorconfig :ensure t)

;; GitHub Copilot
(use-package copilot
  :ensure nil
  :quelpa ((copilot :fetcher github
                    :repo "copilot-emacs/copilot.el"
                    :branch "main"
                    :files ("dist" "*.el")))
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("M-[" . copilot-previous-completion)
              ("M-]" . copilot-next-completion))
  :hook (prog-mode . copilot-mode)
  :config
  ;; Enable Copilot globally
  (global-copilot-mode))

;; Copilot Chat
(use-package copilot-chat
  :ensure nil
  :quelpa ((copilot-chat :fetcher github
                         :repo "copilot-emacs/copilot-chat.el"
                         :branch "main"))
  :after copilot
  :bind (("C-c c c" . copilot-chat)
         ("C-c c e" . copilot-chat-explain-code)
         ("C-c c t" . copilot-chat-generate-tests))
  :config
  (setq copilot-chat-auth-source "~/.authinfo.gpg"))

;; GitHub Codespaces
(use-package codespaces
  :ensure t
  :after gh
  :config
  (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect))

(provide 'init-github)
;;; init-github.el ends here
