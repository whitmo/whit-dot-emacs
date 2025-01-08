;;; init-ai --- AI Helpers

;;; Commentary:
;;
(require 'package)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Initialize quelpa
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

;; -- copilot --

(use-package s)

;; (define-key copilot-completion-map (kbd "<tab>") #'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") #'copilot-accept-completion)

(use-package copilot-chat
  :ensure t
  :after (request shell-maker)
  :custom
  (copilot-chat-frontend 'shell-maker)
  :config
  (require 'copilot-chat-shell-maker)
  (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
  (copilot-chat-shell-maker-init))

(use-package copilot
  :quelpa ((copilot :fetcher github
                    :repo "copilot-emacs/copilot.el"
                    :branch "main"
                    :files ("dist" "*.el"))
           :upgrade t)
  :bind (
	 :map copilot-mode-map
   	 ("<tab>" . copilot-accept-completion)
   	 ("TAB" . copilot-accept-completion)
   	 )
  :hook (prog-mode . copilot-mode)
  )
;;(use-package chatgpt :ensure t)

;;(setq openai-key "***REMOVED***")

(use-package "google-gemini"
  :init
  (setq google-gemini-key (getenv "GOOGLE_GEMINI_API_KEY"))
  :ensure t)

(provide 'init-ai)
