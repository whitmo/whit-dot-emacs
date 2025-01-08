;;; init-ai --- AI Helpers

;;; Commentary:
;;
;;(require 'use-package)

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
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")
		   )
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

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

(use-package "aider"
  :ensure t
  :quelpa (aider :fetcher github :repo "tninja/aider.el")
  ;;:straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :init
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  :config
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "gpt-4o-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu)
  )




(provide 'init-ai)
