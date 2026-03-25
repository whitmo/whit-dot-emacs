;;; init-ai --- AI Helpers

;;; Commentary:
;;
(require 'use-package)



(use-package s :ensure t)




;;(setq openai-key getenv("OPENAI_API_KEY"))

(use-package "google-gemini"
  :init
  (setq google-gemini-key (getenv "GOOGLE_GEMINI_API_KEY"))
  :ensure t)

(provide 'init-ai)
