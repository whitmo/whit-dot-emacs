;;; init-aider --- Aider AI pair programming support

;;; Commentary:
;; Configuration for aider.el integration

;;(require 'use-package)
(require 'quelpa-use-package)
;; Declare custom variables
(defcustom aider-args nil
  "Arguments to pass to aider command line tool."
  :type '(repeat string)
  :group 'aider)

(use-package aider
  :quelpa ((aider :fetcher github 
                  :repo "tninja/aider.el"
                  :files ("*.el"))
           :upgrade t)
  :custom
  (aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  :config
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
  :bind
  ("C-c a" . aider-transient-menu))

(provide 'init-aider)
