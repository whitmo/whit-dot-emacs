;;; init-aider --- Aider AI pair programming support

;;; Commentary:
;; Configuration for aider.el integration

(require 'use-package)

;; Declare custom variables
(defcustom aider-args nil
  "Arguments to pass to aider command line tool."
  :type '(repeat string)
  :group 'aider)

(use-package aider
  :quelpa (aider :fetcher github :repo "tninja/aider.el")
  :config
  ;; Use claude-3-5-sonnet as it performs best in aider benchmark
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
  ;; Set key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-aider)
