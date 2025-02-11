;;; init-aider --- Aider AI pair programming support

;;; Commentary:
;; Configuration for aider.el integration


;; Declare custom variables
(defcustom aider-args nil
  "Arguments to pass to aider command line tool."
  :type '(repeat string)
  :group 'aider)


(package-vc-install '(aider :url "https://github.com/tninja/aider.el"))


(use-package aider
  :custom
  (aider-args '("--cache-prompts" "--model" "anthropic/claude-3-5-sonnet-20241022"))
  :bind
  ("C-c a" . aider-transient-menu))

(provide 'init-aider)
