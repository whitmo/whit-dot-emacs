;;; init-env.el --- Environment variable management

;;; Commentary:
;; Manages environment variables for various tools and services
;; Provides both explicit environment variable management and shell environment importing

;;; Code:
(require 'use-package)

(defvar whit-env-vars
  '(("ANTHROPIC_API_KEY" . "ANTHROPIC_API_KEY")
    ("OPENAI_API_KEY" . "OPENAI_API_KEY")
    ("GOOGLE_GEMINI_API_KEY" . "GOOGLE_GEMINI_API_KEY")
    ("DEEPSEEK_API_KEY" . "DEEPSEEK_API_KEY"))
  "A list of environment variables to preserve/set in Emacs.
Each entry is a cons of (emacs-var . system-env-var).")

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs (mapcar 'car whit-env-vars)))

(defun whit-show-env-vars ()
  "Display current values of managed environment variables."
  (interactive)
  (with-output-to-temp-buffer "Environment Variables"
    (princ "Current Environment Variables:\n\n")
    (dolist (var whit-env-vars)
      (princ (format "%s: %s\n"
                     (car var)
                     (or (getenv (car var)) "Not set"))))))

(provide 'init-env)
;;; init-env.el ends here
