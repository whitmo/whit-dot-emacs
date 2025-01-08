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
    ("GITHUB_TOKEN" . "GITHUB_TOKEN"))
  "Alist of environment variables to preserve/set in Emacs.
Each entry is a cons of (emacs-var . system-env-var).")

(defun whit-load-env-vars ()
  "Load environment variables from system to Emacs environment."
  (dolist (var whit-env-vars)
    (let ((emacs-var (car var))
          (sys-var (cdr var)))
      (when-let ((value (getenv sys-var)))
        (setenv emacs-var value)))))

(defun whit-show-env-vars ()
  "Display current values of managed environment variables."
  (interactive)
  (with-output-to-temp-buffer "*Environment Variables*"
    (princ "Current Environment Variables:\n\n")
    (dolist (var whit-env-vars)
      (princ (format "%s: %s\n" 
                     (car var) 
                     (or (getenv (car var)) "Not set"))))))

(defun whit-load-shell-env ()
  "Load environment from the user's shell."
  (interactive)
  (let ((shell-env-string
         (shell-command-to-string
          "$SHELL -i -c 'printenv'"))
        (shell-env-vars nil))
    (dolist (line (split-string shell-env-string "\n" t))
      (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
        (let ((key (match-string 1 line))
              (value (match-string 2 line)))
          (setenv key value))))))

;; Load variables when the module is loaded
(whit-load-env-vars)

;; Load shell environment if running as GUI Emacs
(when (display-graphic-p)
  (whit-load-shell-env))

(provide 'init-env)
;;; init-env.el ends here
