;;; init-env.el --- Environment variable management

;;; Commentary:
;; Manages environment variables for various tools and services
;; Provides both explicit environment variable management and shell environment importing

;;; Code:
(require 'use-package)

;; GUI Emacs is often started outside a Nix shell.  Put common Nix profile
;; locations on Emacs's path before envrc evaluates a project .envrc; this
;; lets a devenv-backed project expose its own tools (including `pi').
(defun whit-add-nix-profile-paths ()
  "Make common Nix profile binaries available to Emacs and child processes."
  (dolist (directory
           (list (expand-file-name "~/.nix-profile/bin")
                 (expand-file-name "~/.local/state/nix/profile/bin")
                 "/run/current-system/sw/bin"))
    (when (file-directory-p directory)
      (add-to-list 'exec-path directory)
      (unless (member directory (parse-colon-path (or (getenv "PATH") "")))
        (setenv "PATH"
                (concat directory path-separator (or (getenv "PATH") "")))))))

(whit-add-nix-profile-paths)

(defvar whit-env-vars
  '(("ANTHROPIC_API_KEY" . "ANTHROPIC_API_KEY")
    ("GOOGLE_GEMINI_API_KEY" . "GOOGLE_GEMINI_API_KEY")
    ("DEEPSEEK_API_KEY" . "DEEPSEEK_API_KEY"))
  "A list of environment variables to preserve/set in Emacs.
Each entry is a cons of (emacs-var . system-env-var).")

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-variables
        (delete-dups (append exec-path-from-shell-variables
                             (mapcar #'car whit-env-vars))))
  (unless noninteractive
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs (mapcar #'car whit-env-vars))))

(use-package envrc
  :ensure t
  :demand t
  :config
  (unless noninteractive
    (envrc-global-mode)))

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
