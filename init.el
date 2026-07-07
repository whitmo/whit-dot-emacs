;; config the emax!

(require 'package)
(require 'seq)

(defvar whit-package-contents-refreshed nil
  "Whether package archive contents have been refreshed this session.")

(defun whit-package-refresh-contents-once ()
  "Refresh package archive contents at most once per Emacs session."
  (unless whit-package-contents-refreshed
    (setq whit-package-contents-refreshed t)
    (package-refresh-contents)))

(defun whit-package-install-if-missing (package)
  "Install PACKAGE unless it is already installed."
  (unless (package-installed-p package)
    (whit-package-refresh-contents-once)
    (condition-case err
        (package-install package)
      (error
       (message "Failed to install package %s: %s" package err)
       (signal (car err) (cdr err))))))

(defun whit-package-vc-install-if-missing (package url)
  "Install PACKAGE from URL unless it is already installed."
  (unless (package-installed-p package)
    (unless (fboundp 'package-vc-install)
      (error "package-vc-install is unavailable; Emacs 29 or newer is required for %s" package))
    (condition-case err
        (package-vc-install url)
      (error
       (message "Failed to install VC package %s from %s: %s" package url err)
       (signal (car err) (cdr err))))))

(defun whit-load-optional-file (file)
  "Load FILE relative to `user-emacs-directory' when it exists."
  (let ((path (expand-file-name file user-emacs-directory)))
    (when (file-exists-p path)
      (load path nil t))))

(defun whit-available-font-family (families)
  "Return the first installed font family from FAMILIES."
  (seq-find (lambda (family)
              (find-font (font-spec :family family)))
            families))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

;; Make installed packages available even when loading this file explicitly
;; (for example via emacs -Q -l init.el or batch verification).
(unless package--initialized
  (package-initialize))

;; Bootstrap `use-package'.
(whit-package-install-if-missing 'use-package)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list
   'load-path (expand-file-name "elpa" user-emacs-directory))
  (require 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;(require 'bind-key)

(defconst whit-lisp
  (expand-file-name "lisp" user-emacs-directory)
  "My configurations.")

(add-to-list 'load-path whit-lisp)

;; Start the Emacs server for emacsclient connections in interactive sessions.
(use-package server
  :ensure nil
  :config
  (unless (or noninteractive (server-running-p))
    (server-start)))

;; Load environment management early
(use-package init-env
  :ensure nil
  :demand t
  :load-path whit-lisp)

(use-package init-whit
  :ensure nil
  :demand t)
(use-package init-customvar
  :ensure nil
  :demand t)
(use-package init-general
  :ensure nil
  :demand t)
(use-package init-ai
  :ensure nil
  :demand t)
(use-package init-music
  :ensure nil
  :demand t)
(use-package init-github
  :ensure nil
  :demand t)

(whit-package-vc-install-if-missing
 'claude-code
 "https://github.com/yuya373/claude-code-emacs")

(use-package init-claude-code
  :ensure nil
  :demand t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aider-args
   '("--sonnet " "--cache-prompts" "--cache-keepalive-pings=3") nil nil "Customized with use-package aider")
 '(case-fold-search t)
 '(current-language-environment "English")
 '(custom-safe-themes
   '("b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a" "0ed3d96a506b89c1029a1ed904b11b5adcebeb2e0c16098c99c0ad95cb124729" default))
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(dired-use-ls-dired nil)
 '(dirtrack-list ("^.*:\\([^$]*\\)\\$" 1))
 '(global-font-lock-mode t nil (font-lock))
 '(grep-command "grep -nri -e ")
 '(grep-find-command
   "find . -not -path \"*~\" -not -path \"*.git\" -not -path \"*pyc\" -type f -print0 | xargs -0 grep -nri -e ")
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(list-directory-verbose-switches "-lh")
 '(package-selected-packages
   '(nix-mode wrap-region tss tide codex-cli codex-theme most-used-words aider shell-here shell-switcher ruff-format flymake-ruff editorconfig quelpa-use-package quelpa gh openai js2-mode json-mode web-mode magit ample-theme leaf use-package))
 '(py-start-run-ipython-shell t)
 '(rst-level-face-base-light 15)
 '(safe-local-variable-values '((encoding . utf-8)))
 '(shell-input-autoexpand 'input)
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function 'shell-switcher-make-shell)
 '(tool-bar-mode nil)
 '(tool-bar-position 'right)
 '(tool-bar-style 'text)
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((copilot copilot-no-mode-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray21" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil"))))
 '(flymake-errline ((((class color)) (:background "DarkRed"))) t))

(let ((font-family (whit-available-font-family '("Source Code Pro" "Menlo" "Monaco"))))
  (when font-family
    (set-face-attribute 'default nil :family font-family :height 140)))

;; Load per-machine overrides last so local choices win. This file is ignored
;; by git and is optional on fresh machines.
(whit-load-optional-file "lisp/init-local.el")
