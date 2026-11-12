;; config the emax!

(defconst whit-lisp
  (expand-file-name "lisp" user-emacs-directory)
  "My configurations.")

(add-to-list 'load-path whit-lisp)
(setq load-prefer-newer t)

(require 'init-packages)

;; Load environment management early
(use-package init-env
  :ensure nil
  :demand t
  :load-path whit-lisp)

(use-package init-whit :ensure nil :demand t)
(use-package init-customvar :ensure nil :demand t)
(use-package init-general :ensure nil :demand t)
(use-package init-agent-bridge :ensure nil :demand t)
(use-package init-ai :ensure nil :demand t)
(use-package init-music :ensure nil :demand t)
(use-package init-github :ensure nil :demand t)
(use-package init-pi :ensure nil :demand t)
(use-package init-yaml :ensure nil :demand t)


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(case-fold-search t)
;;  '(current-language-environment "English")
;;  '(custom-safe-themes
;;    '("b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a" "0ed3d96a506b89c1029a1ed904b11b5adcebeb2e0c16098c99c0ad95cb124729" default))
;;  '(dired-listing-switches "-alh")
;;  '(dired-recursive-copies 'always)
;;  '(dired-recursive-deletes 'top)
;;  '(dired-use-ls-dired nil)
;;  '(dirtrack-list ("^.*:\\([^$]*\\)\\$" 1))
;;  '(global-font-lock-mode t nil (font-lock))
;;  '(grep-command "grep -nri -e ")
;;  '(grep-find-command
;;    "find . -not -path \"*.git\" -not -path \"*pyc\" -type f -print0 | xargs -0 grep -in -e ")
;;  '(inhibit-startup-screen t)
;;  '(js2-basic-offset 4)
;;  '(list-directory-verbose-switches "-lh")
;;  '(magit-git-executable "/usr/bin/git")
;;  '(py-start-run-ipython-shell t)
;;  '(remote-shell-program "/usr/bin/ssh")
;;  '(rst-level-face-base-light 15)
;;  '(safe-local-variable-values '((encoding . utf-8)))
;;  '(shell-input-autoexpand 'input)
;;  '(shell-switcher-mode t)
;;  '(shell-switcher-new-shell-function 'shell-switcher-make-shell)
;;  '(tool-bar-mode nil)
;;  '(tool-bar-position 'right)
;;  '(tool-bar-style 'text)
;;  '(virtualenv-root "/Users/whit/.venv"))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "gray21" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
;;  '(flymake-errline ((((class color)) (:background "DarkRed"))) t))
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
 '(magit-git-executable "/usr/bin/git")
 '(nil nil t)
 '(package-selected-packages
   '(pilish flymake-yamllint flycheck-yamllint yaml yaml-imenu yaml-mode envrc dockerfile-mode pg pi-coding-agent claude-code claude-code-emacs codespaces aider shell-here shell-switcher ruff-format flymake-ruff editorconfig quelpa-use-package quelpa gh openai js2-mode json-mode web-mode magit ample-theme leaf use-package))
 '(package-vc-selected-packages
   '((claude-code-emacs :vc-backend Git :url "https://github.com/yuya373/claude-code-emacs")))
 '(py-start-run-ipython-shell t)
 '(remote-shell-program "/usr/bin/ssh")
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
 '(default ((t (:inherit nil :stipple nil :background "gray21" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(flymake-errline ((((class color)) (:background "DarkRed"))) t))
