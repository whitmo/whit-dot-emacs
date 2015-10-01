;;; init --- Emacs initialization

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(defconst whit-lisp
  (expand-file-name "lisp" user-emacs-directory)
  "My configurations.")

(add-to-list 'load-path whit-lisp)

(setq debug-on-error t)

(use-package init-shell  :demand t)
(use-package init-general  :demand t)
(use-package init-ui     :demand t)
(use-package init-dev    :demand t)
(use-package init-python    :demand t)
(use-package init-helm   :demand t)
(use-package init-git    :demand t)
;; (use-package init-ace    :demand t)
(use-package init-text   :demand t)
(use-package init-emacs  :demand t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(dired-use-ls-dired nil)
 '(dirtrack-list ("^.*:\\([^$]*\\)\\$" 1))
 '(dvc-tips-enabled nil)
 '(exec-path
   (quote
    (/home/whit/.gvm/pkgsets/go1.2.1/global/bin "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin" "/Users/whit/bin/")))
 '(flycheck-highlighting-mode (quote sexps))
 '(global-font-lock-mode t nil (font-lock))
 '(grep-command "grep -nri -e ")
 '(grep-find-command
   "find . -not -path \"*svn*\" -not -path \"*pyc\" -type f -print0 | xargs -0 grep -in -e ")
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indent turn-on-haskell-indentation)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(list-directory-verbose-switches "-lh")
 '(magit-git-executable "/usr/bin/git")
 '(py-start-run-ipython-shell t)
 '(remote-shell-program "/usr/bin/ssh")
 '(rst-level-face-base-light 15)
 '(shell-input-autoexpand (quote input))
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-shell))
 '(tool-bar-mode nil)
 '(tool-bar-position (quote right))
 '(virtualenv-root "/Users/whit/.venv"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray21" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(flymake-errline ((((class color)) (:background "DarkRed")))))


(provide 'init)
;;; init ends here
