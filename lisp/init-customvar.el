(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a" "0ed3d96a506b89c1029a1ed904b11b5adcebeb2e0c16098c99c0ad95cb124729" default))
 '(package-selected-packages
   '(js2-mode json-mode web-mode magit ample-theme leaf use-package))
 '(tool-bar-style 'text)
  '(case-fold-search t)
 '(current-language-environment "English")
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(dired-use-ls-dired nil)
 '(dirtrack-list ("^.*:\\([^$]*\\)\\$" 1))
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(grep-command "grep -nri -e ")
 '(grep-find-command
   "find . -not -path \"*.git\" -not -path \"*pyc\" -type f -print0 | xargs -0 grep -in -e ")
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
 '(virtualenv-root "/Users/whit/.venv")

 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray21" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 )

(provide 'init-customvar)
