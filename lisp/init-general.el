;;; init-dev --- Initialize development environment

;;; Commentary:

;;; trashcan full of souvenirs

;;; Code:

(require 'use-package)

(setenv "PYMACS_PYTHON" "/Users/whit/dev/elisp/bin/python")
(setenv "VIRTUAL_ENV" "/Users/whit/dev/elisp")



(transient-mark-mode t)
(setq-default transient-mark-mode t)

;; set default mode
(setq major-mode 'org-mode)


;; org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\M-`" 'other-frame)

;; rst
;; (add-hook 'text-mode-hook 'rst-text-mode-bindings)
;; (add-hook 'rst-adjust-hook 'rst-toc-update)
;; (setq rst-mode-lazy nil)


(add-hook 'today-visible-calendar-hook 'calendar-mark-today)




(set-cursor-color "light blue")
(which-function-mode)
(load "tramp")

;; Ruby
(add-to-list 'auto-mode-alist '("\\Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc\\'" . ruby-mode))

;; zip and tar derivatives
(add-to-list 'auto-mode-alist '("\\.bz\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.war\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.deb\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.pybundle\\'" . archive-mode))

;; other extensions
(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls$" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yaml$" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yml$" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yml$" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . html-mode) auto-mode-alist))


(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(setq tramp-default-method "ssh")

(global-font-lock-mode t)
(global-set-key [f4] 'shell)
(global-set-key [f5] 'rename-buffer)
(global-set-key [f6] 'magit-status)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-f" 'grep-find)

(setq frame-background-mode 'dark)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; avoid hangs launching warning box
(setq flymake-gui-warnings-enabled nil)



(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-general)
