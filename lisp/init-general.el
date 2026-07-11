;;; init-dev --- Initialize development environment -*- lexical-binding: t; -*-

;;; Commentary:

;;; trashcan full of souvenirs

(require 'use-package)

(dolist (binding
         '(("<f4>" . shell-here)
           ("<f5>" . rename-buffer)
           ("<f6>" . magit-status)
           ("\C-g" . goto-line)
           ("\C-f" . grep-find)))
  (global-set-key (kbd (car binding)) (cdr binding)))

(global-font-lock-mode t)
(transient-mark-mode t)
(setq-default transient-mark-mode t)

(use-package lua-mode
  :ensure t
  :defer t)

(use-package flymake
  :ensure t
  :hook (lua-mode . flymake-mode))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package server
  :ensure nil
  :config
  ;; Make emacsclient available for editor handoff workflows.  This is not MCP
  ;; by itself, but it is the right local control plane for an MCP bridge.
  (unless (or noninteractive (server-running-p))
    (server-start)))

;; python
(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode))

(fset 'break
      "import pdb;pdb.set_trace()\C-a\C-i")

(fset 'postmortem
      "import pdb, sys;pdb.post_mortem(sys.exc_info()[2])\C-a\C-i")

(fset 'review
      "#@@ DWM: ")

(use-package python
  :ensure t
  :bind (:map python-mode-map
              ("M-p M-p" . break)
              ("M-p M-m" . postmortem)
              ("M-p M-r" . review))
  :hook ((python-mode . flymake-mode)
         (python-mode . flymake-ruff-load)
         (python-mode . (lambda ()
                          (add-hook 'comint-output-filter-functions
                                    #'python-pdbtrack-comint-output-filter-function
                                    nil 'local)))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package projectile
  :init
  (setq projectile-completion-system 'default)
  :bind-keymap ("C-c P" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq js2-basic-offset 4))

(use-package json-mode
  :mode "\\.json\\'")

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.tsx\\'"
         "\\.jsx\\'"
         "\\.vue\\'")
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown"))

(use-package typst-mode
  :mode "\\.typ\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . flymake-mode)
  :init
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-playground
  :after rust-mode)

(use-package go-mode
  :hook ((go-mode . flymake-mode)
         (before-save . gofmt-before-save)))

(use-package vterm
  :commands vterm
  :bind ("C-c v" . vterm))

(load "tramp")
(setq tramp-default-method "ssh"
      remote-shell-program (or (executable-find "ssh") "/usr/bin/ssh"))

(defun my-shell-mode-hook ()
  "Enable pdb tracking in shell buffers."
  (require 'python)
  (when (fboundp 'python-pdbtrack-comint-output-filter-function)
    (add-hook 'comint-output-filter-functions
              #'python-pdbtrack-comint-output-filter-function
              nil 'local)))

(add-hook 'shell-mode-hook #'my-shell-mode-hook)

(use-package shell-switcher
  :ensure t)

(use-package shell-here
  :ensure t)

(provide 'init-general)
;;; init-general.el ends here
