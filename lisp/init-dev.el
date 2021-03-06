;;; init-dev --- Initialize development environment

;;; Commentary:

;;; dev env

;;; Code:

(require 'use-package)
(require 'init-shell)
(require 'init-whit)
(require 'hydra)

(defvar eir-key "C-<return>"
  "Eval-in-REPL key.")

(use-package edit-server-htmlize
  :ensure t
  :config
  (progn
    (setq edit-server-new-frame nil)
    (edit-server-start)))

(use-package yaml-mode
  :ensure t)

(use-package css-mode
  :ensure t)

(use-package shell-switcher
  :ensure t)

(use-package dirtree
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package cask
  :ensure t)

(use-package prog-mode
  :demand t
  :diminish auto-fill-function
  :config
  (add-hook 'prog-mode-hook 'turn-on-auto-fill))

(use-package newcomment
  :config
  (setq comment-auto-fill-only-comments t))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package eshell
  :config
  (bind-keys :map eshell-mode-map
             ("C-:" . helm-esh-pcomplete)))

(use-package ess
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package eval-in-repl
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :demand t)



;; TODO: could be neat to make a hydra for sp motions.
(use-package smartparens-config
  :ensure smartparens
  :demand t
  :diminish (smartparens-mode)
  :init
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-ignore-modes-list '(minibuffer-inactive-mode
                               shell-mode))
  :config
  (progn
    (bind-keys :map smartparens-mode-map
               ("C-)" . sp-forward-slurp-sexp)
               ("C-(" . sp-backward-barf-sexp)
               ("C-M-)" . sp-backward-slurp-sexp)
               ("C-M-(" . sp-backward-barf-sexp))
    (sp-use-paredit-bindings)
    (show-smartparens-global-mode)
    (smartparens-global-strict-mode t)))

(use-package json-rpc
  :ensure t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (progn
    (use-package js :demand t)
    (defun whit-json-mode-hook ()
      (interactive)
      (setq js-indent-level 2))

    (add-hook 'json-mode-hook 'whit-json-mode-hook)))

(use-package anaconda-mode
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package cider
  :ensure t
  :defer t)

(use-package ielm
  :init
  (progn
    (require 'info)
    (require 'lisp-mode)
    (dolist (map (list emacs-lisp-mode-map
                       lisp-interaction-mode-map
                       Info-mode-map))
      (bind-keys :map map
                 (eir-key . eir-eval-in-ielm)))))

(use-package lisp-mode
  :config
  (bind-keys :map emacs-lisp-mode-map
             ("M-." . find-function-at-point)))

(use-package eldoc
  :demand t
  :diminish eldoc-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  python-mode-hook
                  ))
    (add-hook hook 'eldoc-mode)))

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode 1)
  :config
  (progn
    (dolist (package '(company-go
                       company-inf-ruby
                       company-tern
                       company-math
                       company-restclient))
      (use-package package
        :ensure t
        :demand t
        :init (add-to-list 'company-backends package)))
    (setq company-auto-complete t
          company-echo-delay 5
          company-tooltip-minimum-width 30
          company-idle-delay nil)))

(use-package inf-ruby
  :ensure t
  :defer t
  :config
  (progn
    (setq ruby-deep-indent-paren nil
          ruby-deep-arglist nil)))

(use-package js2-mode
  :ensure t
  :defer 1
  :mode ("\\.(json|js)$" . js-mode)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))

(use-package restclient
  :ensure t
  :defer t)

(use-package minitest
  :ensure t
  :defer t)

(use-package scala-mode2
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :config
  (progn
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c f"))
    (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (global-flycheck-mode)))

;; (use-package rubocop
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (require 'flycheck)
;;     (add-hook 'ruby-mode-hook 'rubocop-mode)
;;     (eval-after-load 'flycheck
;;       (flycheck-add-next-checker 'chef-foodcritic 'ruby-rubocop))))

(use-package mmm-mode
  :ensure t
  :commands mmm-mode
  :config
  (progn
    (setq mmm-global-mode 'buffers-with-submode-classes
          mmm-submode-decoration-level 0)
    (use-package mmm-auto)))

(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :init (setq csv-separators '("," ";" "|" " "))
  :config (use-package csv-nav :ensure t))



(use-package ein
  :ensure t)

(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  :mode (("Gemfile\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.irbrc\\'" . ruby-mode)
         ("\\.pryrc\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.rxml\\'" . ruby-mode))
  :init
  (setq ruby-use-encoding-map nil)
  :config
  (progn
    (dolist (package '(ruby-hash-syntax
                       ruby-compilation
                       bundler))
      (use-package package :ensure t))
    (use-package inf-ruby
      :ensure t
      :config
      (setq inf-ruby-default-implementation "pry"))
    (bind-keys :map ruby-mode-map
               ("RET" . reindent-then-newline-and-indent)
               ("TAB" . indent-for-tab-command)
               (eir-key . eir-eval-in-ruby))

    (add-hook 'ruby-mode-hook 'subword-mode)

    (use-package robe
      :ensure t
      :init
      (progn
        (with-eval-after-load 'company
          (push 'company-robe company-backends))
        (add-hook 'ruby-mode-hook 'robe-mode)))

    (use-package yari
      :ensure t
      :init (defalias 'ri 'yari))

    (use-package rinari
      :ensure t
      :init (global-rinari-mode))

    (use-package rspec-mode
      :ensure t
      :config
      (add-hook 'ruby-mode-hook (lambda () (rspec-mode 1))))

    ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
    ;; prog-mode: we run the latter's hooks anyway in that case.
    (add-hook 'ruby-mode-hook
              (lambda ()
                (unless (derived-mode-p 'prog-mode)
                  (run-hooks 'prog-mode-hook))))))

(use-package pp
  :demand t
  :config
  (progn
    (global-set-key [remap eval-expression] 'pp-eval-expression)
    (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)))

;; TODO: essh https://github.com/svend/emacs-essh

(use-package compile
  :demand t
  :config
  (setq compilation-scroll-output t
        compilation-read-command nil
        compilation-ask-about-save nil
        compilation-auto-jump-to-first-error nil
        compilation-save-buffers-predicate nil))

(use-package vc-hooks
  :demand t
  :config
  (bind-keys :map vc-prefix-map ("=" . ediff-revision)))

(use-package ediff
  :demand t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package multiple-cursors
  :ensure t
  :demand t
  :init
  (bind-key
   "C-c c"
   (defhydra hydra-multiple-cursors () "multiple-curors"
     ("e" mc/edit-lines "edit")
     ("n" mc/mark-next-like-this "next")
     ("p" mc/mark-previous-like-this "previous")
     ("a" mc/mark-all-like-this "all"))))

(use-package know-your-http-well
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (progn
    ;; (add-to-list 'aggressive-indent-excluded-modes ...) as needed.
    (global-aggressive-indent-mode 1)))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -c 'import sys,json; data=json.loads(sys.stdin.read()); print json.dumps(data,sort_keys=True,indent=4).decode(\"unicode_escape\").encode(\"utf8\",\"replace\")'" (current-buffer) t)))

;;(define-key json-mode-map (kbd "C-c C-f") 'beautify-json)

(provide 'init-dev)
;;; init-dev ends here
