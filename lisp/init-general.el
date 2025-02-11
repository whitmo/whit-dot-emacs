;;; init-dev --- Initialize development environment

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; (defun pdb-track-hook ()
;;   (add-hook
;;    'comint-output-filter-functions
;;    'python-pdbtrack-comint-output-filter-function t))

;; old
;; (use-package python
;;   :ensure t
;;   :bind (([(meta ?p) (meta ?p)] 'break)
;; 	 ([(meta ?p) (meta ?m)] 'postmortem)
;; 	 ([(meta ?p) (meta ?r)] 'review))
;;   :hook ((fly-check-mode . python-mode)
;; 	 (comint-output-filter-functions . python-pdbtrack-comint-output-filter-function t))
;; )

(use-package python
  :ensure t
  :bind (([M-p M-p] . break)
         ([M-p M-m] . postmortem)
         ([M-p M-r] . review))

  :hook (fly-check-mode . python-mode)
  :hook (comint-output-filter-functions . python-pdbtrack-comint-output-filter-function)
)

(use-package quelpa
  :ensure t
  :custom
  (quelpa-checkout-melpa-p nil)
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :init
  (require 'quelpa-use-package))
(use-package editorconfig :ensure t)


(load "tramp")
(setq tramp-default-method "ssh")

(defun my-shell-mode-hook ()
  (add-hook
   'comint-output-filter-functions
   'python-pdbtrack-comint-output-filter-function t))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(use-package "shell-switcher"
  :ensure t)

(use-package "shell-here"
  :ensure t)

(provide 'init-general)
