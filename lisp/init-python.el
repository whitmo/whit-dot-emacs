;;; init-dev --- Initialize development environment

;;; Commentary:

;;; python receptacle

;;; Code:

;;pymacs
(require 'use-package)
(require 'init-dev)

(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-mode))
  :config
  (defvar gud-pdb-marker-regexp "^> \\([-a-zA-Z0-9_/.:\\ ()]*\\|<string>\\)(\\([0-9]+\\))\\([a-zA-Z0-9_]*\\|\\?\\)()\\(->[^\n]*\\)?\n")

  (defadvice pdb (before gud-query-cmdline activate)
    \"Provide a better default command line when called interactively.\"
    (interactive
     (list (gud-query-cmdline /usr/lib/python2.7/'
                              (file-name-nondirectory buffer-file-name)))))


  (defvar py-flake8-history nil
    "Used by flake8, resp. py-flake8-command.

Default is nil. ")

  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  ;; from http://www.jwz.org/doc/tabs-vs-spaces.html
  (defun untabify-buffer ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max))))
    nil)

  ;; never insert tabs ... always spaces
  (setq-default indent-tabs-mode nil)

                                        ; add more hooks hereno
  (add-hook 'c-mode-hook
            '(lambda ()
               (make-local-variable 'write-contents-hooks)
               (add-hook 'write-contents-hooks 'untabify-buffer)))

  (put 'scroll-left 'disabled nil)

  (add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode));; open egg
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist))

  (autoload 'python-mode "python-mode" "Python editing mode." t)
  ;;(autoload 'pyrex-mode "pyrex-mode" "Pyrex editing mode." t)
  (autoload 'doctest-mode "doctest-mode" "doctest editing mode." t)
  (fset 'break
        "import pdb;pdb.set_trace()\C-a\C-i")

  (fset 'postmortem
        "import pdb, sys;pdb.post_mortem(sys.exc_info()[2])\C-a\C-i")

  (fset 'review
        "#@@ DWM: ")

  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))
  (add-hook 'python-mode-hook
            '(lambda ()
               (local-set-key  [(meta ?p) (meta ?p)] 'break)
               (local-set-key  [(meta ?p) (meta ?m)] 'postmortem)
               (local-set-key  [(meta ?p) (meta ?r)] 'review)
               ))
  (progn
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
    (bind-keys :map python-mode-map
               (eir-key . eir-eval-in-python))

    (use-package elpy
      :ensure t
      :config
      (progn
        ;; Remove elpy company configuration; I like my own.
        (setq elpy-modules
              (remove 'elpy-module-company elpy-modules))
        (elpy-enable)
        ;;(elpy-use-ipython)
        )
      )
    (use-package pyenv-mode
      :ensure t)))

(use-package pymacs
  :ensure t
  :config
  (autoload 'pymacs-load "pymacs" nil t)
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (eval-after-load "pymacs"
    '(add-to-list 'pymacs-load-path "/home/whit/dev/elisp/lib/python2.7/site-packages"))
  )




;;;;; PYTHON ;;;;;;










;;Run pyflakes with flymake.
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)


;; Work around bug in flymake that causes Emacs to hang when you open a
;; docstring.
;; (delete '(" *\\(\\[javac\\]\\)? *\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)" 2 4 nil 5)
;;         flymake-err-line-patterns)


;; (delete '(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(        \n]+\\):\\([0-9]+\\):[  \n]*\\(.+\\)" 2 4 nil 5)
;;         flymake-err-line-patterns)

;;(add-hook 'python-mode-hook 'whitespace-mode)


(provide 'init-python)
;;; init-python ends here
