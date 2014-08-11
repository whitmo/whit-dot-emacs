;;(setq mac-command-modifier 'meta) ;; aquamacs only
(setq load-path  (cons (expand-file-name "~/.emacs.d/local") load-path))


;; packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


(add-to-list 'load-path "/Users/whit/.emacs.d/")
;;(add-to-list 'load-path "/usr/local/share/git-core/contrib/emacs/")

(setenv "PYMACS_PYTHON" "/Users/whit/dev/elisp/bin/python")
(setenv "VIRTUAL_ENV" "/Users/whit/dev/elisp")

;;(load "graphviz-dot-mode.el")
;;(load "tail.el")

;; yasnippet
;;(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;;(require 'io-mode)
;;(require 'io-mode-inf)
(require 'dockerfile-mode)
(require 'python-mode)
(require 'css-mode)
(require 'json)
(require 'rst)
(require 'sgml-mode)
(require 'uniquify)
(require 'yaml-mode)
(require 'shell-switcher)
(require 'time)
(require 'time-date)
(require 'dirtree)
;;(require 'ipython)

;; "date \"+%Y-%m-%d %H:%M:%S\""

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(global-set-key "\M-/" 'ac-start)
(setq ac-auto-start 2)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)

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
(add-hook 'text-mode-hook 'rst-text-mode-bindings)
(add-hook 'rst-adjust-hook 'rst-toc-update)
(setq rst-mode-lazy nil)


(add-hook 'today-visible-calendar-hook 'calendar-mark-today)




(set-cursor-color "light blue")
(which-function-mode)
(setq load-path  (cons (expand-file-name "~/.emacs.d/") load-path))
(load "tramp")

;;pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "/home/whit/dev/elisp/lib/python2.7/site-packages"))

;; javascript mode
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))



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

;;(setq auto-mode-alist (cons '("\\.jst$" . django-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;;;;; PYTHON ;;;;;;
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pjs\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jtmpl\\'" . html-mode)) ;; jinja
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode)) ;; open egg

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'pyrex-mode "pyrex-mode" "Pyrex editing mode." t)
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

(defvar gud-pdb-marker-regexp "^> \\([-a-zA-Z0-9_/.:\\ ()]*\\|<string>\\)(\\([0-9]+\\))\\([a-zA-Z0-9_]*\\|\\?\\)()\\(->[^\n]*\\)?\n")

(defadvice pdb (before gud-query-cmdline activate)
    \"Provide a better default command line when called interactively.\"
    (interactive
     (list (gud-query-cmdline /usr/lib/python2.7/'
			      (file-name-nondirectory buffer-file-name)))))


(defvar py-flake8-history nil
  "Used by flake8, resp. py-flake8-command.

Default is nil. ")

(setq tramp-default-method "ssh")

(global-font-lock-mode t)
(global-set-key [f4] 'shell)
(global-set-key [f5] 'rename-buffer)
(global-set-key [f6] 'magit-status)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'grep-find)

(setq frame-background-mode 'dark)

;;(setq auto-mode-alist
;;     (cons '("\\.txt$" . nil) auto-mode-alist))



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


(setq uniquify-buffer-name-style 'post-forward)
(put 'scroll-left 'disabled nil)

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
(delete '(" *\\(\\[javac\\]\\)? *\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)" 2 4 nil 5)
        flymake-err-line-patterns)


(delete '(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(        \n]+\\):\\([0-9]+\\):[  \n]*\\(.+\\)" 2 4 nil 5)
        flymake-err-line-patterns)

;;(add-hook 'python-mode-hook 'whitespace-mode)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; avoid hangs launching warning box
(setq flymake-gui-warnings-enabled nil)

;; shortcut for directory resync
(add-hook 'shell-mode-hook
  (lambda()
    (local-set-key (kbd "M-]") 'shell-resync-dirs)))

;; slime
;; (add-to-list 'load-path "/opt/local/share/emacs/site-lisp/slime")
;; (require 'slime-autoloads)
;; (setq slime-lisp-implementations
;;      `((sbcl ("/opt/local/bin/sbcl"))
;;        (abcl ("/opt/local/bin/abcl"))
;;        (clisp ("/opt/local/bin/clisp"))))
;; (add-hook 'lisp-mode-hook
;;            (lambda ()
;;              (cond ((not (featurep 'slime))
;;                     (require 'slime)
;;                     (normal-mode)))))

;; (eval-after-load "slime"
;;    '(slime-setup '(slime-fancy slime-banner)))

(defun sort-imports ()
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          (string-lessp-case-insensitive
           (lambda (a b) (string-lessp (downcase a) (downcase b))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             string-lessp-case-insensitive))
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (mapconcat
               (lambda (import) (concat "    " import ",\n"))
               (remove "" sorted-imports) ""))
      (insert "    )")
      )))

;; ipython
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
 '(dirtrack-list ("^.*:\\([^$]*\\)\\$" 1))
 '(dvc-tips-enabled nil)
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin" "/Users/whit/dev/elisp/bin/")))
 '(flycheck-highlighting-mode (quote sexps))
 '(global-font-lock-mode t nil (font-lock))
 '(grep-command "grep -nri -e ")
 '(grep-find-command "find . -not -path \"*svn*\" -not -path \"*pyc\" -type f -print0 | xargs -0 grep -in -e ")
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-indentation)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(list-directory-verbose-switches "-lh")
 '(magit-git-executable "/usr/bin/git")
 '(remote-shell-program "/usr/bin/ssh")
 '(rst-level-face-base-light 15)
 '(safe-local-variable-values (quote ((todo-categories "HOME") (todo-categories "WAT"))))
 '(shell-input-autoexpand (quote input))
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-shell))
 '(tool-bar-mode nil)
 '(tool-bar-position (quote right))
 '(virtualenv-root "~/dev"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "gray5" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :family "Ubuntu Mono" :foundry "unknown" :height 157 :width normal))))
 '(flymake-errline ((((class color)) (:background "DarkRed")))))
;;'(default ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal)))))


(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -c 'import sys,json; data=json.loads(sys.stdin.read()); print json.dumps(data,sort_keys=True,indent=4).decode(\"unicode_escape\").encode(\"utf8\",\"replace\")'" (current-buffer) t)))

;;(define-key json-mode-map (kbd "C-c C-f") 'beautify-json)
