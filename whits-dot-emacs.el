(setq mac-command-modifier 'meta) ;; aquamacs only

(add-to-list 'load-path "/Users/whit/.emacs.d/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(setenv "PYMACS_PYTHON" "/Users/whit/dev/elisp/bin/python")
(setenv "VIRTUAL_ENV" "/Users/whit/dev/elisp")

(load-file "/usr/local/share/emacs/site-lisp/dvc/dvc-load.el")
(load-file "/opt/local/share/doc/git-core/contrib/emacs/git.el")

;; mako
;;(load "/Users/whit/.emacs.d/mmm-mako.el")
;;(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
;;(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

(require 'magit)
(require 'magit-svn)

;; set default mode
(require 'rst)
(setq major-mode 'rst-mode)
(setq initial-major-mode 'rst-mode)
(add-hook 'text-mode-hook 'rst-text-mode-bindings)
(add-hook 'rst-adjust-hook 'rst-toc-update)
(setq rst-mode-lazy nil)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

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
 '(exec-path (quote ("/opt/local/bin" "/sw/bin" "/sw/sbin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/X11R6/bin" "/sw/lib/emacs/22.0.50-carbon/i386-apple-darwin8" "/usr/local/bin" "/Users/whit/dev/elisp/bin/")))
 '(global-font-lock-mode t nil (font-lock))
 '(grep-command "grep -nri -e ")
 '(grep-find-command "find . -not -path \"*svn*\" -not -path \"*pyc\" -type f -print0 | xargs -0 grep -in -e ")
 '(initial-scratch-message ";; scratch ;;

")
 '(js2-basic-offset 4)
 '(list-directory-verbose-switches "-lh")
 '(magit-git-executable "/usr/local/bin/git")
 '(remote-shell-program "/usr/bin/ssh")
 '(rst-level-face-base-light 15)
 '(safe-local-variable-values (quote ((todo-categories "Wedding" "NYC" "TestLayers" "Training" "Wicked" "Todo" "Todo" "Today" "Todo"))))
 '(shell-input-autoexpand (quote input))
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "gray5" :foreground "pale goldenrod" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal))))
 '(flymake-errline ((((class color)) (:background "DarkRed")))))
;;'(default ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal)))))

;; diary mode
(setq view-diary-entries-initially t
       mark-diary-entries-in-calendar t
       number-of-diary-entries 7)
 (add-hook 'diary-display-hook 'fancy-diary-display)
 (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;;(load "jinja.el")
(load "django-mode.el")
(load "gist.el")
(load "graphviz-dot-mode.el")
(load "json.el")

(setq-default transient-mark-mode t)
(set-cursor-color "light blue") 
(which-func-mode)
(setq load-path  (cons (expand-file-name "~/.emacs.d/") load-path))
(load "tramp")


;;pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "/Users/whit/dev/elisp/lib/python2.6/site-packages"))

;; autocomplete

;; (require 'auto-complete)
;; (global-auto-complete-mode t)

;; (when (require 'auto-complete nil t)
;;   ;;(require 'auto-complete-yasnippet)
;;   (require 'auto-complete-python)
;;   (require 'auto-complete-css) 
;;   (require 'auto-complete-cpp)  
;;   (require 'auto-complete-emacs-lisp)  
;;   ;(require 'auto-complete-semantic)  
;;   ;(require 'auto-complete-gtags)

;;   (global-auto-complete-mode t)
;;   (setq ac-auto-start 3)
;;   (setq ac-dwim t)
;;   (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-files-in-current-dir ac-source-symbols)))
  ;(set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer ac-source-files-in-current-dir ac-source-symbols)))

;; Tag Support
;; (defun djcb-gtags-create-or-update ()
;;   "create or update the gnu global tag file"
;;   (interactive)
;;   (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
;;     (let ((olddir default-directory)
;;           (topdir (read-directory-name  
;;                     "gtags: top of source tree:" default-directory)))
;;       (cd topdir)
;;       (shell-command "gtags && echo 'created tagfile'")
;;       (cd olddir)) ; restore   
;;     ;;  tagfile already exists; update it
;;     (shell-command "global -u && echo 'updated tagfile'")))

;; (add-hook 'gtags-mode-hook 
;;   (lambda()
;;     (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
;;     (local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     ;;(require 'gtags)
;;     ;;(gtags-mode t)
;;     (djcb-gtags-create-or-update)))


;; javascript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; chuck mode
;; (setq auto-mode-alist
;;    (append '(("\.ck$" . chuck-mode))
;;            auto-mode-alist))
;; (autoload 'chuck-mode "chuck" "Chuck-mode" t)

;;;;; PYTHON ;;;;;;

(add-to-list 'auto-mode-alist '("\\.pxd\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . pyrex-mode))
(add-to-list 'auto-mode-alist '("\\.pjs\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jtmpl\\'" . html-mode)) ;; jinja
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode)) ;; open egg
(add-to-list 'auto-mode-alist '("\\.bz\\'" . archive-mode)) 
(add-to-list 'auto-mode-alist '("\\.jar\\'" . archive-mode)) 
(add-to-list 'auto-mode-alist '("\\.war\\'" . archive-mode)) 
(add-to-list 'auto-mode-alist '("\\.pybundle\\'" . archive-mode)) 

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

(add-hook 'python-mode-hook 
		  '(lambda ()
		     (local-set-key  [(meta ?p) (meta ?p)] 'break)
		     (local-set-key  [(meta ?p) (meta ?m)] 'postmortem)
		     (local-set-key  [(meta ?p) (meta ?r)] 'review)
))

(defvar gud-pdb-marker-regexp "^> \\([-a-zA-Z0-9_/.:\\ ()]*\\|<string>\\)(\\([0-9]+\\))\\([a-zA-Z0-9_]*\\|\\?\\)()\\(->[^\n]*\\)?\n")

;; (defadvice pdb (before gud-query-cmdline activate)
;;     \"Provide a better default command line when called interactively.\"    
;;     (interactive     
;;      (list (gud-query-cmdline '/usr/lib/python2.3/pdb.py                             
;; 			      (file-name-nondirectory buffer-file-name)))))

(setq tramp-default-method "ssh")

(require 'psvn)
(require 'sgml-mode)
;;(require 'doctest-mode)
(require 'css-mode)
(require 'json)
(require 'php-mode)
;;(require 'jinja)

(setq auto-mode-alist (cons '("\\.jtmpl$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css.dtml$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jst$" . django-mode) auto-mode-alist))

(global-font-lock-mode t)
(global-set-key [f4] 'shell)
(global-set-key [f5] 'rename-buffer)
(global-set-key [f6] 'svn-status)
(global-set-key [f7] 'dvc-status)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'grep-find)

(setq frame-background-mode 'dark)

(setq auto-mode-alist
      (cons '("\\.txt$" . doctest-mode) auto-mode-alist))

;; Presentation font sizing
;; courier 24 font and 80x35 window dimension
;;(setq default-frame-alist '((width . 79)
;;(height . 25)
;;(font . "*-courier-*-150-*")
;;(setq initial-frame-alist default-frame-alist)))

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(put 'scroll-left 'disabled nil)

;;(load-file "/Users/whit/.emacs.d/flymake.el")
(require 'flymake)
(when (load "flymake" t) 
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                       'flymake-create-temp-inplace)) 
           (local-file (file-relative-name 
                        temp-file 
                        (file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 
  
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))) 

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;(require 'dirtrack)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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
