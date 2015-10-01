;;; init-dev --- Initialize development environment

;;; Commentary:

;;; golang receptacle

;;; Code:

;; TODO ;;(add-to-list 'load-path "~/.emacs.d/vendored/go-mode.el/")
;;(require 'go-mode)

;;;### (autoloads (go-download-play godoc gofmt-before-save go-mode)
;;;;;;  "vendored/go-mode.el/go-mode" "vendored/go-mode.el/go-mode.el"
;;;;;;  (21780 11673 412311 81000))
;;; Generated autoloads from vendored/go-mode.el/go-mode.el

;; go get github.com/rogpeppe/godef
(use-package go-mode
  :mode "\\.go"
  :ensure t
  :config
  (progn
    (bind-keys :map go-mode-map
               ("C-c C-d" . godoc-at-point))
    ;; go get code.google.com/p/go.tools/cmd/oracle
    (add-to-list 'load-path
                 (concat whit-gopath "src/code.google.com/p/go.tools/cmd/oracle/"))
    (setq compilation-error-regexp-alist
          (cons 'go-test compilation-error-regexp-alist))
    (use-package go-oracle
      :init (load "oracle")
      :config
      (progn
        (setq go-oracle-command (executable-find "oracle"))
        (add-hook 'go-mode-hook 'go-oracle-mode)))
    (use-package go-eldoc
      :ensure t
      :config (add-hook 'go-mode-hook 'go-eldoc-setup))
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)
    (bind-keys :map go-mode-map
               ("M-." . godef-jump))))

(autoload 'go-mode "vendored/go-mode.el/go-mode" "\
Major mode for editing Go source text.

This mode provides (not just) basic editing capabilities for
working with Go code. It offers almost complete syntax
highlighting, indentation that is almost identical to gofmt and
proper parsing of the buffer content to allow features such as
navigation by function, manipulation of comments or detection of
strings.

In addition to these core features, it offers various features to
help with writing Go code. You can directly run buffer content
through gofmt, read godoc documentation from within Emacs, modify
and clean up the list of package imports or interact with the
Playground (uploading and downloading pastes).

The following extra functions are defined:

- `gofmt'
- `godoc'
- `go-import-add'
- `go-remove-unused-imports'
- `go-goto-imports'
- `go-play-buffer' and `go-play-region'
- `go-download-play'
- `godef-describe' and `godef-jump'
- `go-coverage'

If you want to automatically run `gofmt' before saving a file,
add the following hook to your emacs configuration:

\(add-hook 'before-save-hook #'gofmt-before-save)

If you want to use `godef-jump' instead of etags (or similar),
consider binding godef-jump to `M-.', which is the default key
for `find-tag':

\(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd \"M-.\") #'godef-jump)))

Please note that godef is an external dependency. You can install
it with

go get code.google.com/p/rog-go/exp/cmd/godef


If you're looking for even more integration with Go, namely
on-the-fly syntax checking, auto-completion and snippets, it is
recommended that you look at flycheck
\(see URL `https://github.com/flycheck/flycheck') or flymake in combination
with goflymake (see URL `https://github.com/dougm/goflymake'), gocode
\(see URL `https://github.com/nsf/gocode'), go-eldoc
\(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go
\(see URL `https://github.com/dominikh/yasnippet-go')

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(autoload 'gofmt-before-save "vendored/go-mode.el/go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause go-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading.

\(fn)" t nil)

(autoload 'godoc "vendored/go-mode.el/go-mode" "\
Show Go documentation for QUERY, much like M-x man.

\(fn QUERY)" t nil)

(autoload 'go-download-play "vendored/go-mode.el/go-mode" "\
Download a paste from the playground and insert it in a Go buffer.
Tries to look for a URL at point.

\(fn URL)" t nil)

;;;***

(add-to-list 'load-path (expand-file-name "~/.emacs.d/local/vs-go.el"))
;;(require 'vs-go)

;; (setq exec-path (cons "/home/whit/.gvm/pkgsets/go1.2.1/global/bin:/home/whit/.gvm/gos/go1.2.1/bin" exec-path))
;; (setq exec-path (cons "/home/whit/proj/vs/bin" exec-path))
;; (add-to-list 'exec-path "/Users/tleyden/Development/gocode/bin")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(setenv "PATH" "/home/whit/proj/vs/bin:/home/whit/.gvm/pkgsets/go1.2.1/global/bin:/home/whit/.gvm/gos/go1.2.1/bin:/home/whit/.gvm/pkgsets/go1.2.1/global/overlay/bin:/home/whit/.gvm/bin:/home/whit/.gvm/bin:/home/whit/dev/vs/bin:/home/whit/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/whit/dev/gostuff//bin:/home/whit/.juju-plugins")

(setenv "GOPATH" "/home/whit/proj/vs:/home/whit/.gvm/pkgsets/go1.2.1/global")
(setenv "GOROOT" "/home/whit/.gvm/gos/go1.2.1")

(provide 'init-golang)
;;; init-golang ends here
