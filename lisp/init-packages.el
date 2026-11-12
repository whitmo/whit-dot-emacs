;;; init-packages.el --- Package bootstrap and defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep package management boring and reproducible with package.el + use-package.
;; If ~/.emacs.d/elpa is wiped, this file reinstalls the selected package set.

;;; Code:

(require 'package)
(require 'seq)

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("jcs-elpa"     . "https://jcs-emacs.github.io/jcs-elpa/packages/")))

;; Some package autoload files produced by transient-based packages evaluate
;; `(transient-prefix ...)' while package.el is activating packages.  Make the
;; macro available first so package activation stays quiet.
(dolist (pattern '("elpa/compat-*" "elpa/transient-*"))
  (when-let* ((dir (car (file-expand-wildcards
                        (expand-file-name pattern user-emacs-directory)))))
    (add-to-list 'load-path dir)))
(ignore-errors (require 'transient))

(package-initialize)

(defvar whit-package-selected-packages
  '(acp
    agent-shell
    ai-code
    alert
    ample-theme
    claude-code
    codespaces
    dash
    editorconfig
    exec-path-from-shell
    f
    flycheck
    flycheck-rust
    flymake-ruff
    gh
    go-mode
    google-gemini
    js2-mode
    json-mode
    lua-mode
    magit
    magit-section
    markdown-mode
    openai
    pi-coding-agent
    projectile
    ruff-format
    rust-mode
    rust-playground
    shell-here
    shell-maker
    shell-switcher
    transient
    typst-mode
    use-package
    vterm
    web-mode
    websocket
    yaml-mode
    yaml-pro)
  "Packages I intentionally want restored after an elpa wipe.
Dependencies are deliberately omitted unless I also use them directly;
package.el will install dependency packages automatically.")

(setq package-selected-packages whit-package-selected-packages)

(defun whit-package-refresh-contents-once ()
  "Refresh package archive metadata if needed."
  (unless package-archive-contents
    (package-refresh-contents)))

(defun whit-package-install (package)
  "Install PACKAGE, trying refreshed metadata and VC metadata as fallbacks."
  (unless (package-installed-p package)
    (whit-package-refresh-contents-once)
    (condition-case first-err
        (package-install package)
      (error
       ;; MELPA can garbage-collect tarballs named by stale local archive
       ;; metadata.  Refresh archive contents and retry once before falling back
       ;; to explicit VC metadata or surfacing the original package error.
       (condition-case _retry-err
           (progn
             (package-refresh-contents)
             (package-install package))
         (error
          (let ((vc-spec (and (boundp 'package-vc-selected-packages)
                              (alist-get package package-vc-selected-packages))))
            (if vc-spec
                (package-vc-install (cons package vc-spec))
              (signal (car first-err) (cdr first-err))))))))))

(defun whit-package-install-selected-packages ()
  "Install every package from `whit-package-selected-packages' that is missing."
  (interactive)
  (dolist (package whit-package-selected-packages)
    (whit-package-install package)))

;; Bootstrap use-package first so the rest of init can be declarative.
(whit-package-install 'use-package)
(require 'use-package)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t
      use-package-verbose t)

;; Do the restore check after use-package is available.  This is intentionally
;; eager: after a wiped elpa directory, startup should rebuild the environment.
(whit-package-install-selected-packages)

(provide 'init-packages)
;;; init-packages.el ends here
