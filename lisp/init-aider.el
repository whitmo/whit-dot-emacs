;;; init-aider --- Aider AI pair programming support

;;; Commentary:
;; Configuration for aider.el integration

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Declare custom variables
(defcustom aider-args nil
  "Arguments to pass to aider command line tool."
  :type '(repeat string)
  :group 'aider)

(straight-use-package 'use-package)
(straight-use-package '(aider :type git :host github :repo "tninja/aider.el"))

(use-package aider
  :custom
  (aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  :bind
  ("C-c a" . aider-transient-menu))

(provide 'init-aider)
