;;; init-emacs --- Initialize emacs stuff.

;;; Commentary:

;;; My emacs stuff.

;;; Code:

(require 'use-package)

(use-package deferred
  :ensure t
  :defer t)

(use-package ns-win
  :demand t
  :config
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        ;;ns-auto-hide-menu-bar t
        ns-use-native-fullscreen nil
        ;; TODO: these aren't defined in ns-win -- are they actually used anywhere?
        ;; mac-option-key-is-meta nil
        ;; mac-command-key-is-meta t
        ))

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "GPG_AGENT_INFO" (expand-file-name "~/.gnupg/S.gpg-agent::1"))
    (setq epg-gpg-program "gpg2"))
  (dolist (face '(warning
                  mode-line-buffer-id
                  ;; magit-diff-file-heading
                  org-agenda-structure
                  org-warning))
    (set-face-attribute face nil :weight 'normal))
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :width 'semi-condensed
                      :height 140
                      :weight 'normal)))

(provide 'init-emacs)
;;; init-emacs ends here
