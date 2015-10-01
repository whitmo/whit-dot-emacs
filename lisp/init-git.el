;;; init-git --- Initialize git stuffs.

;;; Commentary:

;;; My git.

;;; Code:

(require 'use-package)
(require 'auth-source)
(require 'url-parse)
(require 'hydra)

(eval-when-compile
  ;; TODO: disabled because magit update
  ;; (require 'git-gutter-fringe+)
  (require 'cl)
  (require 'magit))

;; TODO: disabled because magit update
;; (use-package git-gutter-fringe+
;;   :ensure t
;;   :config
;;   (progn
;;     (global-git-gutter+-mode t)
;;     (git-gutter-fr+-minimal)
;;     (setq git-gutter+-lighter "")))

(use-package browse-at-remote
  :ensure t
  :bind ("C-c b" . browse-at-remote))

(use-package diff-hl
  :ensure t
  :init
  (progn
    (setq diff-hl-command-prefix (kbd "C-c d"))
    (global-diff-hl-mode 1)))

;; (use-package gh
;;   :ensure t
;;   :demand t
;;   :config
;;   (progn
;;     (defun* whit-gh-profile (url user)
;;       (let* (
;;              (urlobj (url-generic-parse-url url))
;;              (host (url-host urlobj))
;;              (auth-info
;;               (car
;;                (auth-source-search
;;                 :max 1
;;                 :host host
;;                 :user user
;;                 :port 443
;;                 :create nil)))
;;              (token (funcall (plist-get auth-info :secret))))
;;         (list
;;          :url url
;;          :username user
;;          :token token
;;          :remote-regexp (gh-profile-remote-regexp host))))

;;     (setq
;;      gh-profile-default-profile "bh"
;;      gh-profile-current-profile nil
;;      gh-profile-alist
;;      (list
;;       (cons "bh" (whit-gh-profile "https://github.banksimple.com/api/v3" "whit"))
;;       (cons "gh" (whit-gh-profile "https://api.github.com" "whitmo"))))))

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t)

(use-package orgit
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-dispatch-popup)
  :config
  (progn
    (autoload 'magit-status' "magit" nil t)
    (add-hook 'git-commit-mode-hook 'turn-off-auto-fill)

    ;; (magit-define-popup-action
    ;;  'magit-dispatch-popup ?. "Status" 'magit-status)
    ;; (magit-define-popup-action
    ;;  'magit-dispatch-popup ?H "Github" 'whit-github-popup)
    ;; (magit-define-popup whit-github-popup
    ;;                     "Popup console for Github interaction."
    ;;                     :actions  '((?p "Pull request" whit-pull-request)
    ;;                                 (?f "Fork" whit-fork))
    ;;                     :default-action 'whit-pull-request)

    (defun whit-pull-request ()
      "Open a Github pull request.

Run `hub pull-request' asynchronously; see
`whit-pull-request-sentinel' for the interesting bits."
      (interactive)
      (set-process-sentinel
       (magit-run-git-with-editor "pull-request")
       'whit-pull-request-sentinel))
    (defun whit-pull-request-sentinel (process event)
      "Handle EVENT in PROCESS.

After `hub pull-request' exits, check the output logged in the magit
process log for a pattern that looks like a pull request URL and add
it to the kill ring."
      (magit-process-sentinel process event)
      (when (eq (process-status process) 'exit)
        (with-current-buffer (process-buffer process)
          (let* ((section (process-get process 'section))
                 (beg (marker-position (magit-section-content section)))
                 (end (marker-position (magit-section-end section)))
                 (content (buffer-substring beg end))
                 (match (string-match "http.*/pull/.*" content))
                 (url (match-string 0 content)))
            (kill-new url)))))
    (defun whit-fork ()
      "Fork a Github repository."
      (interactive)
      (magit-run-git-async "fork"))

    (setq magit-git-executable (expand-file-name "~/bin/hub")
          magit-save-repository-buffers 'dontask
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-completing-read-function 'magit-builtin-completing-read
          ;; helm--completing-read-default
          magit-push-always-verify nil
          magit-revert-buffers 1
          magit-delete-by-moving-to-trash nil
          magit-diff-paint-whitespace nil)))

(provide 'init-git)
;;; init-git ends here
