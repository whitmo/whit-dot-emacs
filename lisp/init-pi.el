;;; init-pi.el --- Pi coding agent integration

;;; Commentary:
;; Configure Pilish, the Emacs frontend for pi.  Project-specific
;; environments are supplied by envrc/direnv in init-env, so sessions started
;; from buffers in a devenv project inherit that project's PATH and
;; PI_CODING_AGENT_DIR settings.
;;
;; The settings below are deliberately explicit and documented here rather
;; than hidden in a machine-local customization file: this is the workflow
;; contract for using pi from Emacs.

;;; Code:
(require 'use-package)

(declare-function envrc-propagate-environment "envrc" (orig &rest args))
(declare-function pilish-send "pilish-input" ())

(use-package pilish
  :ensure t
  :demand t
  :custom
  ;; Use the pi found through the active project environment.  In cream,
  ;; envrc supplies the devenv binary and the repo-local .pi configuration;
  ;; do not hard-code `pi-cream' globally because that launcher is only
  ;; available inside this project's devenv.
  (pilish-executable '("pi"))
  ;; Pilish uses RPC, where pi cannot display its normal trust prompt.  These
  ;; are trusted local repositories with project-managed skills/extensions,
  ;; so approve them explicitly; use `default' or `no-approve' for an
  ;; unfamiliar or untrusted checkout.
  (pilish-project-trust-policy 'approve)
  ;; Model changes/session switches can take longer than a short RPC round
  ;; trip, especially when using the model and subagent-heavy workflows.
  (pilish-rpc-timeout 60)
  ;; Keep a stable writing area for detailed, multiline prompts while
  ;; retaining the transcript above it.  `on-demand' is a good alternative
  ;; when reading a long conversation is more important than composing.
  (pilish-input-window-height 10)
  (pilish-input-window-display 'always)
  ;; Keep completed reasoning visible while reviewing tool use, extension
  ;; behavior, and delegated work.  TAB/menu controls can still collapse it
  ;; locally; use `hidden' here if transcript density becomes the priority.
  (pilish-thinking-display 'visible)
  (pilish-thinking-hidden-preview t)
  ;; Keep the Pi pane readable without changing the source Markdown beneath
  ;; it; these options suit both code review and artifact/document work.
  (pilish-input-markdown-highlighting t)
  (pilish-prettify-tables t)
  ;; Ask before compiling the required Markdown grammars.  This avoids an
  ;; unexpected compiler/network operation during startup; run
  ;; M-x pilish-install-grammars when you want to manage them explicitly.
  (pilish-essential-grammar-action 'prompt)
  ;; Preserve Markdown markup when copying answers into docs, issues, and
  ;; project artifacts.  Toggle this to nil when plain rendered text is more
  ;; useful for a particular destination.
  (pilish-copy-raw-markdown t)
  ;; Visit referenced files in another window so the active Pi transcript is
  ;; not displaced while reviewing or editing an answer.
  (pilish-visit-file-other-window t)
  ;; Keep the quit confirmation enabled: long-running Pi sessions may own
  ;; useful in-flight work.  This is the safe default for an always-on setup.
  (pilish-quit-without-confirmation nil)
  :bind (("C-c p" . pilish-menu)
         ("C-c P" . pilish)
         ;; Global entry points for the session-management actions used most
         ;; often across cream, worktrees, and other project buffers.
         ("C-c T" . pilish-toggle)
         ("C-c R" . pilish-session-browser)
         ("C-c O" . pilish-open-session-file))
  :config
  ;; Send with C-p C-p instead of Pilish's default C-c C-c.  A multi-key
  ;; binding makes C-p a prefix in the input buffer, so it intentionally
  ;; replaces ordinary C-p (`previous-line') there; M-p/M-n remain Pilish's
  ;; prompt-history bindings.  Use C-c C-c again if preserving C-p movement
  ;; is more important than this chord.
  (define-key pilish-input-mode-map (kbd "C-c C-c") nil)
  (define-key pilish-input-mode-map (kbd "C-p C-p") 'pilish-send)
  ;; envrc stores PATH and Pi variables buffer-locally, while Pilish creates
  ;; fresh chat/input buffers before starting its subprocess.  Propagate the
  ;; active project's environment across that boundary so a project devenv's
  ;; Pi binary and credentials/configuration are used, not Emacs's global PATH.
  (when (fboundp 'envrc-propagate-environment)
    (unless (advice-member-p 'envrc-propagate-environment 'pilish)
      (advice-add 'pilish :around #'envrc-propagate-environment)))
  ;; Keep the short command name familiar while making `pilish' the canonical
  ;; function name used by the package and its documentation.
  (defalias 'pi #'pilish))

(provide 'init-pi)
;;; init-pi.el ends here
