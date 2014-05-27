;;; babcore.el --- Core Emacs configuration. This should be the minimum in every emacs config.

;; Copyright (C) 2013 Arne Babenhauserheide

;; Author: Arne Babenhauserheide (and various others in Emacswiki and elsewhere).
;; Maintainer: Arne Babenhauserheide
;; Created 03 April 2013
;; Version: 0.0.5
;; Version Keywords: core configuration

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Quick Start / installation:
;; 1. Download this file and put it next to other files Emacs includes
;; 2. Add this to you .emacs file and restart emacs:
;; 
;;      (require 'babcore)
;;
;; Use Case: Use a common core configuration so you can avoid the
;;   tedious act of gathering all the basic stuff over the years and
;;   can instead concentrate on the really cool new stuff Emacs offers
;;   you.
;;
;; Todo:
;;

;;; Change Log:

;; 2013-11-02 - Disable clipboard sync while exporting with org-mode
;;              org-export-dispatch
;; 2013-10-22 - More useful frame titles
;; 2013-04-03 - Minor adjustments
;; 2013-02-29 - Initial release

;;; Code:

;; Convenient package handling in emacs

(require 'package)
;; use packages from marmalade
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; and the old elpa repo
(add-to-list 'package-archives '("elpa-old" . "http://tromey.com/elpa/"))
;; and automatically parsed versiontracking repositories.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Make sure a package is installed
(defun package-require (package)
  "Install a PACKAGE unless it is already installed 
or a feature with the same name is already active.

Usage: (package-require 'package)"
  ; try to activate the package with at least version 0.
  (package-activate package '(0))
  ; try to just require the package. Maybe the user has it in his local config

  (condition-case nil
      (require package)
    ; if we cannot require it, it does not exist, yet. So install it.
    (error (progn
             (package-install package)
             (require package)))))

;; Initialize installed packages
(package-initialize)  
;; package init not needed, since it is done anyway in emacs 24 after reading the init
;; but we have to load the list of available packages, if it is not available, yet.
(when (not package-archive-contents)
  (with-timeout (15 (message "updating package lists failed due to timeout"))
    (package-refresh-contents)))

;; Flymake: On the fly syntax checking

; stronger error display
(defface flymake-message-face
  '((((class color) (background light)) (:foreground "#b2dfff"))
    (((class color) (background dark))  (:foreground "#b2dfff")))
  "Flymake message face")

; show the flymake errors in the minibuffer
(package-require 'flymake-cursor)

;; Inline auto completion and suggestions
(package-require 'auto-complete)

; use ido mode for file and buffer Completion when switching buffers
(require 'ido)
(ido-mode t)

;; Convenient printing
(require 'printing)
(pr-update-menus t)
; make sure we use localhost as cups server
(setenv "CUPS_SERVER" "localhost")
(package-require 'cups)

; use allout minor mode to have outlining everywhere.
(allout-mode)

; syntax highlighting everywhere
(global-font-lock-mode 1)

; Activate org-mode
(require 'org)
; and some more org stuff

; http://orgmode.org/guide/Activation.html#Activation

; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

; And add babel inline code execution
; babel, for executing code in org-mode.
(org-babel-do-load-languages
 'org-babel-load-languages
 ; load all language marked with (lang . t).
 '((C . t)
   (R . t)
   (asymptote)
   (awk)
   (calc)
   (clojure)
   (comint)
   (css)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (fortran)
   (gnuplot . t)
   (haskell)
   (io)
   (java)
   (js)
   (latex)
   (ledger)
   (lilypond)
   (lisp)
   (matlab)
   (maxima)
   (mscgen)
   (ocaml)
   (octave)
   (org . t)
   (perl)
   (picolisp)
   (plantuml)
   (python . t)
   (ref)
   (ruby)
   (sass)
   (scala)
   (scheme)
   (screen)
   (sh . t)
   (shen)
   (sql)
   (sqlite)))

; Add proper word wrapping
(global-visual-line-mode t)

; go to the last change
(package-require 'goto-chg)
(global-set-key [(control .)] 'goto-last-change)
; M-. can conflict with etags tag search. But C-. can get overwritten
; by flyspell-auto-correct-word. And goto-last-change needs a really
; fast key.
(global-set-key [(meta .)] 'goto-last-change)

; Make german umlauts work.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;aspell und flyspell
(setq-default ispell-program-name "aspell")

;make aspell faster but less correctly
(setq ispell-extra-args '("--sug-mode=ultra" "-w" "äöüÄÖÜßñ"))
(setq ispell-list-command "list")

; control-lock-mode, so we can enter a vi style command-mode with standard emacs keys.
(package-require 'control-lock)
; also bind M-ü and M-ä to toggling control lock.
(global-set-key (kbd "M-ü") 'control-lock-toggle)
(global-set-key (kbd "C-ü") 'control-lock-toggle)
(global-set-key (kbd "M-ä") 'control-lock-toggle)
(global-set-key (kbd "C-ä") 'control-lock-toggle)
(global-set-key (kbd "C-z") 'control-lock-toggle)

; use key chords invoke commands
(package-require 'key-chord)
(key-chord-mode 1)
; buffer actions
(key-chord-define-global "vg"     'eval-region)
(key-chord-define-global "vb"     'eval-buffer)
(key-chord-define-global "cy"     'yank-pop)
(key-chord-define-global "cg"     "\C-c\C-c")
; frame actions
(key-chord-define-global "xo"     'other-window);
(key-chord-define-global "x1"     'delete-other-windows)
(key-chord-define-global "x0"     'delete-window)
(defun kill-this-buffer-if-not-modified ()
  (interactive)
  ; taken from menu-bar.el
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))
(key-chord-define-global "xk"     'kill-this-buffer-if-not-modified)
; file actions
(key-chord-define-global "bf"     'ido-switch-buffer)
(key-chord-define-global "cf"     'ido-find-file)
(key-chord-define-global "vc"     'vc-next-action)

(defun show-frame (&optional frame)
  "Show the current Emacs frame or the FRAME given as argument.

And make sure that it really shows up!"
  (raise-frame)
  ; yes, you have to call this twice. Dont ask me why&
  ; select-frame-set-input-focus calls x-focus-frame and does a bit of
  ; additional magic.
  (select-frame-set-input-focus (selected-frame))
  (select-frame-set-input-focus (selected-frame)))

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg: 

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property 
                            "WM_HINTS" frame "WM_HINTS" 
                            source nil t) nil))
         (flags (car wm-hints)))
    ; (message flags)
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention. 

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
  (x-urgency-hint frame (not arg))))

; fullscreen, taken from http://www.emacswiki.org/emacs/FullScreen#toc26
; should work for X und OSX with emacs 23.x (TODO find minimum version).
; for windows it uses (w32-send-sys-command #xf030) (#xf030 == 61488)
(defvar babcore-fullscreen-p t "Check if fullscreen is on or off")
(setq babcore-stored-frame-width nil)
(setq babcore-stored-frame-height nil)

(defun babcore-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter nil 'width 
                                (if babcore-stored-frame-width
                                    babcore-stored-frame-width 82))
           (set-frame-parameter nil 'height
                                (if babcore-stored-frame-height 
                                    babcore-stored-frame-height 42))
           (set-frame-parameter nil 'fullscreen nil))))

(defun babcore-fullscreen ()
  (interactive)
  (setq babcore-stored-frame-width (frame-width))
  (setq babcore-stored-frame-height (frame-height))
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun toggle-fullscreen ()
  (interactive)
  (setq babcore-fullscreen-p (not babcore-fullscreen-p))
  (if babcore-fullscreen-p
      (babcore-non-fullscreen)
    (babcore-fullscreen)))

(global-set-key [f11] 'toggle-fullscreen)

; Default KDE keybindings to make emacs nicer integrated into KDE. 

; can treat C-m as its own mapping.
; (define-key input-decode-map "\C-m" [?\C-1])

(defun revert-buffer-preserve-modes ()
  (interactive)
  (revert-buffer t nil t))

; C-m shows/hides the menu bar - thanks to http://stackoverflow.com/questions/2298811/how-to-turn-off-alternative-enter-with-ctrlm-in-linux
; f5 reloads
(defconst kde-default-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map [f5] 'revert-buffer-preserve-modes)
    (define-key map [?\C-1] 'menu-bar-mode)
    (define-key map [?\C-+] 'text-scale-increase)
    (define-key map [?\C--] 'text-scale-decrease) ; shadows 'negative-argument which is also available via M-- and C-M--, though.
    (define-key map [C-kp-add] 'text-scale-increase)
    (define-key map [C-kp-subtract] 'text-scale-decrease)
    map)
  "Keymap for `kde-default-keys-minor-mode'.")

;; Minor mode for keypad control
(define-minor-mode kde-default-keys-minor-mode
  "Adds some default KDE keybindings"
  :global t
  :init-value t
  :lighter ""
  :keymap 'kde-default-keys-minor-mode-map
  )

;; Set the frame title as by http://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format (list "%b : " (user-login-name) "@" (system-name) "%[ - GNU %F " emacs-version)
icon-title-format (list "%b ; " (user-login-name) "@" (system-name) " - GNU %F " emacs-version))

;; Highlight TODO and FIXME in comments 
(package-require 'fic-ext-mode)
(defun add-something-to-mode-hooks (mode-list something)
  "helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

(add-something-to-mode-hooks '(c++ tcl emacs-lisp python text markdown latex) 'fic-ext-mode)

; save the current macro as reusable function.
(defun save-current-kbd-macro-to-dot-emacs (name)
  "Save the current macro as named function definition inside
your initialization file so you can reuse it anytime in the
future."
  (interactive "SSave Macro as: ")
  (name-last-kbd-macro name)
  (save-excursion 
    (find-file-literally user-init-file)
    (goto-char (point-max))
    (insert "\n\n;; Saved macro\n")
    (insert-kbd-macro name)
    (insert "\n")))

; Activate transparent GnuPG encryption.
(require 'epa-file)
(epa-file-enable)

; colored shell commands via C-!
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun babcore-shell-execute(cmd)
  "Execute a shell command in an interactive shell buffer."
   (interactive "sShell command: ")
   (shell (get-buffer-create "*shell-commands-buf*"))
   (process-send-string (get-buffer-process "*shell-commands-buf*") (concat cmd "\n")))
(global-set-key (kbd "C-!") 'babcore-shell-execute)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

; save the place in files
(require 'saveplace)
(setq-default save-place t)

; show recent files
(package-require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)

; save minibuffer history
(require 'savehist)
(savehist-mode t)

;; save registers and open files over restarts,
;; thanks to http://www.xsteve.at/prg/emacs/power-user-tips.html
;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
;(setq desktop-save 'if-exists)
;(desktop-save-mode 1)

;; ;; save a bunch of variables to the desktop file
;; ;; for lists specify the len of the maximal saved data also
;; (setq desktop-globals-to-save
;;       (append '((extended-command-history . 300)
;;                 (file-name-history        . 100)
;;                 (grep-history             . 30)
;;                 (compile-history          . 30)
;;                 (minibuffer-history       . 5000)
;;                 (query-replace-history    . 60)
;;                 (read-expression-history  . 60)
;;                 (regexp-history           . 60)
;;                 (regexp-search-ring       . 20)
;;                 (search-ring              . 2000)
;;                 (shell-command-history    . 50)
;;                 tags-file-name
;;                 register-alist)))

;; ;; restore only 5 buffers at once and the rest lazily
;; (setq desktop-restore-eager 5)

; maybe nicer: http://github.com/doomvox/desktop-recover

; Do not Use the system clipboard. Reason: 
(setq x-select-enable-clipboard t)

; When I have this enabled, compiling an org-mode file to PDF locks
; KDE - I think it does so by filling up the clipboard.
(defadvice org-export-dispatch-no-clipboard-advice (around org-export-dispatch)
  "Do not clobber the system-clipboard while compiling an org-mode file with `org-export`."
  (let ((select-active-regions nil)
        (x-select-enable-clipboard nil)
        (x-select-enable-primary nil)
        (interprogram-cut-function nil)
        (interprogram-paste-function nil))
    ad-do-it))
(ad-activate 'org-export-dispatch-no-clipboard-advice t) ; :compile t

(package-require 'legalese)

(provide 'babcore)
;;; babcore.el ends here
