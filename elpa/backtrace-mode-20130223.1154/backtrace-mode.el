;;; backtrace-mode.el --- A better way to browse /var/log/messages files
;;
;; Author: Eric Crosson
;; URL: https://github.com/EricCrosson/backtrace-mode
;; Version: 20130223.1154
;; X-Original-Version: 0.0.10
;;

(defconst backtrace-identifier "SEGV\\|ABRT"
  "A regexp to identify the beginning of the backtrace.")

(when (require 'generic-x)
  (define-generic-mode
      'backtrace-mode			;name of mode
    '("///////")			;comments
    '("backtrace" "valid" "SEGV" "ABRT" "Thread" "false" "true" "dumpmemlog"
      "kernel" "syslog-ng" "pphone" "udhcpc"
      "this" "int" "void" "bool" "p2" "p8" "p8cg" "_phone") ;some keywords
    '(("^.\\{29\\}"		.	font-lock-comment-face) ;time-stamp
      ("\\(none\\)"		.	font-lock-comment-face)
      ("pri.*,sf"		.	font-lock-comment-face)
      ("<\\.\\*>"		.	font-lock-string-face) ;operators
      ("src.*,msg"		.	font-lock-string-face)
      ("null"			.	font-lock-constant-face)
      ("0x\\B[0-9A-Fa-f]*\\b"	.	font-lock-constant-face)
      ("[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}"
				.	font-lock-preprocessor-face)
      ("End dumpmemlog"		.	font-lock-warning-face)
      ("Segmentation fault!!!"	.	font-lock-warning-face))
    '("[.]*messages$")			;default file type
    ;; 'backtrace-mode-hook		;other functions to call
    "A mode to view /var/log/message files with."))

;; (defvar backtrace-mode-hook
;;   "This is the hook corresponding with backtrace-mode.")

;(add-hook 'backtrace-mode-hook 'buffer-read-only)

(defun extract-the-backtrace ()
  "Extract the backtrace from a `messages' file and show it in a
  temp buffer."
  (interactive)
  (end-of-buffer)
  (when (search-backward-regexp backtrace-identifier nil t)
    (next-line)
    (beginning-of-line)

    (let ((excerpt (buffer-substring
		    (point)
		    (progn
		      (search-forward "Start dumpmemlog")
		      (previous-line)
		      (end-of-line)
		      (recenter-top-bottom)
		      (point))))
	  (tmp-buf (generate-new-buffer
		    (generate-backtrace-temp-buffer
		     (buffer-name)))))
      (switch-to-buffer tmp-buf)
      (erase-buffer)
      (insert excerpt))))

(defun generate-backtrace-temp-buffer (buffer)
  "Return the name of the temp buffer to be used in
  \\[extract-the-backtrace]."
  (concat "*crash-from:" buffer "*"))


(provide 'backtrace-mode)

;;; backtrace-mode.el ends here
