;;; deft-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (deft deft-new-file deft-find-file) "deft" "deft.el"
;;;;;;  (21988 23063 225265 898000))
;;; Generated autoloads from deft.el

(autoload 'deft-find-file "deft" "\
Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

\(fn FILE)" t nil)

(autoload 'deft-new-file "deft" "\
Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title.

\(fn)" t nil)

(autoload 'deft "deft" "\
Switch to *Deft* buffer and load files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("deft-pkg.el") (21988 23063 243858 660000))

;;;***

(provide 'deft-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; deft-autoloads.el ends here
