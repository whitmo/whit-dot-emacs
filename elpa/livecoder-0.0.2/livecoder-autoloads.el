;;; livecoder-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (livecoder-hookup livecoder-comment-edit) "livecoder"
;;;;;;  "livecoder.el" (21555 56427 86485 427000))
;;; Generated autoloads from livecoder.el

(autoload 'livecoder-comment-edit "livecoder" "\
Toggle between editing a comment and code.

Edit the comment at the start of a top level form and switch back
to the previous position when you're done.

This is intended to make screencast recording simple.  It's
probably not much better than just using the keys to go to the
top and then editing... but it is a little quicker.

\(fn)" t nil)

(autoload 'livecoder-hookup "livecoder" "\
Call this from a mode hook to init livecoder.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("livecoder-pkg.el") (21555 56427 100873
;;;;;;  368000))

;;;***

(provide 'livecoder-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; livecoder-autoloads.el ends here
