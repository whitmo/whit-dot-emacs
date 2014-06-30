;;; shorten-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (shorten-strings) "shorten" "shorten.el" (21425
;;;;;;  32864 66407 417000))
;;; Generated autoloads from shorten.el

(autoload 'shorten-strings "shorten" "\
Takes a list of strings and returns an alist ((STRING
. SHORTENED-STRING) ...).  Uses `shorten-split-function' to split
the strings, and `shorten-join-function' to join shortened
components back together into SHORTENED-STRING.  See also
`shorten-validate-component-function'.

\(fn STRINGS)" nil nil)

;;;***

;;;### (autoloads nil nil ("shorten-pkg.el") (21425 32864 92257 12000))

;;;***

(provide 'shorten-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shorten-autoloads.el ends here
