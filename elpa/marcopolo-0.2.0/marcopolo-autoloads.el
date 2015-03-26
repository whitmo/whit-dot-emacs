;;; marcopolo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (marcopolo-search) "marcopolo-mode" "marcopolo-mode.el"
;;;;;;  (21778 51003 262166 166000))
;;; Generated autoloads from marcopolo-mode.el

(autoload 'marcopolo-search "marcopolo-mode" "\
Show Docker repositories  using `TERM' request.

\(fn TERM)" t nil)

;;;***

;;;### (autoloads (marcopolo-version) "marcopolo-version" "marcopolo-version.el"
;;;;;;  (21778 51003 238166 168000))
;;; Generated autoloads from marcopolo-version.el

(autoload 'marcopolo-version "marcopolo-version" "\
Get the marcopolo version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

;;;***

;;;### (autoloads nil nil ("marcopolo-api.el" "marcopolo-custom.el"
;;;;;;  "marcopolo-hub.el" "marcopolo-pkg.el" "marcopolo-registry.el"
;;;;;;  "marcopolo-ui.el" "marcopolo-utils.el" "marcopolo.el") (21778
;;;;;;  51003 287539 954000))

;;;***

(provide 'marcopolo-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; marcopolo-autoloads.el ends here
