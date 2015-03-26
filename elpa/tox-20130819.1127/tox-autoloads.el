;;; tox-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tox-current-class tox-current-test) "tox" "tox.el"
;;;;;;  (21594 19761 7736 265000))
;;; Generated autoloads from tox.el

(autoload 'tox-current-test "tox" "\
Launch tox on current test.
A prefix arg will ask for a env to use which is by default what
specified in `tox-default-env'.

\(fn &optional ASKENVS)" t nil)

(autoload 'tox-current-class "tox" "\
Launch tox on current class.
A prefix arg will ask for a env to use which is by default what
specified in `tox-default-env'.

\(fn &optional ASKENVS)" t nil)

;;;***

;;;### (autoloads nil nil ("tox-pkg.el") (21594 19761 26947 272000))

;;;***

(provide 'tox-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tox-autoloads.el ends here
