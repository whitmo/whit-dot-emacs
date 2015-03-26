;;; ansible-doc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ansible-doc-mode ansible-doc) "ansible-doc" "ansible-doc.el"
;;;;;;  (21679 14677 647015 356000))
;;; Generated autoloads from ansible-doc.el

(autoload 'ansible-doc "ansible-doc" "\
Show ansible documentation for MODULE.

\(fn MODULE)" t nil)

(autoload 'ansible-doc-mode "ansible-doc" "\
Minor mode for Ansible documentation.

When called interactively, toggle `ansible-doc-mode'.  With
prefix ARG, enable `ansible-doc-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ansible-doc-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ansible-doc-mode'.  Otherwise behave as if called interactively.

In `ansible-doc-mode' provide the following keybindings for
Ansible documentation lookup:

\\{ansible-doc-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ansible-doc-pkg.el") (21679 14677 662325
;;;;;;  454000))

;;;***

(provide 'ansible-doc-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ansible-doc-autoloads.el ends here
