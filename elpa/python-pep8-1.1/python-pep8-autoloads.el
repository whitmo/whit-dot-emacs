;;; python-pep8-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (python-pep8) "python-pep8" "python-pep8.el" (20856
;;;;;;  3364 0 0))
;;; Generated autoloads from python-pep8.el

(autoload 'python-pep8 "python-pep8" "\
Run PEP8, and collect output in a buffer.
While pep8 runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<python-pep8-mode-map>\\[compile-goto-error] in the grep output buffer, to go to the lines where pep8 found matches.

\(fn)" t nil)

(defalias 'pep8 'python-pep8)

;;;***

;;;### (autoloads nil nil ("python-pep8-pkg.el") (20856 3364 918506
;;;;;;  0))

;;;***

(provide 'python-pep8-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-pep8-autoloads.el ends here
