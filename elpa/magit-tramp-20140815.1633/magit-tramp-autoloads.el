;;; magit-tramp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gist-tramp-file-name-handler) "gist-tramp" "gist-tramp.el"
;;;;;;  (21555 57931 14420 964000))
;;; Generated autoloads from gist-tramp.el

(defconst gist-tramp-method "gist" "\
TRAMP method for browsing gists.")

(defsubst gist-tramp-file-name-p (filename) (let ((v (tramp-dissect-file-name filename))) (string= (tramp-file-name-method v) gist-tramp-method)))

(autoload 'gist-tramp-file-name-handler "gist-tramp" "\


\(fn OPERATION &rest ARGS)" nil nil)

(eval-after-load 'tramp '(progn (add-to-list 'tramp-methods (cons gist-tramp-method nil)) (add-to-list 'tramp-foreign-file-name-handler-alist '(gist-tramp-file-name-p . gist-tramp-file-name-handler))))

;;;***

;;;### (autoloads (magit-tramp-file-name-handler) "magit-tramp" "magit-tramp.el"
;;;;;;  (21555 57931 6420 965000))
;;; Generated autoloads from magit-tramp.el

(defconst magit-tramp-method "git" "\
TRAMP method for browsing git repositories.")

(defsubst magit-tramp-file-name-p (filename) (let ((v (tramp-dissect-file-name filename))) (string= (tramp-file-name-method v) magit-tramp-method)))

(autoload 'magit-tramp-file-name-handler "magit-tramp" "\


\(fn OPERATION &rest ARGS)" nil nil)

(eval-after-load 'tramp '(progn (add-to-list 'tramp-methods (cons magit-tramp-method nil)) (add-to-list 'tramp-foreign-file-name-handler-alist '(magit-tramp-file-name-p . magit-tramp-file-name-handler))))

;;;***

;;;### (autoloads nil nil ("magit-tramp-pkg.el") (21555 57931 25783
;;;;;;  232000))

;;;***

(provide 'magit-tramp-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-tramp-autoloads.el ends here
