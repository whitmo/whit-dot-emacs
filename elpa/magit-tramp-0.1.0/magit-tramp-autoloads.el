;;; magit-tramp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-tramp-file-name-handler) "magit-tramp" "magit-tramp.el"
;;;;;;  (21355 42153 801062 385000))
;;; Generated autoloads from magit-tramp.el

(defconst magit-tramp-method "git" "\
TRAMP method for browsing git repositories.")

(defsubst magit-tramp-file-name-p (filename) (let ((v (tramp-dissect-file-name filename))) (string= (tramp-file-name-method v) magit-tramp-method)))

(autoload 'magit-tramp-file-name-handler "magit-tramp" "\


\(fn OPERATION &rest ARGS)" nil nil)

(eval-after-load 'tramp '(progn (add-to-list 'tramp-methods (cons magit-tramp-method nil)) (add-to-list 'tramp-foreign-file-name-handler-alist '(magit-tramp-file-name-p . magit-tramp-file-name-handler))))

;;;***

;;;### (autoloads nil nil ("magit-tramp-pkg.el") (21355 42153 822779
;;;;;;  571000))

;;;***

(provide 'magit-tramp-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-tramp-autoloads.el ends here
