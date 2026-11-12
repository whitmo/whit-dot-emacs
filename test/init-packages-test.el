;;; init-packages-test.el --- Tests for package bootstrap -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))

(defvar whit-package-selected-packages nil)

(cl-letf (((symbol-function 'package-installed-p) (lambda (_package) t)))
  (require 'init-packages))

(ert-deftest whit-package-install-refreshes-stale-archive-and-retries ()
  (let ((install-attempts 0)
        (refreshes 0))
    (cl-letf (((symbol-function 'package-installed-p) (lambda (_package) nil))
              ((symbol-function 'package-refresh-contents)
               (lambda () (cl-incf refreshes)))
              ((symbol-function 'package-install)
               (lambda (_package)
                 (cl-incf install-attempts)
                 (when (= install-attempts 1)
                   (signal 'file-error '("stale archive tarball")))))
              ((symbol-function 'package-vc-install)
               (lambda (_spec)
                 (error "VC fallback should not run after a stale archive refresh succeeds"))))
      (whit-package-install 'go-mode)
      (should (= install-attempts 2))
      (should (= refreshes 1)))))

;;; init-packages-test.el ends here
