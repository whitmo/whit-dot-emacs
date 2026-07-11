;;; init-ai-test.el --- Tests for AI init helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(setq load-prefer-newer t)

(dolist (dir (file-expand-wildcards
              (expand-file-name "../elpa/*" (file-name-directory load-file-name))))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))

(require 'init-ai)

(ert-deftest whit-pi-coding-agent-install-missing-grammars-installs-all-pi-missing-grammars ()
  (let (installed)
    (cl-letf (((symbol-function 'require) (lambda (feature &optional _filename _noerror)
                                            (eq feature 'pi-coding-agent-grammars)))
              ((symbol-function 'pi-coding-agent--missing-essential-grammars)
               (lambda () '(markdown markdown-inline)))
              ((symbol-function 'pi-coding-agent--missing-optional-grammars)
               (lambda () '(python yaml)))
              ((symbol-function 'pi-coding-agent--install-grammars)
               (lambda (grammars)
                 (setq installed grammars)
                 (length grammars))))
      (should (= 4 (whit-pi-coding-agent-install-missing-grammars)))
      (should (equal installed '(markdown markdown-inline python yaml))))))

;;; init-ai-test.el ends here
