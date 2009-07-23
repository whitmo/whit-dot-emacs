(eval-after-load "pymacs"
  '(setq pymacs-load-path
  (append (list "~/emacs/pymacs"
                "/Users/whit/.emacs.d"))))
(require 'pymacs)
(pymacs-load "lovely.emacs.py")