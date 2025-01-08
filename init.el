;; config the emax!

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

(package-initialize)


;; Bootstrap `use-package'
;; -- @@ look at leaf
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list
   'load-path (expand-file-name "elpa" user-emacs-directory))
  (require 'use-package))

(eval-when-compile (require 'use-package))

(require 'bind-key)

(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(unless (package-installed-p 'quelpa-use-package)
  (package-install 'quelpa-use-package))

(defconst whit-lisp
  (expand-file-name "lisp" user-emacs-directory)
  "My configurations.")

(add-to-list 'load-path whit-lisp)


(use-package init-whit  :demand t)
(use-package init-customvar  :demand t)
(use-package init-general  :demand t)
(use-package init-ai  :demand t)
(use-package init-music  :demand t)
(use-package init-aider  :demand t)
