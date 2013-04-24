;;; pylint.el --- run the python pylint checker putting hits in a grep buffer

;; Copyright (C) 2011 Darius Powell

;; Author: Darius Powell <dariusp686@gmail.com>
;; Version: 1.0
;; URL: http://bitbucket.org/dariusp686/emacs-pylint
;; Keywords: python, pylint, check, lint

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar pylint-hist nil)

(defgroup pylint nil
  "Run pylint putting hits in a grep buffer."
  :group 'tools
  :group 'processes)

(defcustom pylint-cmd "pylint -rn -f parseable"
  "The pylint command."
  :type 'string
  :group 'pylint)

;;;###autoload
(defun pylint ()
  (interactive)
  (let* ((cmd (read-shell-command "Command: " (concat pylint-cmd " " (file-name-nondirectory (or (buffer-file-name) ""))) 'pylint-hist))
         (null-device nil))
    (grep cmd)))

(provide 'pylint)

;;; pylint.el ends here
