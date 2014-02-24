;;; framesize.el --- change the size of frames in Emacs

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: frames
;; URL: http://github.com/nicferrier/emacs-framesize
;; Package-Requires: ((key-chord "0.5.20080915"))
;; Version: 0.0.1
;; Created: 18th February 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple functions for changing font-size on a whole frame in Emacs.

;;; Code:

;;;###autoload
(defun frame-font-bigger ()
  (interactive)
  (let* ((inc 50)
         (sz (face-attribute 'default :height (selected-frame)))
         (ceil (+ (* inc (ceiling sz inc)) inc)))
    (set-face-attribute 'default (selected-frame) :height ceil)))

;;;###autoload
(defun frame-font-smaller ()
  (interactive)
  (let* ((inc 50)
         (sz (face-attribute 'default :height (selected-frame)))
         (ceil (- (* inc (ceiling sz inc)) (* 2 inc))))
    (condition-case err
        (set-face-attribute 'default (selected-frame) :height ceil)
      (error (message "can't change frame font size")))))

(defun frame-font-keychord-init ()
  (key-chord-mode t)
  (key-chord-define-global "fv" 'frame-font-bigger)
  (key-chord-define-global "fc" 'frame-font-smaller))

(provide 'framesize)

;;; framesize.el ends here
