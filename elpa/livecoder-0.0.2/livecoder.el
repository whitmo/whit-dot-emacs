;;; livecoder.el --- tools for live coders

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: languages
;; Version: 0.0.2

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

;; I do live coding quite a bit, it needs better tool support. This is
;; the start of me collecting the things I do.

;;; Code:

(require 'rx)

(defvar livecoder-comment-pos nil
  "Stores the last code position when we're editing a comment.")

;;;###autoload
(defun livecoder-comment-edit ()
  "Toggle between editing a comment and code.

Edit the comment at the start of a top level form and switch back
to the previous position when you're done.

This is intended to make screencast recording simple.  It's
probably not much better than just using the keys to go to the
top and then editing... but it is a little quicker."
  (interactive)
  (if (markerp livecoder-comment-pos)
      (progn
        (goto-char livecoder-comment-pos)
        (set-marker livecoder-comment-pos nil)
        (setq livecoder-comment-pos nil))
      (setq livecoder-comment-pos (point-marker))
      (beginning-of-defun)
      (forward-line -1)
      (beginning-of-line)
      (if (looking-at (rx-to-string `(and bol ,comment-start)))
          (goto-char (re-search-forward ";+ *" nil (line-end-position)))
          (insert (format "%s " comment-start)))))

;;;###autoload
(defun livecoder-hookup ()
  "Call this from a mode hook to init livecoder."
  ;; Not sure about the key binding
  (local-set-key (kbd "C-c e") 'livecoder-comment-edit))


(provide 'livecoder)

;;; livecoder.el ends here
