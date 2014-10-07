;;; org-journal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (org-journal-previous-entry org-journal-next-entry
;;;;;;  org-journal-read-entry org-journal-mark-entries org-journal-get-list
;;;;;;  org-journal-new-date-entry org-journal-new-entry org-journal-file-format
;;;;;;  org-journal-dir org-journal-format-string->regex org-journal-update-auto-mode-alist)
;;;;;;  "org-journal" "org-journal.el" (21555 57927 554421 113000))
;;; Generated autoloads from org-journal.el

(autoload 'org-journal-update-auto-mode-alist "org-journal" "\
Update auto-mode-alist to open journal files in
  org-journal-mode

\(fn)" nil nil)

(add-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)

(autoload 'org-journal-format-string->regex "org-journal" "\
Update org-journal-file-pattern with the current
  org-journal-file-format

\(fn FORMAT-STRING)" nil nil)

(defvar org-journal-dir "~/Documents/journal/" "\
Directory containing journal entries.
  Setting this will update auto-mode-alist using
  `(org-journal-update-auto-mode-alist)`")

(custom-autoload 'org-journal-dir "org-journal" nil)

(defvar org-journal-file-format "%Y%m%d" "\
Format string for journal file names, by default \"YYYYMMDD\".
  This pattern must include `%Y`, `%m` and `%d`. Setting this
  will update the internal `org-journal-file-pattern` to a regex
  that matches the format string using
  `(org-journal-format-string->regex format-string)`, and update
  `auto-mode-alist` using
  `(org-journal-update-auto-mode-alist)`.")

(custom-autoload 'org-journal-file-format "org-journal" nil)

(add-hook 'calendar-initial-window-hook 'org-journal-get-list)

(add-hook 'calendar-today-visible-hook 'org-journal-mark-entries)

(add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

(eval-after-load "calendar" '(progn (define-key calendar-mode-map "j" 'org-journal-read-entry) (define-key calendar-mode-map "]" 'org-journal-next-entry) (define-key calendar-mode-map "[" 'org-journal-previous-entry) (define-key calendar-mode-map (kbd "i j") 'org-journal-new-date-entry)))

(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(autoload 'org-journal-new-entry "org-journal" "\
Open today's journal file and start a new entry

\(fn)" t nil)

(autoload 'org-journal-new-date-entry "org-journal" "\
Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time.

\(fn ARG &optional EVENT)" t nil)

(autoload 'org-journal-get-list "org-journal" "\
Loads the list of files in the journal directory, and converts
  it into a list of calendar DATE elements

\(fn)" nil nil)

(autoload 'org-journal-mark-entries "org-journal" "\
Mark days in the calendar for which a diary entry is present

\(fn)" nil nil)

(autoload 'org-journal-read-entry "org-journal" "\
Open journal entry for selected date for viewing

\(fn ARG &optional EVENT)" t nil)

(autoload 'org-journal-next-entry "org-journal" "\
Go to the next date with a journal entry

\(fn)" t nil)

(autoload 'org-journal-previous-entry "org-journal" "\
Go to the previous date with a journal entry

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("org-journal-pkg.el") (21555 57927 572431
;;;;;;  336000))

;;;***

(provide 'org-journal-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-journal-autoloads.el ends here
