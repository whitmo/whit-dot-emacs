; $Id: xml-mode.el,v 1.2 2000/08/29 13:51:11 connolly Exp $

; References
;Major and Minor Modes
;http://www.gnu.org/manual/elisp-manual-20-2.5/html_chapter/elisp_23.html#SEC337

;GNU Emacs Lisp Reference Manual
;
;For Emacs Version 20.3
;
;Revision 2.5, May 1998
;
;by Bil Lewis, Dan LaLiberte, Richard Stallman
;and the GNU Manual Group
; http://www.gnu.org/manual/elisp-manual-20-2.5/html_chapter/elisp_toc.html

;@@rdf model for publication events,
; containment of one resource in another for
; the purpose of some publication


; also, sgml-mode.el from
; emacs19-el 19.34-26.5 package
; http://www.debian.org/Packages/stable/editors/emacs19-el.html

;;; sgml-mode.el --- SGML- and HTML-editing modes

;; Copyright (C) 1992, 1995, 1996 Free Software Foundation, Inc.

;; Author: James Clark <jjc@clark.com>
;; Adapted-By: ESR; Daniel.Pfeiffer@Informatik.START.dbp.de
;; Keywords: wp, hypermedia, comm, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Configurable major mode for editing document in the SGML standard general
;; markup language.  As an example contains a mode for editing the derived
;; HTML hypertext markup language.

;;; Code:

;; As long as Emacs' syntax can't be complemented with predicates to context
;; sensitively confirm the syntax of characters, we have to live with this
;; kludgy kind of tradeoff.
(defvar xml-specials '(?\" ?-)
  "List of characters that have a special meaning for xml-mode.
This list is used when first loading the xml-mode library.
The supported characters and potential disadvantages are:

  ?\\\"	Makes \" in text start a string.
  ?'	Makes ' in text start a string.
  ?-	Makes -- in text start a comment.

When only one of ?\\\" or ?' are included, \"'\" or '\"' as it can be found in
DTDs, start a string.  To partially avoid this problem this also makes these
self insert as named entities depending on `xml-quick-keys'.  <!----> must
contain an even multiple of two (4, 8, ...) minuses, or Emacs' syntax
mechanism won't recognize a comment.")

(defvar xml-quick-keys t
  "Use <, >, &, SPC and `xml-specials' keys ``electrically'' when non-nil.
This takes effect when first loading the library.")


(defvar xml-mode-map
  (let ((map (list 'keymap (make-vector 256 nil)))
	(menu-map (make-sparse-keymap "SGML")))
    (define-key map "\t" 'indent-relative-maybe)
    (define-key map "\C-c\C-i" 'xml-tags-invisible)
    (define-key map "/" 'xml-slash)
    (define-key map "\C-c\C-n" 'xml-name-char)
    (define-key map "\C-c\C-t" 'xml-tag)
    (define-key map "\C-c\C-a" 'xml-attributes)
    (define-key map "\C-c\C-b" 'xml-skip-tag-backward)
    (define-key map [?\C-c left] 'xml-skip-tag-backward)
    (define-key map "\C-c\C-f" 'xml-skip-tag-forward)
    (define-key map [?\C-c right] 'xml-skip-tag-forward)
    (define-key map "\C-c\C-d" 'xml-delete-tag)
    (define-key map "\C-c\^?" 'xml-delete-tag)
    (define-key map "\C-c?" 'xml-tag-help)
    (define-key map "\C-c8" 'xml-name-8bit-mode)
    (define-key map "\C-c\C-v" 'xml-validate)
    (if xml-quick-keys
	(progn
	  (define-key map "&" 'xml-name-char)
	  (define-key map "<" 'xml-tag)
	  (define-key map " " 'xml-auto-attributes)
	  (define-key map ">" 'xml-maybe-end-tag)))
    (let ((c 127)
	  (map (nth 1 map)))
      (while (< (setq c (1+ c)) 256)
	(aset map c 'xml-maybe-name-self)))
    (define-key map [menu-bar sgml] (cons "SGML" menu-map))
    (define-key menu-map [xml-validate] '("Validate" . xml-validate))
    (define-key menu-map [xml-name-8bit-mode]
      '("Toggle 8 Bit Insertion" . xml-name-8bit-mode))
    (define-key menu-map [xml-tags-invisible]
      '("Toggle Tag Visibility" . xml-tags-invisible))
    (define-key menu-map [xml-tag-help]
      '("Describe Tag" . xml-tag-help))
    (define-key menu-map [xml-delete-tag]
      '("Delete Tag" . xml-delete-tag))
    (define-key menu-map [xml-skip-tag-forward]
      '("Forward Tag" . xml-skip-tag-forward))
    (define-key menu-map [xml-skip-tag-backward]
      '("Backward Tag" . xml-skip-tag-backward))
    (define-key menu-map [xml-attributes]
      '("Insert Attributes" . xml-attributes))
    (define-key menu-map [xml-tag] '("Insert Tag" . xml-tag))
    map)
  "Keymap for SGML mode.  See also `xml-specials'."
  )


(defvar xml-mode-syntax-table
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (if (memq ?- xml-specials)
	(modify-syntax-entry ?- "_ 1234" table))
    (if (memq ?\" xml-specials)
	(modify-syntax-entry ?\" "\"\"" table))
    (if (memq ?' xml-specials)
	(modify-syntax-entry ?\' "\"'" table))
    table)
  "Syntax table used in SGML mode.  See also `xml-specials'.")


(defvar xml-name-8bit-mode nil
  "*When non-`nil' insert 8 bit characters with their names.")

(defvar xml-char-names
  [nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil "quot" nil nil nil nil "apos"
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil "lt" nil "gt" nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
]
  "Vector of symbolic character names without `&' and `;'.")


;; sgmls is a free SGML parser available from
;; ftp.uu.net:pub/text-processing/sgml
;; Its error messages can be parsed by next-error.
;; The -s option suppresses output.

(defvar xml-validate-command "xmlwf"
  "*The command to validate an SGML document.
The file name of current buffer file name will be appended to this,
separated by a space.")

(defvar xml-saved-validate-command nil
  "The command last used to validate in this buffer.")


;;; I doubt that null end tags are used much for large elements,
;;; so use a small distance here.
(defconst xml-slash-distance 1000
  "*If non-nil, is the maximum distance to search for matching /.")

(defconst xml-start-tag-regex
  "<[A-Za-z]\\([-_.A-Za-z0-9= \n\t]\\|\"[^\"]*\"\\|'[^']*'\\)*"
  "Regular expression that matches a non-empty start tag.
Any terminating > or / is not matched.")


(defvar xml-font-lock-keywords
  '(("<\\([!?][a-z0-9]+\\)" 1 font-lock-keyword-face)
    ("<\\(/?[a-z0-9]+\\)" 1 font-lock-function-name-face)
    ("&[-._A-Za-z0-9]+;?" . font-lock-variable-name-face))
  "*Rules for highlighting SGML code.  See also `xml-tag-face-alist'.")

;; internal
(defvar xml-font-lock-keywords-1 ())

(defvar xml-face-tag-alist ()
  "Alist of face and tag name for facemenu.")

(defvar xml-tag-face-alist ()
  "Tag names and face or list of faces to fontify with when invisible.
When `font-lock-maximum-decoration' is 1 this is always used for fontifying.
When more these are fontified together with `xml-font-lock-keywords'.")


(defvar xml-display-text ()
  "Tag names as lowercase symbols, and display string when invisible.")

;; internal
(defvar xml-tags-invisible nil)


(defvar xml-tag-alist
  '()
  "*Alist of tag names for completing read and insertion rules.
This alist is made up as

  ((\"tag\" . TAGRULE)
   ...)

TAGRULE is a list of optionally `t' (no endtag) or `\\n' (separate endtag by
newlines) or a skeleton with `nil', `t' or `\\n' in place of the interactor
followed by an ATTRIBUTERULE (for an always present attribute) or an
attribute alist.

The attribute alist is made up as

  ((\"attribute\" . ATTRIBUTERULE)
   ...)

ATTRIBUTERULE is a list of optionally `t' (no value when no input) followed by
an optional alist of possible values.")

(defvar xml-tag-help
  '(("!" . "Empty declaration for comment")
    ("![" . "Embed declarations with parser directive")
    ("!attlist" . "Tag attributes declaration")
    ("!doctype" . "Document type (DTD) declaration")
    ("!element" . "Tag declaration")
    ("!entity" . "Entity (macro) declaration"))
  "*Alist of tag name and short description.")


;; put read-only last to enable setting this even when read-only enabled
(or (get 'xml-tag 'invisible)
    (setplist 'xml-tag
	      (append '(invisible t
			rear-nonsticky t
			point-entered xml-point-entered
			read-only t)
		      (symbol-plist 'xml-tag))))



(defun xml-mode-common (xml-tag-face-alist xml-display-text)
  "Common code for setting up `xml-mode' and derived modes.
XML-TAG-FACE-ALIST is used for calculating `xml-font-lock-keywords-1'.
XML-DISPLAY-TEXT sets up alternate text for when tags are invisible (see
varables of same name)."
  (kill-all-local-variables)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table xml-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'xml-saved-validate-command)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'xml-tags-invisible)
  (make-local-variable 'skeleton-transformation)
  (make-local-variable 'skeleton-further-elements)
  (make-local-variable 'skeleton-end-hook)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'xml-font-lock-keywords-1)
  (make-local-variable 'facemenu-add-face-function)
  (make-local-variable 'facemenu-end-add-face)
  ;;(make-local-variable 'facemenu-remove-face-function)
  (and xml-tag-face-alist
       (not (assq 1 xml-tag-face-alist))
       (nconc xml-tag-face-alist
	      `((1 (,(concat "<\\("
			     (mapconcat 'car xml-tag-face-alist "\\|")
			     "\\)\\([ \t].+\\)?>\\(.+\\)</\\1>")
		    3 (cdr (assoc (match-string 1) ',xml-tag-face-alist)))))))
  (setq indent-line-function 'indent-relative-maybe
	;; A start or end tag by itself on a line separates a paragraph.
	;; This is desirable because SGML discards a newline that appears
	;; immediately after a start tag or immediately before an end tag.
	paragraph-start "^[ \t\n]\\|\
\\(</?\\([A-Za-z]\\([-_.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$\\)"
	paragraph-separate "^[ \t\n]*$\\|\
^</?\\([A-Za-z]\\([-_.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$"
	comment-start "<!-- "
	comment-end " -->"
	comment-indent-function 'xml-comment-indent
	;; This will allow existing comments within declarations to be
	;; recognized.
	comment-start-skip "--[ \t]*"
	skeleton-transformation 'identity
	skeleton-further-elements '((completion-ignore-case t))
	skeleton-end-hook (lambda ()
			    (or (eolp)
				(not (or (eq v2 '\n)
					 (eq (car-safe v2) '\n)))
				(newline-and-indent)))
	xml-font-lock-keywords-1 (cdr (assq 1 xml-tag-face-alist))
	font-lock-defaults '((xml-font-lock-keywords
			      xml-font-lock-keywords-1)
			     nil
			     t)
	facemenu-add-face-function
	  (lambda (face end)
	    (if (setq face (cdr (assq face xml-face-tag-alist)))
		(progn
		  (setq facemenu-end-add-face (concat "</" face ">"))
		  (concat "<" face ">"))
	      (error "Face not configured for %s mode." mode-name))))
  (while xml-display-text
    (put (car (car xml-display-text)) 'before-string
	 (cdr (car xml-display-text)))
    (setq xml-display-text (cdr xml-display-text)))
  (run-hooks 'text-mode-hook 'xml-mode-hook))


;;;###autoload
(defun xml-mode (&optional function)
  "Major mode for editing SGML documents.
Makes > match <.  Makes / blink matching /.
Keys <, &, SPC within <>, \" and ' can be electric depending on
`xml-quick-keys'.

Do \\[describe-variable] xml- SPC to see available variables.

Use \\[xml-validate] to validate your document with an SGML parser.
\\{xml-mode-map}"
  (interactive)
  (xml-mode-common xml-tag-face-alist xml-display-text)
  (use-local-map xml-mode-map)
  (setq mode-name "SGML"
	major-mode 'xml-mode))



(defun xml-comment-indent ()
  (if (and (looking-at "--")
	   (not (and (eq (preceding-char) ?!)
		     (eq (char-after (- (point) 2)) ?<))))
      (progn
	(skip-chars-backward " \t")
	(max comment-column (1+ (current-column))))
    0))



(defun xml-slash ()
  "Insert / and if it follows <, close the matching tag."
  (interactive)
  (insert "/")
  (let ((n nil))
    (save-excursion
      (backward-char 2)
      (if (looking-at "<")
	  (setq n (xml-goto-element-start)) ) )
    (if n (progn (insert n) (insert ">")))
    ) )


(defun xml-name-char (&optional char)
  "Insert a symbolic character name according to `xml-char-names'.
8 bit chars may be inserted with the meta key as in M-SPC for no break space,
or M-- for a soft hyphen."
  (interactive "*")
  (insert ?&)
  (or char
      (setq char (read-quoted-char)))
  (delete-backward-char 1)
  (insert char)
  (undo-boundary)
  (delete-backward-char 1)
  (insert ?&
	  (or (aref xml-char-names char)
	      (format "#%d" char))
	  ?\;))


(defun xml-name-self ()
  "Insert a symbolic character name according to `xml-char-names'."
  (interactive "*")
  (xml-name-char last-command-char))


(defun xml-maybe-name-self ()
  "Insert a symbolic character name according to `xml-char-names'."
  (interactive "*")
  (if xml-name-8bit-mode
      (xml-name-char last-command-char)
    (self-insert-command 1)))


(defun xml-name-8bit-mode ()
  "Toggle insertion of 8 bit characters."
  (interactive)
  (setq xml-name-8bit-mode (not xml-name-8bit-mode)))


(defun xml-tag ()
  "Insert a tag or insert tags around the region."
  (interactive)
  (let ((name (completing-read "Tag: " xml-tag-alist))
	)
    (cond
     ((string= "/" name) (xml-slash))

     ((string-match "/$" name)
      (insert "<")
      (insert name)
      (insert ">")
      )

     ((string= "![" name) ;; CDATA section; e.g. scripts

      (save-excursion
	(goto-char (min (point) (mark)))
	(insert "<![CDATA[")
	)
      (goto-char (max (point) (mark)))
      (insert "]]>")
      )

     (t
      (save-excursion
	(goto-char (min (point) (mark)))
	(insert "<")
	(insert name)
	(insert ">")
	)
      (save-excursion
	(goto-char (max (point) (mark)))
	(insert "</")
	(insert name)
	(insert ">") )
      )

     )
    )
  )

(define-skeleton xml-tag
  "Insert a tag you are prompted for, optionally with attributes.
Completion and configuration is according to `xml-tag-alist'.
If you like tags and attributes in uppercase set `skeleton-transformation'
to `upcase'."
  (funcall skeleton-transformation
	   (completing-read "Tag: " xml-tag-alist))
  ?< (setq v1 (eval str)) |
  (("") -1 '(undo-boundary) "&lt;") |
  (("") '(setq v2 (xml-attributes v1 t)) (if (eq v2 t) " /") ?>
   (if (string= "![" v1)
       (prog1 '(("") " [ " _ " ]]")
	 (backward-char))
     (if (or (eq v2 t)
	     (string-match "^[/!?]" v1))
	 ()
       (if (symbolp v2)
	   '(("") v2 _ v2 "</" v1 ?>)
	 (if (eq (car v2) t)
	     (cons '("") (cdr v2))
	   (append '(("") (car v2))
		   (cdr v2)
		   '(resume: (car v2) _ "</" v1 ?>))))))))

(autoload 'skeleton-read "skeleton")

(defun xml-attributes (alist &optional quiet)
  "When at toplevel of a tag, interactively insert attributes."
  (interactive (list (save-excursion (xml-beginning-of-tag t))))
  (or (stringp alist) (error "Wrong context for adding attribute"))
  (if alist
      (let (car attribute i)
	(setq alist (cdr (assoc (downcase alist) xml-tag-alist)))
	(if (or (symbolp (car alist))
		(symbolp (car (car alist))))
	    (setq car (car alist)
		  alist (cdr alist)))
	(or quiet
	    (message "No attributes configured."))
	(if (stringp (car alist))
	    (progn
	      (insert (if (eq (preceding-char) ? ) "" ? ) (car alist))
	      (xml-value alist))
	  (setq i (length alist))
	  (while (> i 0)
	    (insert ? )
	    (insert (funcall skeleton-transformation
			     (setq attribute
				   (skeleton-read '(completing-read
						    "[Attribute]: "
						    alist)))))
	    (if (string= "" attribute)
		(setq i 0)
	      (xml-value (assoc attribute alist))
	      (setq i (1- i))))
	  (if (eq (preceding-char) ? )
	      (delete-backward-char 1)))
	car)))

(defun xml-auto-attributes (arg)
  "Self insert, except, when at top level of tag, prompt for attributes.
With prefix ARG only self insert."
  (interactive "*P")
  (let ((point (point))
	tag)
    (if (or arg
	    (not xml-tag-alist)	; no message when nothing configured
	    (symbolp (setq tag (save-excursion (xml-beginning-of-tag t))))
	    (eq (aref tag 0) ?/))
	(self-insert-command (prefix-numeric-value arg))
      (xml-attributes tag)
      (setq last-command-char ? )
      (or (> (point) point)
	  (self-insert-command 1)))))


(defun xml-tag-help (&optional tag)
  "Display description of optional TAG or tag at point."
  (interactive)
  (or tag
      (save-excursion
	(if (eq (following-char) ?<)
	    (forward-char))
	(setq tag (xml-beginning-of-tag))))
  (or (stringp tag)
      (error "No tag selected"))
  (setq tag (downcase tag))
  (message "%s"
	   (or (cdr (assoc tag xml-tag-help))
	       (and (eq (aref tag 0) ?/)
		    (cdr (assoc (substring tag 1) xml-tag-help)))
	       "No description available")))


(defun xml-maybe-end-tag ()
  "Name self unless in position to end a tag."
  (interactive)
  (or (condition-case nil
	  (save-excursion (up-list -1))
	(error
	 (xml-name-self)
	 t))
      (condition-case nil
	  (progn
	    (save-excursion (up-list 1))
	    (xml-name-self))
	(error (self-insert-command 1)))))


(defun xml-skip-tag-backward (arg)
  "Skip to beginning of tag or matching opening tag if present.
With prefix ARG, repeat that many times."
  (interactive "p")
  (while (>= arg 1)
    (search-backward "<" nil t)
    (if (looking-at "</\\([^ \n\t>]+\\)")
	;; end tag, skip any nested pairs
	(let ((case-fold-search t)
	      (re (concat "</?" (regexp-quote (match-string 1)))))
	  (while (and (re-search-backward re nil t)
		      (eq (char-after (1+ (point))) ?/))
	    (forward-char 1)
	    (xml-skip-tag-backward 1))))
    (setq arg (1- arg))))

(defun xml-goto-element-start ()
  "Skip to beginning of enclosing element and return its name"
  (interactive)
  (let ((found nil))
    (while (not found)
      (search-backward "<" nil t)
      (if (looking-at "</\\([^ \n\t>]+\\)")
	  ;; end tag, skip any nested pairs
	  (let ((case-fold-search t)
		(re (concat "</?" (regexp-quote (match-string 1)))))
	    (while (and (re-search-backward re nil t)
			(eq (char-after (1+ (point))) ?/))
	      (forward-char 1)
	      (xml-skip-tag-backward 1)))
	(if (looking-at "<\\([^ \n\t>]+\\)")
	    (setq found (match-string 1))) ) )
    found) )

(defun xml-skip-tag-forward (arg &optional return)
  "Skip to end of tag or matching closing tag if present.
With prefix ARG, repeat that many times.
Return t iff after a closing tag."
  (interactive "p")
  (setq return t)
  (while (>= arg 1)
    (skip-chars-forward "^<>")
    (if (eq (following-char) ?>)
	(up-list -1))
    (if (looking-at "<\\([^/ \n\t>]+\\)")
	;; start tag, skip any nested same pairs _and_ closing tag
	(let ((re (concat "</?" (regexp-quote (match-string 1))))
	      point close)
	  (forward-list 1)
	  (setq point (point))
	  (while (and (re-search-forward re nil t)
		      (not (setq close
				 (eq (char-after (1+ (match-beginning 0))) ?/)))
		      (not (up-list -1))
		      (xml-skip-tag-forward 1))
	    (setq close nil))
	  (if close
	      (up-list 1)
	    (goto-char point)
	    (setq return)))
      (forward-list 1))
    (setq arg (1- arg)))
  return)

(defun xml-delete-tag (arg)
  "Delete tag on or after cursor, and matching closing or opening tag.
With prefix ARG, repeat that many times."
  (interactive "p")
  (while (>= arg 1)
    (save-excursion
      (let* (close open)
	(if (looking-at "[ \t\n]*<")
	    ;; just before tag
	    (if (eq (char-after (match-end 0)) ?/)
		;; closing tag
		(progn
		  (setq close (point))
		  (goto-char (match-end 0))))
	  ;; on tag?
	  (or (save-excursion (setq close (xml-beginning-of-tag)
				    close (and (stringp close)
					       (eq (aref close 0) ?/)
					       (point))))
	      ;; not on closing tag
	      (let ((point (point)))
		(xml-skip-tag-backward 1)
		(if (or (not (eq (following-char) ?<))
			(save-excursion
			  (forward-list 1)
			  (<= (point) point)))
		    (error "Not on or before tag")))))
	(if close
	    (progn
	      (xml-skip-tag-backward 1)
	      (setq open (point))
	      (goto-char close)
	      (kill-sexp 1))
	  (setq open (point))
	  (xml-skip-tag-forward 1)
	  (backward-list)
	  (forward-char)
	  (if (eq (aref (xml-beginning-of-tag) 0) ?/)
	      (kill-sexp 1)))
	(goto-char open)
	(kill-sexp 1)))
    (setq arg (1- arg))))



(defun xml-tags-invisible (arg)
  "Toggle visibility of existing tags."
  (interactive "P")
  (let ((modified (buffer-modified-p))
	(inhibit-read-only t)
	(point (point-min))
	symbol)
    (save-excursion
      (goto-char point)
      (if (setq xml-tags-invisible
		(if arg
		    (>= (prefix-numeric-value arg) 0)
		  (not xml-tags-invisible)))
	  (while (re-search-forward "<\\([!/?A-Za-z][-A-Za-z0-9]*\\)"
				    nil t)
	    (setq symbol (intern-soft (downcase (match-string 1))))
	    (goto-char (match-beginning 0))
	    (and (get symbol 'before-string)
		 (not (overlays-at (point)))
		 (overlay-put (make-overlay (point)
					    (match-beginning 1))
			      'category symbol))
	    (put-text-property (setq point (point)) (forward-list)
			       'intangible (point))			
	    (put-text-property point (point)
			       'category 'xml-tag))
	(while (< (setq point (next-overlay-change point)) (point-max))
	  (delete-overlay (car (overlays-at point))))
	(remove-text-properties (point-min) (point-max)
				'(category xml-tag intangible t))))
    (set-buffer-modified-p modified)
    (run-hooks 'xml-tags-invisible-hook)
    (message "")))

(defun xml-point-entered (x y)
  ;; Show preceding or following hidden tag, depending of cursor direction.
  (let ((inhibit-point-motion-hooks t))
    (save-excursion
      (message "Invisible tag: %s"
	       (buffer-substring
		(point)
		(if (or (and (> x y)
			     (not (eq (following-char) ?<)))
			(and (< x y)
			     (eq (preceding-char) ?>)))
		    (backward-list)
		  (forward-list)))))))


(autoload 'compile-internal "compile")

(defun xml-validate (command)
  "Validate an SGML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-string "Validate command: "
		      (or xml-saved-validate-command
			  (concat xml-validate-command
				  " "
				  (let ((name (buffer-file-name)))
				    (and name
					 (file-name-nondirectory name))))))))
  (setq xml-saved-validate-command command)
  (compile-internal command "No more errors"))


(defun xml-beginning-of-tag (&optional top-level)
  "Skip to beginning of tag and return its name.
Else `t'."
  (or (if top-level
	  (condition-case nil
	      (up-list -1)
	    (error t))
	(>= (point)
	    (if (search-backward "<" nil t)
		(save-excursion
		  (forward-list)
		  (point))
	      0)))
      (if (looking-at "<[!?/]?[[A-Za-z][A-Za-z0-9]*")
	  (buffer-substring-no-properties
	   (1+ (point))
	   (match-end 0))
	t)))

(defun xml-value (alist)
  (setq alist (cdr alist))
  (if (stringp (car alist))
      (insert "=\"" (car alist) ?\")
    (if (eq (car alist) t)
	(if (cdr alist)
	    (progn
	      (insert "=\"")
	      (setq alist (skeleton-read '(completing-read
					   "[Value]: " (cdr alist))))
	      (if (string< "" alist)
		  (insert (funcall skeleton-transformation alist) ?\")
		(delete-backward-char 2))))
      (insert "=\"")
      (if alist
	  (insert (funcall skeleton-transformation
			   (skeleton-read '(completing-read "Value: " alist)))))
      (insert ?\"))))

(provide 'xml-mode)

(defvar xhtml-quick-keys xml-quick-keys
  "Use C-c X combinations for quick insertion of frequent tags when non-nil.
This defaults to `xml-quick-keys'.
This takes effect when first loading the library.")

(defvar xhtml-mode-map
  (let ((map (nconc (make-sparse-keymap) xml-mode-map))
	(menu-map (make-sparse-keymap "XHTML")))
    (define-key map "\C-c6" 'xhtml-headline-6)
    (define-key map "\C-c5" 'xhtml-headline-5)
    (define-key map "\C-c4" 'xhtml-headline-4)
    (define-key map "\C-c3" 'xhtml-headline-3)
    (define-key map "\C-c2" 'xhtml-headline-2)
    (define-key map "\C-c1" 'xhtml-headline-1)
    (define-key map "\C-c\r" 'xhtml-paragraph)
    (define-key map "\C-cb" 'xhtml-line)
    (define-key map "\C-c\C-c-" 'xhtml-horizontal-rule)
    (define-key map "\C-c\C-co" 'xhtml-ordered-list)
    (define-key map "\C-c\C-cu" 'xhtml-unordered-list)
    (define-key map "\C-c\C-cr" 'xhtml-radio-buttons)
    (define-key map "\C-c\C-cc" 'xhtml-checkboxes)
    (define-key map "\C-c\C-cl" 'xhtml-list-item)
    (define-key map "\C-c\C-ch" 'xhtml-href-anchor)
    (define-key map "\C-c\C-cn" 'xhtml-name-anchor)
    (define-key map "\C-c\C-ci" 'xhtml-image)
    (if xhtml-quick-keys
	(progn
	  (define-key map "\C-c-" 'xhtml-horizontal-rule)
	  (define-key map "\C-co" 'xhtml-ordered-list)
	  (define-key map "\C-cu" 'xhtml-unordered-list)
	  (define-key map "\C-cr" 'xhtml-radio-buttons)
	  (define-key map "\C-cc" 'xhtml-checkboxes)
	  (define-key map "\C-cl" 'xhtml-list-item)
	  (define-key map "\C-ch" 'xhtml-href-anchor)
	  (define-key map "\C-cn" 'xhtml-name-anchor)
	  (define-key map "\C-ci" 'xhtml-image)))
    (define-key map "\C-c\C-s" 'xhtml-autoview-mode)
    (define-key map "\C-c\C-v" 'browse-url-of-buffer)
    (define-key map [menu-bar html] (cons "XHTML" menu-map))
    (define-key menu-map [xhtml-autoview-mode]
      '("Toggle Autoviewing" . xhtml-autoview-mode))
    (define-key menu-map [browse-url-of-buffer]
      '("View Buffer Contents" . browse-url-of-buffer))
    (define-key menu-map [nil] '("--"))
    ;;(define-key menu-map "6" '("Heading 6" . xhtml-headline-6))
    ;;(define-key menu-map "5" '("Heading 5" . xhtml-headline-5))
    ;;(define-key menu-map "4" '("Heading 4" . xhtml-headline-4))
    (define-key menu-map "3" '("Heading 3" . xhtml-headline-3))
    (define-key menu-map "2" '("Heading 2" . xhtml-headline-2))
    (define-key menu-map "1" '("Heading 1" . xhtml-headline-1))
    (define-key menu-map "l" '("Radio Buttons" . xhtml-radio-buttons))
    (define-key menu-map "c" '("Checkboxes" . xhtml-checkboxes))
    (define-key menu-map "l" '("List Item" . xhtml-list-item))
    (define-key menu-map "u" '("Unordered List" . xhtml-unordered-list))
    (define-key menu-map "o" '("Ordered List" . xhtml-ordered-list))
    (define-key menu-map "-" '("Horizontal Rule" . xhtml-horizontal-rule))
    (define-key menu-map "\n" '("Line Break" . xhtml-line))
    (define-key menu-map "\r" '("Paragraph" . xhtml-paragraph))
    (define-key menu-map "i" '("Image" . xhtml-image))
    (define-key menu-map "h" '("Href Anchor" . xhtml-href-anchor))
    (define-key menu-map "n" '("Name Anchor" . xhtml-name-anchor))
    map)
  "Keymap for commands for use in HTML mode."
  )


(defvar xhtml-face-tag-alist
  '((bold . "b")
    (italic . "i")
    (underline . "u")
    (modeline . "rev"))
  "Value of `xml-face-tag-alist' for HTML mode.")

(defvar xhtml-tag-face-alist
  '(("b" . bold)
    ("big" . bold)
    ("blink" . highlight)
    ("cite" . italic)
    ("em" . italic)
    ("h1" bold underline)
    ("h2" bold-italic underline)
    ("h3" italic underline)
    ("h4" . underline)
    ("h5" . underline)
    ("h6" . underline)
    ("i" . italic)
    ("rev"  . modeline)
    ("s" . underline)
    ("small" . default)
    ("strong" . bold)
    ("title" bold underline)
    ("tt" . default)
    ("u" . underline)
    ("var" . italic))
  "Value of `xml-tag-face-alist' for HTML mode.")


(defvar xhtml-display-text
  '((img . "[/]")
    (hr . "----------")
    (li . "o "))
  "Value of `xml-display-text' for HTML mode.")


; should code exactly HTML 3 here when that is finished
(defvar xhtml-tag-alist
  (let* ((1-9 '(("8") ("9")
		("1") ("2") ("3") ("4") ("5") ("6") ("7")))
	 (align '(("align" ("left") ("center") ("right"))))
	 (valign '(("top") ("middle") ("bottom") ("baseline")))
	 (rel '(("next") ("previous") ("parent") ("subdocument") ("made")))
	 (href '("href" ("ftp:") ("file:") ("finger:") ("gopher:") ("http:")
		 ("mailto:") ("news:") ("rlogin:") ("telnet:") ("tn3270:")
		 ("wais:") ("/cgi-bin/")))
	 (name '("name"))
	 (link `(,href
		 ("rel" ,@rel)
		 ("rev" ,@rel)
		 ("title")))
	 (list '((nil \n
		      ( "List item: "
			"<li>" str "</li>" \n))
		 ("type" ("A") ("a") ("I") ("i") ("1"))))
	 (cell `(t
		 ,align
		 ("valign" ,@valign)
		 ("colspan" ,@1-9)
		 ("rowspan" ,@1-9)
		 ("nowrap" t))))
    ;; put ,-expressions first, else byte-compile chokes (as of V19.29)
    ;; and like this it's more efficient anyway
    `(("a" ,name ,@link)
      ("base" t ,@href)
      ("dir" ,@list)
      ("form" (\n _ \n "<input type=\"submit\" value=\"\">")
       ("action" ,@(cdr href)) ("method" ("get") ("post")))
      ("div" ,@align)
      ("h1" ,@align)
      ("h2")
      ("h3")
      ("h4")
      ("h5")
      ("h6")
      ("hr" t )
      ("img" t ("align" ,@valign ("texttop") ("absmiddle") ("absbottom"))
       ("src") ("alt") ("width" "1") ("height" "1")
       ("border" "1") ("vspace" "1") ("hspace" "1") ("ismap" t))
      ("input" t ("size" ,@1-9) ("maxlength" ,@1-9) ("checked" t) ,name
       ("type" ("text") ("password") ("checkbox") ("radio")
	("submit") ("reset"))
       ("value"))
      ("link" t ,@link)
      ("menu" ,@list)
      ("ol" ,@list)
      ("p" \n)
      ("select" (nil \n
		     ("Text: "
		      "<option>" str "</option>" \n))
       ,name ("size" ,@1-9) ("multiple" t))
      ("table" (nil \n
		    ((completing-read "Cell kind: " '(("td") ("th"))
				      nil t "t")
		     "<tr><" str ?> _ \n))
       ("border" t ,@1-9) ("width" "10") ("cellpadding"))
      ("td" ,@cell)
      ("textarea" ,name ("rows" ,@1-9) ("cols" ,@1-9))
      ("th" ,@cell)
      ("ul" ,@list)

      ,@xml-tag-alist

      ("abbrev")
      ("acronym")
      ("address" \n)
      ("au")
      ("b")
      ("big")
      ("blink")
      ("blockquote" \n)
      ("body" \n)
      ("br" t)
      ("caption" ("valign" ("top") ("bottom")))
      ("cite")
      ("code")
      ("dd" t)
      ("del")
      ("dfn")
      ("dl" (nil \n
		 ( "Term: "
		   "<dt>" str "</dt>" "<dd>" _ "</dd>" \n)))
      ("dt" (t _ "</dt><dd></dd>"))
      ("em")
      ("head" \n)
      ("html" (\n
	       "<head>\n"
	       "<title>" (setq str (read-input "Title: ")) "</title>\n"
	       "<body>\n<h1>" str "</h1>\n" _
	       "\n<address>\n<a href=\"mailto:"
	       user-mail-address
	       "\">" (user-full-name) "</a>\n</address>")
              ("xmlns" "http://www.w3.org/1999/xhtml"))
      ("i")
      ("ins")
      ("kbd")
      ("li" t)
      ("nobr")
      ("option" t ("value") ("label") ("selected" t))
      ("pre" \n)
      ("q")
      ("samp")
      ("small")
      ("strong")
      ("sub")
      ("sup")
      ("title")
      ("tr" \n)
      ("tt")
      ("u")
      ("var")
      ("wbr" t)))
  "*Value of `xml-tag-alist' for HTML mode.")

(defvar xhtml-tag-help
  `(,@xml-tag-help
    ("a" . "Anchor of point or link elsewhere")
    ("abbrev" . "?")
    ("acronym" . "?")
    ("address" . "Signature of page or blockquote")
    ("b" . "Bold face")
    ("base" . "Base address for relative URI references")
    ("big" . "Font size")
    ("blink" . "Blinking text")
    ("blockquote" . "Block quotation")
    ("body" . "Document body")
    ("br" . "Line break")
    ("caption" . "Table caption")
    ("cite" . "Citation of a document")
    ("code" . "Formatted source code")
    ("dd" . "Definition of term")
    ("del" . "deleted text")
    ("dfn" . "defining instance of a term")
    ("dir" . "Directory list (obsolete)")
    ("dl" . "Definition list")
    ("dt" . "Term to be definined")
    ("em" . "Emphasised") 
    ("embed" . "Embedded data in foreign format")
    ("form" . "Form with input fields")
    ("h1" . "Most important section headline")
    ("h2" . "Important section headline")
    ("h3" . "Section headline")
    ("h4" . "Minor section headline")
    ("h5" . "Unimportant section headline")
    ("h6" . "Least important section headline")
    ("head" . "Document header")
    ("hr" . "Horizontal rule")
    ("html" . "HTML Document")
    ("i" . "Italic face")
    ("img" . "Graphic image")
    ("input" . "Form input field")
    ("ins" . "inserted text")
    ("kbd" . "Keybard example face")
    ("li" . "List item")
    ("link" . "Document link, with relationship")
    ("menu" . "Menu list (obsolete)")
    ("nobr" . "Text without line break")
    ("ol" . "Ordered list")
    ("option" . "Selection list item")
    ("p" . "Paragraph start")
    ("pre" . "Preformatted fixed width text")
    ("q" . "short quote")
    ("samp" . "Sample text")
    ("select" . "Selection list")
    ("small" . "Font size")
    ("strong" . "Standout text")
    ("sub" . "Subscript")
    ("sup" . "Superscript")
    ("table" . "Table with rows and columns")
    ("td" . "Table data cell")
    ("textarea" . "Form multiline edit area")
    ("th" . "Table header cell")
    ("title" . "Document title")
    ("tr" . "Table row separator")
    ("tt" . "Typewriter face")
    ("u" . "Underlined text")
    ("ul" . "Unordered list")
    ("var" . "Math variable face")
    ("wbr" . "Enable <br /> within <nobr>"))
"*Value of `xml-tag-help' for HTML mode.")



;;;###autoload
(defun xhtml-mode ()
  "Major mode based on SGML mode for editing HTML documents.
This allows inserting skeleton costructs used in hypertext documents with
completion.  See below for an introduction to HTML.  Use
\\[browse-url-of-buffer] to see how this comes out.  See also `xml-mode' on
which this is based.

Do \\[describe-variable] xhtml- SPC and \\[describe-variable] xml- SPC to see available variables.

To write fairly well formatted pages you only need to know few things.  Most
browsers have a function to read the source code of the page being seen, so
you can imitate various tricks.  Here's a very short HTML primer which you
can also view with a browser to see what happens:

<title>A Title Describing Contents</title> should be on every page.  Pages can
have <h1>Very Major Headlines</h1> through <h6>Very Minor Headlines</h6>
<hr /> Parts can be separated with horizontal rules.

<p>Paragraphs appear like so</p>.  Line breaks and multiple spaces are
ignored unless the text is <pre>preformatted.</pre>  Text can be marked as
<b>bold</b>, <i>italic</i> or <u>underlined</u> using the normal  M-g  or
Edit/Text Properties/Face commands.

Pages can have <a name=\"SOMENAME\">named points</a> and can link other points
to them with <a href=\"#SOMENAME\">see also somename</a>.  In the same way <a
href=\"URL\">see also URL</a> where URL is a filename relative to current
directory or something like http://www.cs.indiana.edu/elisp/w3/docs.html.

Images in many formats can be inlined with <img src=\"URL\" />.

If you mainly create your own documents, `xml-specials' might be interesting.
But note that some HTML 2 browsers can't handle &apos;.  To work around that
do:

\(eval-after-load \"xml-mode\" '(aset xml-char-names ?' nil))
\\{xhtml-mode-map}"
  (interactive)
  (xml-mode-common xhtml-tag-face-alist xhtml-display-text)
  (use-local-map xhtml-mode-map)
  (make-local-variable 'xml-tag-alist)
  (make-local-variable 'xml-face-tag-alist)
  (make-local-variable 'xml-tag-help)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-heading-end-regexp)
  (make-local-variable 'outline-level)
  (setq mode-name "XHTML"
        major-mode 'xhtml-mode
	xml-tag-alist xhtml-tag-alist
	xml-face-tag-alist xhtml-face-tag-alist
	xml-tag-help xhtml-tag-help
	outline-regexp "^.*<[Hh][1-6]\\>"
	outline-heading-end-regexp "</[Hh][1-6]>"
	outline-level (lambda ()
			(char-after (1- (match-end 0)))))
  (run-hooks 'xhtml-mode-hook))


(define-skeleton xhtml-href-anchor
  "HTML anchor tag with href attribute."
  nil
  "<a href=\"http:" _ "\"></a>")

(define-skeleton xhtml-name-anchor
  "HTML anchor tag with name attribute."
  nil
  "<a name=\"" _ "\"></a>")

(define-skeleton xhtml-headline-1
  "HTML level 1 headline tags."
  nil
  "<h1>" _ "</h1>")

(define-skeleton xhtml-headline-2
  "HTML level 2 headline tags."
  nil
  "<h2>" _ "</h2>")

(define-skeleton xhtml-headline-3
  "HTML level 3 headline tags."
  nil
  "<h3>" _ "</h3>")

(define-skeleton xhtml-headline-4
  "HTML level 4 headline tags."
  nil
  "<h4>" _ "</h4>")

(define-skeleton xhtml-headline-5
  "HTML level 5 headline tags."
  nil
  "<h5>" _ "</h5>")

(define-skeleton xhtml-headline-6
  "HTML level 6 headline tags."
  nil
  "<h6>" _ "</h6>")

(define-skeleton xhtml-horizontal-rule
  "HTML horizontal rule tag."
  nil
  "<hr />" \n)

(define-skeleton xhtml-image
  "HTML image tag."
  nil
  "<img src=\"http:" _ "\" />")

(define-skeleton xhtml-line
  "HTML line break tag."
  nil
  "<br />" \n)

(define-skeleton xhtml-ordered-list
  "HTML ordered list tags."
  nil
  ?< "ol>" \n
  "<li>" _ "</li>" \n
  "</ol>")

(define-skeleton xhtml-unordered-list
  "HTML unordered list tags."
  nil
  ?< "ul>" \n
  "<li>" _ "</li>" \n
  "</ul>")

(define-skeleton xhtml-list-item
  "HTML list item tag."
  nil
  (if (bolp) nil '\n)
  "<li>" _ "</li>")

(define-skeleton xhtml-paragraph
  "HTML paragraph tag."
  nil
  (if (bolp) nil ?\n)
  \n "<p>" _ "</p>")

(define-skeleton xhtml-checkboxes
  "Group of connected checkbox inputs."
  nil
  '(setq v1 (eval str))			; allow passing name as argument
  ("Value & Text: "
   "<input type=\"checkbox\" name=\""
   (or v1 (setq v1 (skeleton-read "Name: ")))
   "\" value=\"" str ?\"
   (if v2 "" " checked='checked'") ?/ ?> str
   (or v2 (setq v2 (if (y-or-n-p "Newline? ") "<br />" ""))) \n))

(define-skeleton xhtml-radio-buttons
  "Group of connected radio button inputs."
  nil
  '(setq v1 (eval str))			; allow passing name as argument
  ("Value & Text: "
   "<input type=\"radio\" name=\""
   (or v1 (setq v1 (skeleton-read "Name: ")))
   "\" value=\"" str ?\"
   (if v2 "" " checked") ?> str
   (or v2 (setq v2 (if (y-or-n-p "Newline? ") "<br />" ""))) \n))


(defun xhtml-link-ref ()
  "Link the region to the yank'd address."
  (interactive)
  (goto-char (max (point) (mark)))
  (save-excursion
    (goto-char (min (point) (mark)))
    (insert (format "<a name=\"N%d\" href=\"" (random)))
    (yank) ;@@ need to escape " and & and <
    (insert "\">")
    )
  (goto-char (max (point) (mark)))
  (insert "</a>")
  )


(defun xhtml-autoview-mode (&optional arg)
  "Toggle automatic viewing via `xhtml-viewer' upon saving buffer.
With positive prefix ARG always turns viewing on, with negative ARG always off.
Can be used as a value for `xhtml-mode-hook'."
  (interactive "P")
  (if (setq arg (if arg
		    (< (prefix-numeric-value arg) 0)
		  (and (boundp 'after-save-hook)
		       (memq 'browse-url-of-buffer after-save-hook))))
      (setq after-save-hook (delq 'browse-url-of-buffer after-save-hook))
    (make-local-hook 'after-save-hook)
    (add-hook 'after-save-hook 'browse-url-of-buffer nil t))
  (message "Autoviewing turned %s."
	   (if arg "off" "on")))

;;; xml-mode.el ends here
