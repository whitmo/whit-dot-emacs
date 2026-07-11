;;; init-agent-bridge.el --- Small CLI bridge for coding agents -*- lexical-binding: t; -*-

;;; Commentary:
;; Whitelisted Emacs functions callable by local coding agents through
;; emacsclient.  The companion CLI is bin/emacs-agent.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup whit-agent-bridge nil
  "Local bridge between Emacs and coding agents."
  :group 'tools
  :prefix "whit-agent-bridge-")

(defcustom whit-agent-bridge-directory
  (expand-file-name "agent-bridge" user-emacs-directory)
  "Directory used for agent bridge inbox, requests, and responses."
  :type 'directory
  :group 'whit-agent-bridge)

(defun whit-agent-bridge--dir (name)
  "Return bridge subdirectory NAME, creating it when needed."
  (let ((dir (expand-file-name name whit-agent-bridge-directory)))
    (make-directory dir t)
    dir))

(defun whit-agent-bridge-json (object)
  "Encode OBJECT as a JSON string."
  (let ((json-encoding-pretty-print nil))
    (json-encode object)))

(defun whit-agent-bridge--ok (&rest pairs)
  "Return a JSON success object with PAIRS appended."
  (whit-agent-bridge-json (append '((ok . t)) pairs)))

(defun whit-agent-bridge--error (message &rest pairs)
  "Return a JSON error object with MESSAGE and PAIRS."
  (whit-agent-bridge-json (append `((ok . :json-false) (error . ,message)) pairs)))

(defun whit-agent-bridge--decode-args (args-base64)
  "Decode ARGS-BASE64 as a JSON array."
  (let ((json-array-type 'list)
        (json-object-type 'alist)
        (json-false :json-false))
    (if (or (null args-base64) (string-empty-p args-base64))
        nil
      (json-read-from-string (base64-decode-string args-base64)))))

(defun whit-agent-bridge--request-id ()
  "Generate a request id."
  (format-time-string "%Y%m%dT%H%M%S%6N"))

(defun whit-agent-bridge--write-json-file (file object)
  "Write OBJECT as JSON to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (whit-agent-bridge-json object) "\n")))

(defun whit-agent-bridge--append-jsonl (file object)
  "Append OBJECT as one JSONL row to FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (insert (whit-agent-bridge-json object) "\n")
    (append-to-file (point-min) (point-max) file)))

(defun whit-agent-bridge-ping (&rest args)
  "Return a ping response including ARGS."
  (whit-agent-bridge--ok `(message . "pong") `(args . ,args)))

(defun whit-agent-bridge-notify (message &optional title)
  "Show MESSAGE in Emacs and return JSON status."
  (message "%s%s" (if title (format "%s: " title) "Agent: ") message)
  (whit-agent-bridge--ok `(message . ,message) `(title . ,(or title "Agent"))))

(defun whit-agent-bridge-open (file &optional line column)
  "Open FILE at LINE and COLUMN for user attention."
  (let ((path (expand-file-name file)))
    (if (not (file-exists-p path))
        (whit-agent-bridge--error "file does not exist" `(file . ,path))
      (let ((buffer (find-file-noselect path)))
        (pop-to-buffer buffer)
        (when line
          (goto-char (point-min))
          (forward-line (max 0 (1- (truncate line))))
          (when column (move-to-column (max 0 (truncate column)))))
        (message "Agent opened %s%s" path (if line (format ":%s" line) ""))
        (whit-agent-bridge--ok `(file . ,path)
                               `(line . ,(or line :json-null))
                               `(buffer . ,(buffer-name buffer)))))))

(defun whit-agent-bridge-current ()
  "Return information about the current buffer."
  (whit-agent-bridge--ok
   `(buffer . ,(buffer-name))
   `(file . ,(or (buffer-file-name) :json-null))
   `(modified . ,(if (buffer-modified-p) t :json-false))
   `(line . ,(line-number-at-pos))
   `(column . ,(current-column))))

(defun whit-agent-bridge-selection ()
  "Return active region text and location, or an empty selection."
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (whit-agent-bridge--ok
         `(text . ,(buffer-substring-no-properties start end))
         `(file . ,(or (buffer-file-name) :json-null))
         `(start . ,start)
         `(end . ,end)
         `(startLine . ,(line-number-at-pos start))
         `(endLine . ,(line-number-at-pos end))))
    (whit-agent-bridge--ok
     '(text . "")
     `(file . ,(or (buffer-file-name) :json-null))
     '(start . 0)
     '(end . 0))))

(defun whit-agent-bridge-buffers ()
  "Return file-backed open buffers."
  (let (buffers)
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (push `((name . ,(buffer-name buffer))
                (file . ,file)
                (modified . ,(if (buffer-modified-p buffer) t :json-false)))
              buffers)))
    (whit-agent-bridge--ok `(buffers . ,(nreverse buffers)))))

(defun whit-agent-bridge-inbox (message &optional from)
  "Append MESSAGE from FROM to the agent inbox and display it."
  (let* ((entry `((id . ,(whit-agent-bridge--request-id))
                  (from . ,(or from "agent"))
                  (message . ,message)
                  (createdAt . ,(format-time-string "%FT%T%z"))))
         (file (expand-file-name "inbox.jsonl" whit-agent-bridge-directory)))
    (whit-agent-bridge--append-jsonl file entry)
    (with-current-buffer (get-buffer-create "*Agent Inbox*")
      (goto-char (point-max))
      (insert (format "\n[%s] %s\n%s\n"
                      (alist-get 'id entry)
                      (alist-get 'from entry)
                      message))
      (display-buffer (current-buffer)))
    (message "Agent inbox: %s" message)
    (whit-agent-bridge--ok `(id . ,(alist-get 'id entry)) `(inbox . ,file))))

(defun whit-agent-bridge-ask (message &optional from)
  "Create an async request from FROM with MESSAGE."
  (let* ((id (whit-agent-bridge--request-id))
         (request `((id . ,id)
                    (from . ,(or from "agent"))
                    (message . ,message)
                    (createdAt . ,(format-time-string "%FT%T%z"))))
         (file (expand-file-name (concat id ".json")
                                 (whit-agent-bridge--dir "requests"))))
    (whit-agent-bridge--write-json-file file request)
    (whit-agent-bridge-inbox (format "Request %s from %s:\n%s\n\nRespond in Emacs with M-x whit-agent-bridge-respond, or from CLI with: emacs-agent respond %s <text>"
                                     id (or from "agent") message id)
                              from)
    (whit-agent-bridge--ok `(id . ,id) `(request . ,file))))

(defun whit-agent-bridge-respond (id response &optional from)
  "Write RESPONSE for async request ID."
  (interactive
   (list (read-string "Request id: ")
         (read-string "Response: ")
         "emacs"))
  (let ((file (expand-file-name (concat id ".json")
                                (whit-agent-bridge--dir "responses"))))
    (whit-agent-bridge--write-json-file
     file `((id . ,id)
            (from . ,(or from "emacs"))
            (response . ,response)
            (createdAt . ,(format-time-string "%FT%T%z"))))
    (message "Agent bridge response written for %s" id)
    (whit-agent-bridge--ok `(id . ,id) `(response . ,file))))

(defun whit-agent-bridge-cli (command args-base64)
  "Dispatch whitelisted bridge COMMAND with ARGS-BASE64 JSON array."
  (let ((args (whit-agent-bridge--decode-args args-base64)))
    (condition-case err
        (pcase command
          ("ping" (apply #'whit-agent-bridge-ping args))
          ("notify" (apply #'whit-agent-bridge-notify args))
          ("open" (apply #'whit-agent-bridge-open args))
          ("current" (whit-agent-bridge-current))
          ("selection" (whit-agent-bridge-selection))
          ("buffers" (whit-agent-bridge-buffers))
          ("inbox" (apply #'whit-agent-bridge-inbox args))
          ("ask" (apply #'whit-agent-bridge-ask args))
          ("respond" (apply #'whit-agent-bridge-respond args))
          (_ (whit-agent-bridge--error "unknown command" `(command . ,command))))
      (error (whit-agent-bridge--error (error-message-string err)
                                       `(command . ,command))))))

(provide 'init-agent-bridge)
;;; init-agent-bridge.el ends here
