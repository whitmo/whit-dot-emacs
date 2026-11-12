;;; agent-bridge-test.el --- Tests for agent bridge -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'init-agent-bridge)

(ert-deftest whit-agent-bridge-encodes-json-objects ()
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (encoded (whit-agent-bridge-json '((ok . t) (value . "hello"))))
         (decoded (json-read-from-string encoded)))
    (should (equal (alist-get 'ok decoded) t))
    (should (equal (alist-get 'value decoded) "hello"))))

(ert-deftest whit-agent-bridge-cli-decodes-base64-json-args ()
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (args (base64-encode-string "[\"agent\"]" t))
         (decoded (json-read-from-string (whit-agent-bridge-cli "ping" args))))
    (should (equal (alist-get 'ok decoded) t))
    (should (equal (alist-get 'message decoded) "pong"))
    (should (equal (alist-get 'args decoded) '("agent")))))

(ert-deftest whit-agent-bridge-ask-writes-request-file ()
  (let* ((tmp (make-temp-file "agent-bridge" t))
         (whit-agent-bridge-directory tmp)
         (json-object-type 'alist)
         (json-array-type 'list)
         (decoded (json-read-from-string
                   (whit-agent-bridge-cli
                    "ask"
                    (base64-encode-string "[\"look here\",\"pi\"]" t))))
         (id (alist-get 'id decoded))
         (path (expand-file-name (concat id ".json")
                                 (expand-file-name "requests" tmp))))
    (should (equal (alist-get 'ok decoded) t))
    (should (file-exists-p path))))

(ert-deftest whit-agent-bridge-open-builds-response-for-existing-file ()
  (let* ((file (make-temp-file "agent-bridge-open" nil ".txt" "hi"))
         (json-object-type 'alist)
         (json-array-type 'list)
         (decoded (json-read-from-string
                   (whit-agent-bridge-cli
                    "open"
                    (base64-encode-string
                     (json-encode (vector file 1)) t)))))
    (should (equal (alist-get 'ok decoded) t))
    (should (equal (alist-get 'file decoded) file))))

;;; agent-bridge-test.el ends here
