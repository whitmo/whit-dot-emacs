;;; marcopolo-utils.el --- some tools

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'json)
(require 'request)
(require 's)


(require 'marcopolo-api)
(require 'marcopolo-version)


;; Errors

(eval-and-compile
  (unless (fboundp 'define-error)
    ;; Shamelessly copied from Emacs trunk :)
    (defun define-error (name message &optional parent)
      "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
      (unless parent (setq parent 'error))
      (let ((conditions
             (if (consp parent)
                 (apply #'nconc
                        (mapcar (lambda (parent)
                                  (cons parent
                                        (or (get parent 'error-conditions)
                                            (error "Unknown signal `%s'" parent))))
                                parent))
               (cons parent (get parent 'error-conditions)))))
        (put name 'error-conditions
             (delete-dups (copy-sequence (cons name conditions))))
        (when message (put name 'error-message message))))))

(define-error 'marcopolo-error "Marcopolo error")

(define-error 'marcopolo-http-error "HTTP Error" 'marcopolo-error)


;; HTTP tools

(defun marcopolo--get-registry-rest-uri (uri)
  "Retrieve the Docker registry complete url.
`URI` is the api path."
  (let ((host (marcopolo--get-registry-host)))
    (if host
        (s-concat host "/" marcopolo--docker-api-version "/" uri)
      (error (signal 'marcopolo-error '("Docker registry host unknown."))))))

(defun marcopolo--get-hub-rest-uri (uri)
  "Retrieve the Docker Hub complete url.
`URI` is the api path."
  (if marcopolo--hub-host
      (s-concat marcopolo--hub-host "/" marcopolo--docker-api-version "/" uri)
    (error (signal 'marcopolo-error '("Docker Hub host unknown.")))))


(defun marcopolo--get-registry-headers ()
  "Generate HTTP headers for Marcopolo registry API."
  (let ((headers (list (cons "Accept" "application/json")
                       (cons "Content-Type" "application/json")
                       (cons "User-Agent"
                             (s-concat marcopolo--user-agent
                                       "/"
                                       (marcopolo--library-version))))))
    headers))

(defun marcopolo--get-hub-headers ()
  "Generate HTTP headers for Marcopolo Hub API."
  (let* ((auth (base64-encode-string
                (s-concat (marcopolo--get-hub-username)
                          ":"
                          (marcopolo--get-hub-password))))
         (headers
          (list (cons "Accept" "application/json")
                (cons "Content-Type" "application/json")
                (cons "User-Agent"
                      (s-concat marcopolo--user-agent
                                "/"
                                (marcopolo--library-version)))
                (cons "Authorization" (concat "Basic " auth)))))

    headers))


(defun marcopolo--perform-http-request (method uri headers params status-code)
  "Do a HTTP METHOD request using URI, HEADERS and PARAMS.
If HTTP return code is STATUS-CODE, send the response content otherwise
raise an error."
  (when marcopolo-debug
    (message "[MarcoPolo] HTTP Request: %s %s %s" uri headers params))
  (let ((response (request uri ;(marcopolo--get-rest-uri uri)
                           :type method
                           :headers headers ;(marcopolo--get-headers)
                           :sync t
                           :data params
                           :parser 'json-read)))
    (if (= status-code (request-response-status-code response))
        (request-response-data response)
      (error
       (signal 'marcopolo-http-error
               (list (request-response-status-code response)
                     (request-response-data response)))))))

(defun marcopolo--perform-registry-request (method path params status-code)
  (marcopolo--perform-http-request method
                                   (marcopolo--get-registry-rest-uri path)
                                   (marcopolo--get-registry-headers)
                                   params
                                   status-code))

(defun marcopolo--perform-hub-request (method path params status-code)
  (marcopolo--perform-http-request method
                                   (marcopolo--get-hub-rest-uri path)
                                   (marcopolo--get-hub-headers)
                                   params
                                   status-code))

;; Assoc tools

(defun marcopolo--assoc-cdr (key list)
  (let ((result (cdr (assoc key list))))
    (if result
        result
      "")))


(provide 'marcopolo-utils)
;;; marcopolo-utils.el ends here
