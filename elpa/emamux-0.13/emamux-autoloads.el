;;; emamux-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emamux:zoom-runner emamux:clear-runner-history
;;;;;;  emamux:interrupt-runner emamux:inspect-runner emamux:close-panes
;;;;;;  emamux:close-runner-pane emamux:run-last-command emamux:run-command
;;;;;;  emamux:kill-session emamux:yank-from-list-buffers emamux:copy-kill-ring
;;;;;;  emamux:send-command) "emamux" "emamux.el" (21594 19846 523732
;;;;;;  599000))
;;; Generated autoloads from emamux.el

(autoload 'emamux:send-command "emamux" "\
Send command to target-session of tmux

\(fn)" t nil)

(autoload 'emamux:copy-kill-ring "emamux" "\
Set (car kill-ring) to tmux buffer

\(fn ARG)" t nil)

(autoload 'emamux:yank-from-list-buffers "emamux" "\


\(fn)" t nil)

(autoload 'emamux:kill-session "emamux" "\
Kill tmux session

\(fn)" t nil)

(autoload 'emamux:run-command "emamux" "\
Run command

\(fn CMD &optional CMDDIR)" t nil)

(autoload 'emamux:run-last-command "emamux" "\


\(fn)" t nil)

(autoload 'emamux:close-runner-pane "emamux" "\
Close runner pane

\(fn)" t nil)

(autoload 'emamux:close-panes "emamux" "\
Close all panes except current pane

\(fn)" t nil)

(autoload 'emamux:inspect-runner "emamux" "\
Enter copy-mode in runner pane

\(fn)" t nil)

(autoload 'emamux:interrupt-runner "emamux" "\
Send SIGINT to runner pane

\(fn)" t nil)

(autoload 'emamux:clear-runner-history "emamux" "\
Clear history of runner pane

\(fn)" t nil)

(autoload 'emamux:zoom-runner "emamux" "\
Zoom runner pane. This feature requires tmux 1.8 or higher

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("emamux-pkg.el") (21594 19846 536558 485000))

;;;***

(provide 'emamux-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emamux-autoloads.el ends here
