(setq exec-path (cons "/home/whit/.gvm/pkgsets/go1.2.1/global/bin:/home/whit/.gvm/gos/go1.2.1/bin" exec-path))
(setq exec-path (cons "/home/whit/proj/vs/bin" exec-path))
(add-to-list 'exec-path "/Users/tleyden/Development/gocode/bin")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(setenv "PATH" "/home/whit/proj/vs/bin:/home/whit/.gvm/pkgsets/go1.2.1/global/bin:/home/whit/.gvm/gos/go1.2.1/bin:/home/whit/.gvm/pkgsets/go1.2.1/global/overlay/bin:/home/whit/.gvm/bin:/home/whit/.gvm/bin:/home/whit/dev/vs/bin:/home/whit/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/whit/dev/gostuff//bin:/home/whit/.juju-plugins")

(setenv "GOPATH" "/home/whit/proj/vs:/home/whit/.gvm/pkgsets/go1.2.1/global")
(setenv "GOROOT" "/home/whit/.gvm/gos/go1.2.1")
