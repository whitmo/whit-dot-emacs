(require 'use-package)

(use-package lua-mode
  :ensure t)

(use-package norns
  ;; Not in the package archive set; keep this as an optional/manual package.
  :ensure nil
  :hook (lua-mode-hook . norns-mode-maybe-activate)
  :bind (
         :map norns-mode-map
         ("C-c e b" . norns-load-current-script)
         ("C-c e r" . norns-send-selection)

         :map norns-maiden-repl-mode-map
         ("C-c e b" . norns-rerun)

         :map norns-sc-repl-mode-map
         ("C-." . norns-sc-stop))
  :config ;; define networking
  (progn
    (setq norns-host "fates")
    (setq norns-mdns-domain "local")
    ;;(setq norns-local-mdns-domain "local")
    )
  )


(provide 'init-music)
