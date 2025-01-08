;;; hail the hub

(use-package codespaces
  :config (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect))
