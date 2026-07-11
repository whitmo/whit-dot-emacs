;;; init-yaml --- YAML mode configuration

;;; Commentary:
;; Configuration for YAML editing, including highlighting, formatting, and linting.

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :mode ("\\.yaml\\'" . yaml-mode)
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map (kbd "RET") 'newline-and-indent)))
  :config
  ;; Additional yaml-mode configurations can go here
  )



;; Formatting with yaml-pro (requires Tree-sitter)
(use-package yaml-pro
  :hook (yaml-mode . (lambda ()
                       (require 'yaml-pro)
                       ;; Assuming Tree-sitter is available and grammar is installed
                       (yaml-pro-ts-mode +1)
                       ;; Optional: Bind a key for formatting
                       (define-key yaml-mode-map (kbd "C-c C-f") 'yaml-pro-format))))

(provide 'init-yaml)
