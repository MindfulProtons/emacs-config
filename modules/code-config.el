;;; code-config.el --- Configuration and packages for programming.

;;; Commentary:

;;; Used to set up the important parts of my programming environment:
;;; an LSP provider (such as eglot in this example) for IDE-like functionality,
;;; company-mode for completion, and Magit for git integration.

;;; Code:

;; sets up eglot
(use-package eglot
  :hook
  (prog-mode . eglot-ensure))

;; sets up company
(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  ;; recommended settings by lsp-mode
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0
	company-backends '((company-capf company-dabbrev-code))))

;; sets up magit
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(provide 'code-config)

;;; code-config.el ends here
