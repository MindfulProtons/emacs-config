;;; theme-ui.el --- Theming the Emacs UI.

;;; Commentary:

;;; Theming the Emacs UI with the base16-theme package, where we load the base16-brewer theme.
;;; Loads the emacs-dashboard.
;;; Adds the doom modeline, as well as all-the-icons required by it.

;;; Code:

;; sets up the base16-theme package
(use-package base16-theme
    :config
    (setq base16-distinct-fringe-background nil)
    (load-theme 'base16-brewer t))

;; loads theme whenever emacs is in daemon mode.

  (defvar my:theme 'base16-brewer)
  (defvar my:theme-window-loaded nil)
  (defvar my:theme-terminal-loaded nil)

  (if (daemonp)
      (add-hook 'after-make-frame-functions(lambda (frame)
					    (select-frame frame)
					    (if (window-system frame)
						(unless my:theme-window-loaded
						  (if my:theme-terminal-loaded
						      (enable-theme my:theme)
						    (load-theme my:theme t))
						  (setq my:theme-window-loaded t)
						  )
					      (unless my:theme-terminal-loaded
						(if my:theme-window-loaded
						    (enable-theme my:theme)
						  (load-theme my:theme t))
						(setq my:theme-terminal-loaded t)
						)
					      )))

    (progn
      (load-theme my:theme t)
      (if (display-graphic-p)
	  (setq my:theme-window-loaded t)
	(setq my:theme-terminal-loaded t)))
    )

;; sets up emacs-dashboard.
(use-package dashboard
  :custom
  (dashboard-set-footer nil)
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; sets up doom-modeline.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; sets up all-the-icons.
(use-package all-the-icons)

(provide 'theme-ui)

;;; theme-ui.el ends here
