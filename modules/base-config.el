;;; base-config.el -- Base configuration.

;;; Commentary:
;;; Sets up base Emacs to my liking, without any packages or add-ons.
;;; The bare minimum for me to feel comfortable.

;;; Code:

;; disables the Emacs functionality that creates backup files. unnecessary if you use a daemon and don't kill buffers.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; disables the startup screen.
(setq inhibit-startup-screen t)

;; sets the default "yes or no" prompt to "y or n".
(defalias 'yes-or-no-p 'y-or-n-p)

;; terminal settings, setting the default shell to zsh.
(defvar default-shell "/bin/zsh")
(defadvice term (before force-bash)
  (interactive (list default-shell)))
(ad-activate 'term)

;; set scroll mode to be line-by-line.
(setq scroll-conservatively 100)

;; disable toolbar and menubar.
(tool-bar-mode -1)
(menu-bar-mode -1)

;; display line numbers in both programming and configuration modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'hl-line-mode)

;; replace functionality set to work like modern editors.
(delete-selection-mode +1)

;; default font set here later in init.
(add-to-list 'default-frame-alist
	     '(font . "Iosevka-10"))

;; change the suspend keybind to correspond to undo.
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; ensures that in shell, the prompt is read-only (i.e. cannot be backspaced).
(setq comint-prompt-read-only t)

;; ensures that turning the region upper case is enabled.
(put 'upcase-region 'disabled nil)

(provide 'base-config)

;;; base-config.el ends here
