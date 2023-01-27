;;; init.el --- init file

;;; Commentary:

;;; Initializes Emacs with straight.el, installs use-package, and then follows with loading the modules required.

;; bootstraps straight.el (https://github.com/raxod502/straight.el) and sets use-package up to use straight.

;;; Code:

(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; sets up use-package
(straight-use-package 'use-package)

;; adds to load path
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "modules/")))

;; load modules
(require 'base-config)
(require 'theme-ui)
(require 'code-config)
(require 'org-config)
(require 'completion)

;;; loads my config file from config.org
;; (org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
