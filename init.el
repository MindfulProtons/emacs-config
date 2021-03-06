;; bootstraps straight.el (https://github.com/raxod502/straight.el) and sets use-package up to use straight.

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

;; loads my config file from config.org
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))
