(require 'package)
(add-to-list 'package-archives             
	'("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; customizations that don't play nice with org-babel

;; scroll-bar and fringe mode disabled
(scroll-bar-mode -1)
(set-fringe-mode 0)

;; sets up use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; loads my config file from config.org
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
