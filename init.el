;; Package mirrors
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Customizations that don't like org-babel for some reason (??)

;; disable scroll-bar
(scroll-bar-mode -1)
(set-fringe-mode 0)

;; End of weird customizations

;; use-package installation
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; loads my config file from config.org
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


;; Stuff that emacs told me not to touch, so don't touch it!
