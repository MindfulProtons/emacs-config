;; early-init.el -- by MindfulProtons (https://github.com/MindfulProtons)

(setq gc-cons-threshold most-positive-fixnum)

;; disable package initialization early in the process (code used from doom-emacs)
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; scroll-bar and fringe mode disabled
(scroll-bar-mode -1)
(set-fringe-mode 0)

;; decrease startup time by stopping emacs from resizing the frame
(setq frame-inhibit-implied-resize t)
