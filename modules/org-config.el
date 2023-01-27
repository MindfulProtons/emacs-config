;;; org-config.el  --- Org additions/configuration

;;; Commentary:

;;; Provides Org mode configuration.

;;; Code:

(use-package org-bullets
  :hook 
  (org-mode . org-bullets-mode))

(provide 'org-config)

;;; org-config.el ends here
