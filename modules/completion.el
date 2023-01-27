;;; completion.el --- set up for completion framework.

;;; Commentary:
;;; Sets up vertico as the completion framework, with a few more additions.

;;; Code:

;; installs vertico, completion framework.
(use-package vertico
  :init
  (vertico-mode))

;; installs orderless, optional completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'completion)

;;; completion.el ends here
