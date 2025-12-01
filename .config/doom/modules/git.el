;;; git.el --- Git and Magit configuration -*- lexical-binding: t; -*-

;;;; Magit
;; Show more recent commits

(use-package! magit
  :config
  (setq magit-log-section-commit-count 20))

;; Correctly handle escape sequences in output of e.g. pre-commit hooks

(defun color-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(advice-add 'magit-process-filter :after #'color-buffer)

;; Project TODOs in Magit

(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))

;;; git.el ends here
