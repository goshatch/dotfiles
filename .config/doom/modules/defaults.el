;;; defaults.el --- Sensible defaults for editing -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration,
;; email clients, file templates, and snippets.

(setq user-full-name "Gosha Tcherednitchenko"
      user-mail-address "mail@gosha.net")

;; We want to raise the ~undo-limit~ to 80Mb, use granular undo,
;; and replace ~...~ with a unicode ellipse.

(setq undo-limit 80000000
      ;; evil-want-fine-undo t
      truncate-string-ellipsis "…")

;; Some convenience from Vim:

(setq evil-escape-key-sequence "jj"
      evil-escape-delay 0.3)

;; Use j/k to move up/down in visual lines

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Vim-style movement in undo-tree

; FIXME: Does not work apparently
(after! undo-tree
  (define-key undo-tree-visualizer-mode-map (kbd "j")
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-mode-map (kbd "k")
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-mode-map (kbd "h")
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map (kbd "l")
    'undo-tree-visualize-switch-branch-right)
  )

;; An easier way to call avy-goto-char-timer:

(setq avy-all-windows t)
(map! "C-c SPC" #'avy-goto-char-2)

;;;; Auth sources configuration

(setq auth-sources '("~/.authinfo.gpg"))

(defun gt/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

;;; defaults.el ends here
