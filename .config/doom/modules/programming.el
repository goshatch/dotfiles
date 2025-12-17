;;; programming.el --- General programming configuration -*- lexical-binding: t; -*-

;; Easily jump between the beginning and end of blocks

(global-evil-matchit-mode 1)

;; Show the fill column indicator and turn on rainbow delimiters for
;; programming modes

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Use Mise to manage ruby/node/etc versions
;; https://mise.jdx.dev/

(use-package! mise
 :config
 (add-hook 'doom-after-init-hook #'global-mise-mode))

;;;; IDE

;;;;; Navigation
;; Use lsp-ui-peek for definitions and references.

(defun gt/setup-lsp-ui-peek ()
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(add-hook 'lsp-ui-mode-hook #'gt/setup-lsp-ui-peek)

;;;;; Biome support

(use-package! lsp-biome
  :after eglot)

;;;;; Warnings display
;; Use end of line diagnostics instead of Doom's default popon mode

(use-package! flymake
  :config
  (setq flymake-show-diagnostics-at-end-of-line 'short))

;;;; Emacs metaprogramming
;; Set the scratch buffer to open in ~lisp-interaction-mode~ by default.

(setq-default doom-scratch-initial-major-mode 'lisp-interaction-mode)

;;;; Conveniences

;; Make script files executable when saving

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Support for ASCII Doc file format

(use-package! adoc-mode)

;;;; Gleam

(use-package! gleam-ts-mode
  :mode (rx ".gleam" eos))

(after! treesit
  (add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode)))

(after! gleam-ts-mode
  (unless (treesit-language-available-p 'gleam)
    (gleam-ts-install-grammar)))

(after! eglot
  (add-to-list 'eglot-server-programs
               '(gleam-ts-mode . ("gleam" "lsp"))))

(add-hook 'gleam-ts-mode-hook #'eglot-ensure)

;;; programming.el ends here
