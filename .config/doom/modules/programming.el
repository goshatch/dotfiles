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

;;;; Clojure

;; Ignore the semantic token provider for clojure-lsp, as it overrides the font
;; locking provided by the Clojure modes
(add-hook! '(clojure-mode-hook clojure-ts-mode-hook)
  (defun +disable-eglot-semantic-tokens-h ()
    (require 'eglot)
    (setq-local eglot-ignored-server-capabilities
                (cons :semanticTokensProvider
                      (default-value 'eglot-ignored-server-capabilities)))))

;;; programming.el ends here
