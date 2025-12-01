;;; ledger.el --- Ledger mode configuration -*- lexical-binding: t; -*-

;; Use ledger-mode for hledger files:

(after! ledger-mode
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode)))

;;; ledger.el ends here
