;;; theme.el --- Theme configuration -*- lexical-binding: t; -*-

(defun gt/set-fill-column-colors ()
  (set-face-attribute 'fill-column-indicator nil
                      :foreground (modus-themes-get-color-value 'bg-inactive)
                      :background (modus-themes-get-color-value 'unspecified)))

;; We will use the wonderful Modus Vivendi theme by Protesilaos Stavrou,
;; with some slight customisations:
;; https://protesilaos.com/modus-themes/

(setq modus-themes-bold-constructs t
      modus-themes-common-palette-overrides
      '((fringe unspecified)
        (comment yellow-cooler)
        (border-mode-line-inactive unspecified)
        (border-mode-line-active unspecified)
        (fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
        (bg-paren-match bg-magenta-intense)
        (underline-paren-match fg-main)
        (fg-region fg-main)
        (bg-mode-line-active bg-cyan-subtle)
        (fg-mode-line-active fg-main))
      doom-theme 'modus-operandi-tinted)

;;;; Better fill column indicator colours

(add-hook 'modus-themes-after-load-theme-hook #'gt/set-fill-column-colors)
(add-hook 'doom-load-theme-hook #'gt/set-fill-column-colors)

;;;; Automatically toggle themes based on OS dark/light theme

(after! doom-ui
  (setq! auto-dark-themes '((modus-vivendi) (modus-operandi-tinted)))
  (auto-dark-mode))

;;;; Set keyboard shortcut to toggle between light/dark Modus themes

(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
(define-key doom-leader-map (kbd "t m")
  'modus-themes-toggle)

;;; theme.el ends here
