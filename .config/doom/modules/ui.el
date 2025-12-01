;;; ui.el --- UI tweaks and configuration -*- lexical-binding: t; -*-

;;;; TODO Remove this once Emacs 30.1 ships
;; This needed to be redefined in order to build the pdf-tools

;; (defvar x-gtk-use-system-tooltips use-system-tooltips)

;;;; Frame title
;; Set the frame title to include the name of the current ~persp-mode~ workspace:

(setq
 frame-title-format
 '("%b — "
   (:eval
    (format "%s"  persp-last-persp-name))
   " — Emacs"
   ))

;;;; Disable the menu bar

(menu-bar-mode -1)

;;;; Vim-style tabs: tab-bar-mode
;; I miss the way Vim tabs work, and it seems like ~tab-bar-mode~ is a good
;; solution to implement something like this.
;;
;; Links: documentation (https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html),
;; BSAG blog post (https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/).
;;
;; Also integrate ~tab-bar-mode~ and ~persp-mode~, as stolen from here:
;; https://github.com/LemonBreezes/.doom.d/blob/master/lisp/persp-mode-tab-bar-integration.el
;; (originally found here: https://github.com/Bad-ptr/persp-mode.el/issues/122#issuecomment-1224884651).

(use-package! tab-bar
  :after emacs
  :config
  (setq tab-bar-close-button-show   nil
        tab-bar-new-button-show     nil
        tab-bar-auto-width          nil
        tab-bar-back-button         nil
        tab-bar-forward-button      nil
        tab-bar-show                1)
  (define-key evil-normal-state-map (kbd "g t") #'tab-bar-switch-to-next-tab)
  (define-key evil-normal-state-map (kbd "g T") #'tab-bar-switch-to-prev-tab)
  (define-key global-map (kbd "s-t") #'tab-bar-new-tab)

  (add-hook 'persp-before-deactivate-functions
            (defun +workspaces-save-tab-bar-data-h (_)
              "Save the tab-bar-tabs  "
              (when (get-current-persp)
                (set-persp-parameter
                 'tab-bar-tabs (tab-bar-tabs))
                (set-persp-parameter 'tab-bar-closed-tabs tab-bar-closed-tabs))))

  (add-hook 'persp-activated-functions
            (defun +workspaces-load-tab-bar-data-h (_)
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (setq tab-bar-closed-tabs (persp-parameter 'tab-bar-closed-tabs))
              (tab-bar--update-tab-bar-lines t)))
  (tab-bar-mode 1))

;;;; Split windows to the right and down by default

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;;; Vterm
;; Send C-c to the terminal

(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" (vterm-send-key (kbd "C-c")))

;;;; Indent bars
;; See examples in indent-bars repo:
;; https://github.com/jdtsmith/indent-bars/blob/main/examples.md

(use-package! indent-bars
  :config
  (setq
    indent-bars-color '(highlight :face-bg t :blend 0.15)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
    indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
    indent-bars-display-on-blank-lines t)
  :hook ((prog-mode) . indent-bars-mode))

;;;; File path in modeline
;; Show buffer names relative to project

(setq! doom-modeline-buffer-file-name-style 'relative-to-project)

;;;;; Pomodoro notifications
;; Set path to terminal-notifier executable

(setq alert-notifier-command (executable-find "terminal-notifier"))

;;;;; Corfu
;; Candidate selection tweaks

(use-package! corfu
  :config
  (setq corfu-preselect 'first
        corfu-preview-current 'insert))

;;; ui.el ends here
