(require 'package)
(setq package-enable-at-startup nil) ;; Don't load packages before starting up
(setq package-archives '(("gnu"       . "https://elpa.gnu.org/packages/")
			 ("org"       . "https://orgmode.org/elpa/")
			 ("melpa"     . "https://melpa.org/packages/")
			 ))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; custom-set-variables go here
(setq custom-file (expand-file-name "system-custom.el" user-emacs-directory))

;; Sensible defaults
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character

;; Remove unnecessary chrome
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Display line numbers
(global-display-line-numbers-mode t)

;; Undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; Evil-mode everywhere
(use-package evil
  :ensure t ;; install evil if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil) ; needed by evil-collection
  :config ;; tweak evil after loading it
  (evil-mode)
  (evil-escape-mode)
  (evil-set-undo-system 'undo-tree)
  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(use-package evil-escape
  :ensure t
  :init
  (setq
   evil-escape-excluded-states '(normal visual multiedit emacs motion)
   evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
   evil-escape-key-sequence "jj"
   evil-escape-delay 0.2)
  )

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

;; Ivy, swiper, counsel
(defun gt/find-file-right ()
  (interactive)
  (ivy-exit-with-action
   (lambda (cand)
     (split-window-right)
     (other-window 1)
     (find-file cand))))

(defun gt/find-file-below ()
  (interactive)
  (ivy-exit-with-action
   (lambda (cand)
     (split-window-below)
     (other-window 1)
     (find-file cand))))

(use-package ivy :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :config
  (setq ivy-wrap t)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-v") 'gt/find-file-right)
  (define-key ivy-minibuffer-map (kbd "C-x") 'gt/find-file-below)
  )

;; Avy, jump to things in Emacs tree-style
(use-package avy :ensure t
  :commands (avy-goto-word-1))

;; General
;; https://github.com/noctuid/general.el
(use-package general :ensure t)
(use-package which-key :ensure t :config (which-key-mode 1))

;; Key bindings
(general-define-key
 "C-'" 'avy-goto-word-1
 "C-s" 'swiper ;; Search for string in current buffer
 "M-x" 'counsel-M-x ;; Replace the default M-x with ivy backend
 )

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "SPC" '(counsel-git :which-key "find file in project")
 "p" '(counsel-rg :which-key "search in project")

 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "find file")
 "fr" '(counsel-recentf :which-key "recent files")

 "b" '(:ignore t :which-key "buffers")
 "bb" '(ivy-switch-buffer :which-key "switch buffer")

 "j" '(:ignore t :which-key "jump to...")
 "jj" '(avy-goto-char-timer :which-key "chars")
 "jw" '(avy-goto-word-1 :which-key "word")
 "jl" '(avy-goto-line :which-key "line")
 
 "g" '(:ignore t :which-key "git")
 "gg" '(magit :which-key "Magit")
 "gf" '(counsel-git-grep :which-key "git grep")

 "e" '(:ignore t :which-key "eval")
 "ee" '(eval-expression :which-key "expression")
 "er" '(eval-region :which-key "region")
 "eb" '(eval-buffer :which-key "buffer")
 )

;; Git integration
(use-package magit :ensure t)
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;; Editorconfig support
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Theme: Gruvbox
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; Font face
(set-face-attribute 'default nil :family "Iosevka" :height 130 :weight 'book)
(setq-default line-spacing 0.15)

;; -------------------------------------------------------------------------

;; TODO later, after refactoring
;; Load config from separate files
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; (load "defaults.el")
;; (load "ui.el")
