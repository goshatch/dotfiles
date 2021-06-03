;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; ~ Identity ~
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gueorgui Tcherednitchenko"
      user-mail-address "gt@gueorgui.net")

;;
;; ~ Sensible defaults ~
;;
(setq undo-limit 80000000               ; Raise undo-limit to 80Mb
      evil-want-fine-undo t             ; Use granular undo
      truncate-string-ellipsis "…")    ; Replace ... with unicode ellipse

;;
;; ~ Typography ~
;;

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq gt/base-font-size 13)

(setq doom-font
      (font-spec :family "Iosevka" :size gt/base-font-size))
(setq doom-variable-pitch-font
      (font-spec :family "IBM Plex Sans" :size gt/base-font-size))
(setq doom-big-font
      (font-spec :family "Iosevka" :size (+ gt/base-font-size 5)))

;; Use the Apple emoji font on Mac, and the Twitter Color Emoji font on Linux.
(setq doom-unicode-font
      (if IS-MAC
          (font-spec :family "Apple Color Emoji")
        (font-spec :family "Twitter Color Emoji"))
      )

;; Specify a font for CJK text, for better performance.
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :family "Noto Sans CJK TC" :size gt/base-font-size)))

;; TODO Have a different config for Mac and Linux:
;; - On Mac use the beautiful system fonts
;; - On Linux, use Noto Sans CJK

;; A little more line height
(setq-default line-spacing 0.1)

;;
;; ~ UI Tweaks ~
;;

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq modus-themes-subtle-line-numbers t
      modus-themes-mode-line 'borderless-accented-3d
      modus-themes-bold-constructs t
      modus-themes-paren-match 'subtle-bold
      modus-themes-region 'bg-only
      modus-themes-syntax 'yellow-comments)
(setq doom-theme 'modus-vivendi)

;; Set frame title to current projectile project
(setq frame-title-format '((:eval (projectile-project-name)) "@emacs"))

;; TODO tab-bar-mode
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html
;; https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - Enable by default
;; - Set vim tab mappings to target this instead of workspace tabs
;; gt -> tab-bar-switch-to-next-tab
;; gT -> tab-bar-switch-to-prev-tab
;; (tab-bar-mode 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Show a fill column indicator
(global-display-fill-column-indicator-mode t)

;; Ivy
;; Preview buffers
(setq +ivy-buffer-preview t)

;; Use hydra for navigation
(setq ivy-read-action-function #'ivy-hydra-read-action)

;; Split windows to the right and down by default
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Projectile
;; Automatically find projects in ~/repos
(setq projectile-project-search-path '("~/repos"))
;; Default action on opening project is dired
(setq projectile-switch-project-action #'projectile-dired)
;; Recognize Rails/Rspec projects
(projectile-register-project-type
 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
 :project-file "Gemfile"
 :compile "bundle exec rails server"
 :src-dir "lib/"
 :test "bundle exec rspec"
 :test-dir "spec/"
 :test-suffix "_spec")

;;
;; ~ Custom key bindings ~
;;

;; Convenience from Vim
(setq evil-escape-key-sequence "jj"
      evil-escape-delay 0.3)

;; Use j/k to move up/down in visual lines
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; FIXME Vim-style movement in undo-tree
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

;;
;; ~ Programming ~
;;

;; Easily jump between beginning/end of blocks
(global-evil-matchit-mode 1)

;; Use rjsx-mode over js-2 mode for all JS files
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))


;;
;; ~ Org mode ~
;;

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
