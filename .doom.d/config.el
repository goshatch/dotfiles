(setq user-full-name "Gueorgui Tcherednitchenko"
      user-mail-address "gt@gueorgui.net")

(setq undo-limit 80000000
      evil-want-fine-undo t
      truncate-string-ellipsis "…")

(defvar gt/base-font-size 13
  "The base font size from which all others are calculated")

(setq doom-font
      (font-spec :family "Iosevka" :size gt/base-font-size))
(setq doom-variable-pitch-font
      (font-spec :family "IBM Plex Sans" :size gt/base-font-size))
(setq doom-big-font
      (font-spec :family "Iosevka" :size (+ gt/base-font-size 5)))

(setq doom-unicode-font
      (if IS-MAC
          (font-spec :family "Apple Color Emoji")
        (font-spec :family "Twitter Color Emoji"))
      )

(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :family "Noto Sans CJK TC" :size gt/base-font-size)))

(setq-default line-spacing 0.1)

(setq modus-themes-subtle-line-numbers t
      modus-themes-mode-line 'borderless-accented-3d
      modus-themes-bold-constructs t
      modus-themes-paren-match 'subtle-bold
      modus-themes-region 'bg-only
      modus-themes-syntax 'yellow-comments)

(setq doom-theme 'modus-vivendi)

(setq frame-title-format '((:eval (projectile-project-name)) "@emacs"))

(setq +ivy-buffer-preview t)

(setq ivy-read-action-function #'ivy-hydra-read-action)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(setq projectile-project-search-path '("~/repos"))

(setq projectile-switch-project-action #'projectile-dired)

(after! projectile
  (projectile-register-project-type
   'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
   :project-file "Gemfile"
   :compile "bundle exec rails server"
   :src-dir "lib/"
   :test "bundle exec rspec"
   :test-dir "spec/"
   :test-suffix "_spec")
  )

(setq evil-escape-key-sequence "jj"
      evil-escape-delay 0.3)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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

(global-evil-matchit-mode 1)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(setq org-directory "~/org/")
