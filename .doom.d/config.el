(setq user-full-name "Gosha Tcherednitchenko"
      user-mail-address "mail@gosha.net")

(setq undo-limit 80000000
      evil-want-fine-undo t
      truncate-string-ellipsis "…")

(defvar gt/base-font-size 13
  "The base font size from which all others are calculated")

(setq doom-font
      (font-spec :family "PragmataPro Mono" :size gt/base-font-size :weight 'normal :spacing 100))
(setq gt/ru-font
      (font-spec :family "Iosevka" :size gt/base-font-size :weight 'normal :spacing 100))
(setq doom-variable-pitch-font
      (font-spec :family "PT Serif" :weight 'regular :height 160 :width 'normal))

(setq doom-symbol-font
      (if (featurep :system 'macos)
          (font-spec :family "Apple Color Emoji")
        (font-spec :family "Twitter Color Emoji"))
      )

(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family "Noto Sans CJK TC" :size gt/base-font-size))))

(if (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'cyrillic
     gt/ru-font))

(setq-default line-spacing 0.1)

(setq modus-themes-bold-constructs t)

(setq modus-themes-common-palette-overrides
      '((fringe unspecified)
        (comment yellow-cooler)
        (border-mode-line-inactive unspecified)
        (border-mode-line-active unpsecified)
        (fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
        (bg-paren-match bg-magenta-intense)
        (underline-paren-match fg-main)
        (fg-region fg-main)
        (bg-mode-line-active bg-cyan-subtle)
        (fg-mode-line-active fg-main)))

(setq doom-theme 'modus-vivendi)

(when (and (eq system-type 'darwin) (display-graphic-p))
  (use-package! auto-dark
    :init
    (setq auto-dark-dark-theme 'modus-vivendi
          auto-dark-light-theme 'modus-operandi)
    (auto-dark-mode t))
  )

(define-key doom-leader-map (kbd "t m")
  'modus-themes-toggle)

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(setq
 frame-title-format
 '("%b — "
   (:eval
    (format "%s"  persp-last-persp-name))
   " — Emacs"
   ))

(menu-bar-mode -1)

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

(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" (vterm-send-key (kbd "C-c")))

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

(setq avy-all-windows t)
(map! "C-c SPC" #'avy-goto-char-2)

(setq auth-sources '("~/.authinfo.gpg"))

(use-package! magit
  :config
  (setq magit-log-section-commit-count 20))

(global-evil-matchit-mode 1)

(setq typescript-indent-level 2)

(setq rbenv-show-active-ruby-in-modeline nil)
(global-rbenv-mode)

(after! lsp-mode
  (setq lsp-solargraph-use-bundler nil)
  (setq lsp-sorbet-as-add-on t)
  (setq lsp-sorbet-use-bundler t))

(use-package! uxntal-mode)

(use-package! forth-mode)

(defun gt/setup-lsp-ui-peek ()
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(add-hook 'lsp-ui-mode-hook #'gt/setup-lsp-ui-peek)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; (require 'lsp-sonarlint)

;; (defun gt/setup-sonarlint-ruby ()
;;   (require 'lsp-sonarlint-ruby)
;;   (setq lsp-sonarlint-ruby-enabled t))

;; (add-hook 'ruby-mode #'gt/setup-sonarlint-ruby)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; (setq lsp-sonarlint-modes-enabled
;;       (delete-dups
;;        (append lsp-sonarlint-modes-enabled '(typescript-mode typescript-tsx-mode rjsx-mode))))

;; (defun gt/setup-sonarlint-js ()
;; (require 'lsp-sonarlint-javascript)
;; (setq lsp-sonarlint-javascript-enabled t)

;; (require 'lsp-sonarlint-typescript)
;; (setq lsp-sonarlint-typescript-enabled t)
;; )

;; (dolist (hook '(js2-mode-hook rjsx-mode-hook typescript-mode-hook typescript-tsx-mode-hook))
;;   (add-hook hook #'gt/setup-sonarlint-js))

(use-package! nvm)

(use-package! lsp-tailwindcss)

(setq-default doom-scratch-initial-major-mode 'lisp-interaction-mode)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; (use-package! casual)

(use-package! feature-mode)

(setq org-directory "~/org/")

(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . auto)))

(setq org-log-done 'time)

(setq org-agenda-files
      (list (concat org-directory "inbox.org")
            (concat org-directory "work/")
            (concat org-directory "projects/")))

(defun gt/open-agenda ()
  (interactive)
  (org-agenda nil "a"))

(use-package! org
  :bind
  ("C-c a" . gt/open-agenda))

(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (define-key org-agenda-mode-map (kbd "C-c C-l") 'org-agenda-log-mode)))

(defun gt/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

(use-package! org
  :bind
  ("C-c l" . gt/org-insert-link-dwim))

(setq deft-directory org-roam-directory)

(setq org-roam-completion-everywhere t)

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

(use-package! org-roam-ui
  :init
  (when (featurep 'xwidget-internal)
    (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))

(defvar gt/org-roam-instances
  '(("Personal" . ("~/org/roam" "~/.emacs.d/.local/cache/org-roam.db"))
    ("Work" . ("~/org/roam-work" "~/.emacs.d/.local/cache/org-roam-work.db")))
  "An Alist of org-roam instances with labels")

(defun gt/set-org-roam-instance (label)
  (interactive)
  (let* ((instance (assoc label gt/org-roam-instances))
         (directory (expand-file-name (nth 1 instance)))
         (db-location (expand-file-name (nth 2 instance))))
    (unless instance
      (error "No org-roam instance found with label: %s" label))
    (setq org-roam-directory directory)
    (setq org-roam-db-location db-location)
    (org-roam-db-sync)))

(defun gt/select-org-roam-instance ()
  (interactive)
  (let* ((labels (mapcar 'car gt/org-roam-instances))
         (label (completing-read "Choose org-roam instance: " labels nil t)))
    (gt/set-org-roam-instance label)))

(use-package! org-roam
  :bind
  ("C-c j j" . org-roam-dailies-goto-today)
  ("C-c j i" . org-roam-dailies-capture-today))

(defun gt/daily-location ()
  (let ((location
         (with-temp-buffer
           (insert-file-contents-literally "~/.current_location.txt")
           (split-string
            (string-trim-right
             (buffer-substring-no-properties (point-min) (point-max)))
            ","))))
    (format "%s (%s)" (nth 0 location) (nth 1 location))))

(defun gt/daily-weather ()
  (string-trim-right
   (shell-command-to-string "~/.bin/location_weather.sh")))

(defun gt/daily-pregnancy-week-day (time-stamp)
  (let* ((days-since (- (org-time-stamp-to-now time-stamp)))
         (weeks (/ days-since 7))
         (days (- days-since (* weeks 7))))
    (format "Week %s, Day %s" weeks days)))

(defun gt/child-age-in-weeks (birth-date)
  "Calculates how many weeks and days it has been since BIRTH-DATE, and returns
a formatted string with the number of days, or without the number of days if
the number of days is zero."
  (let* ((days-since (- (org-time-stamp-to-now birth-date)))
         (weeks (/ days-since 7))
         (days (- days-since (* weeks 7)))
         (format-string (if (eq days 0) "%s weeks" "%s weeks and %s days")))
    (format format-string weeks days)))

(defun gt/child-age-in-months (birth-date)
  "Checks whether it has been exactly some months since BIRTH-DATE and prints a
corresponding output string (e.g. '4 months'), and otherwise passes
BIRTH-DATE to `gt/child-age-in-weeks'."
  (let* ((parsed-birth-date (parse-time-string birth-date))
         (birth-year (nth 5 parsed-birth-date))
         (birth-month (nth 4 parsed-birth-date))
         (birth-day (nth 3 parsed-birth-date))
         (parsed-current-date (decode-time (current-time)))
         (current-year (nth 5 parsed-current-date))
         (current-month (nth 4 parsed-current-date))
         (current-day (nth 3 parsed-current-date))
         (months-diff (+ (* (- current-year birth-year) 12)
                         (- current-month birth-month))))
    (if (eq current-day birth-day)
        (format "%d months" months-diff)
      (gt/child-age-in-weeks birth-date))))

(defun gt/child-age (birth-date)
  "Outputs the age of a child based on BIRTH-DATE."
  (gt/child-age-in-months birth-date))

(defun gt/org-roam-on-this-day ()
  "Return a list of links to org-roam daily notes from this day in previous
   years, or NIL if none are found."
  (require 'org-roam)
  (let* ((query "SELECT id, title FROM nodes WHERE file LIKE '%%daily%%' AND file LIKE '%%' || strftime('%%m-%%d', 'now') || '%%' ORDER BY title DESC")
         (rows (org-roam-db-query query))
         (results '()))
    (if (null rows)
        nil
      (progn
        (dolist (row rows)
          (let* ((id (nth 0 row))
                 (title (nth 1 row))
                 (year (substring title 0 4)))
            (push (format "[[id:%s][%s]]" id year) results)))
        (concat "On this day: " (mapconcat 'identity results ", "))))))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M> %?"
         :if-new (file+head
                  "%<%Y-%m-%d>.org"
                  "%[~/org/roam/templates/daily-template.org]"))))

(setq gt/org-inbox-file (file-name-concat org-directory "inbox.org"))

(defun gt/refile-to-file-headline (file headline)
  "Refile tree to a specific file and a specific headline"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(defun gt/org-roam-today-daily-path ()
  "Return the path for today's org-roam daily note"
  (file-name-concat
   org-roam-directory
   org-roam-dailies-directory
   (format-time-string "%F.org")))

(defun gt/refile-inbox-to-org-roam-today-daily ()
  "Refile all org trees from inbox file to today's org-roam daily note"
  (let ((inbox-buffer (find-file-noselect gt/org-inbox-file)))
    (set-buffer inbox-buffer)
    (org-map-entries
     (lambda ()
       (setq org-map-continue-from 0)
       (gt/refile-to-file-headline
        (gt/org-roam-today-daily-path)
        "Inbox")))
    (save-buffer)))

(defun gt/org-roam-daily-hook ()
  "Hook called upon visiting an org-roam daily note"
  (let ((daily-buffer (current-buffer)))
    ;; Only perform this if we're visiting the buffer for today's daily
    (when (string= (buffer-file-name daily-buffer) (gt/org-roam-today-daily-path))
      (gt/refile-inbox-to-org-roam-today-daily)
      (set-buffer daily-buffer))))

(add-hook 'org-roam-dailies-find-file-hook 'gt/org-roam-daily-hook)

(defvar gt/weekly-review-capture-template
  `(("d" "default" entry
     "* %?"
     :if-new (file+head
              "daily/%<%Y-%m-%d>-weekly-review.org"
              "%[~/org/roam/templates/weekly-review-template.org]"))))

(defun gt/weekly-review-capture (&optional no-visit)
  "Create a weekly review note from the appropriate template"
  (interactive)
  (org-roam-capture- :goto (unless no-visit '(4))
                     :node (org-roam-node-create)
                     :templates gt/weekly-review-capture-template))

(use-package! org-roam
  :bind
  ("C-c j w" . gt/weekly-review-capture))

(defvar gt/monthly-review-capture-template
  `(("d" "default" entry
     "* %?"
     :if-new (file+head
              "daily/%<%Y-%m>-monthly-review.org"
              "%[~/org/roam/templates/monthly-review-template.org]"))))

(defun gt/monthly-review-capture (&optional no-visit)
  "Create a monthly review note from the appropriate template"
  (interactive)
  (org-roam-capture- :goto (unless no-visit '(4))
                     :node (org-roam-node-create)
                     :templates gt/monthly-review-capture-template))

(use-package! org-roam
  :bind
  ("C-c j m" . gt/monthly-review-capture))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! org-roam
  :bind
  ("C-c n n" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n u" . org-roam-ui-open)
  ("C-c n h" . gt/select-org-roam-instance))

(use-package! org
  :config
  (setq org-hide-emphasis-markers t
        org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :background "Transparent")
  (add-to-list 'org-todo-keyword-faces '("REVW" . +org-todo-onhold))
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)
)))

;; (use-package! mixed-pitch
;;   :hook
;;   (org-mode . mixed-pitch-mode)
;;   :config
;;   (setq! mixed-pitch-set-height gt/base-font-size)
;;   (setq org-hide-emphasis-markers t)
;;   (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-drawer))

(use-package! wc-mode
  :config
  (global-set-key "\C-cw" 'wc-mode))

;; NOTE: These are not the same
(setq doom-modeline-enable-word-count t)

(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)

(defun gt/visual-line-range ()
  (save-excursion
    (cons
     (progn (beginning-of-visual-line) (point))
     (progn (end-of-visual-line) (point)))))

;; (use-package! emacs
;;   :config
;;   (setq-default scroll-preserve-screen-position t)
;;   (setq-default scroll-conservatively 1) ; affects `scroll-step'
;;   (setq-default scroll-margin 0)

;;   (define-minor-mode gt/scroll-centre-cursor-mode
;;     "Toggle centred cursor scrolling behaviour."
;;     :init-value nil
;;     :lighter " S="
;;     :global nil
;;     (if gt/scroll-centre-cursor-mode
;;         (setq-local scroll-margin (* (frame-height) 2)
;;                     scroll-conservatively 0
;;                     maximum-scroll-margin 0.5)
;;       (dolist (local '(scroll-preserve-screen-position
;;                        scroll-conservatively
;;                        maximum-scroll-margin
;;                        scroll-margin))
;;         (kill-local-variable `,local))))
;;   :bind ("C-c L" . gt/scroll-centre-cursor-mode))

;; (use-package! olivetti
;;   :ensure
;;   :diminish
;;   :config
;;   (setq olivetti-body-width 80)
;;   (setq olivetti-minimum-body-width 80)
;;   (setq olivetti-recall-visual-line-mode-entry-state t)

;;   (define-minor-mode gt/olivetti-mode
;;     "Toggle buffer-local `olivetti-mode' with additional parameters."
;;     :init-value nil
;;     :global nil
;;     (if gt/olivetti-mode
;;         (progn
;;           (olivetti-mode 1)
;;           (org-indent-mode -1)
;;           (setq line-spacing 0.4)
;;           (buffer-face-mode)
;;           (hide-mode-line-mode)
;;           (vi-tilde-fringe-mode -1)
;;           (set-window-fringes (selected-window) 0 0)
;;           (text-scale-increase 1)
;;           (display-line-numbers-mode -1)
;;           (gt/scroll-centre-cursor-mode)
;;           (setq hl-line-range-function 'gt/visual-line-range))
;;       (olivetti-mode -1)
;;       (org-indent-mode 1)
;;       (setq line-spacing 0.1)
;;       (hide-mode-line-mode -1)
;;       (vi-tilde-fringe-mode 1)
;;       (set-window-fringes (selected-window) nil) ; Use default width
;;       (text-scale-decrease 1)
;;       (gt/scroll-centre-cursor-mode -1)
;;       (display-line-numbers-mode)
;;       (setq hl-line-range-function nil)))

;;   :bind ("C-c o" . gt/olivetti-mode))

(use-package! quail-russian-qwerty)

;; (use-package lsp-ltex
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-ltex)
;;                        (lsp)))  ; or lsp-deferred
;;   :init
;;   (setq lsp-ltex-version "16.0.0"))  ; make sure you have set this, see below

(defun gt/insert-anki-card ()
  "Insert a new Anki note at the bottom of the current subtree."
  (interactive)
  (let* ((question (read-string "Question: "))
         (current-level (org-current-level))
         (subheading-level (+ 2 current-level))
         (deck "")
         (tags "")
         (last-card-properties (save-excursion
                                 (save-restriction
                                   (org-narrow-to-subtree)
                                   (goto-char (point-max))
                                   (if (re-search-backward ":ANKI_DECK:" nil t)
                                       (let ((deck (org-entry-get (point) "ANKI_DECK"))
                                             (tags (org-entry-get (point) "ANKI_TAGS")))
                                         (list deck tags))
                                     (list nil nil)))))
         (deck (or (nth 0 last-card-properties) ""))
         (tags (or (nth 1 last-card-properties) "")))
    (org-insert-subheading nil)
    (insert (format "%s\n:PROPERTIES:\n:ANKI_DECK: %s\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: %s\n:END:\n"
                    question deck tags))
    (insert (format "%s Front\n%s\n"
                    (make-string subheading-level ?*) question))
    (insert (format "%s Back\n"
                    (make-string subheading-level ?*))))
  (outline-up-heading 1))

(defun gt/get-anki-tags ()
  "Collect all unique :ANKI_TAGS: in the current org buffer."
  (let ((tags '()))
    (save-restriction
      (widen)
      (org-element-map (org-element-parse-buffer) 'node-property
        (lambda (property)
          (when (string= (org-element-property :key property) "ANKI_TAGS")
            (setq tags (append tags (split-string (org-element-property :value property) " "))))))
      (delete-dups tags))))

(defun gt/anki-tags-autocomplete ()
  "Autocomplete for :ANKI_TAGS: property."
  (interactive)
  (let* ((tags (gt/get-anki-tags))
         (selected-tags (completing-read-multiple "Select tags: " tags nil t)))
    (org-set-property "ANKI_TAGS" (mapconcat 'identity selected-tags " "))))

(use-package! anki-editor
  :config
  (define-key org-mode-map (kbd "C-c n a a") #'gt/insert-anki-card)
  (define-key org-mode-map (kbd "C-c n a t") #'gt/anki-tags-autocomplete)
  (define-key org-mode-map (kbd "C-c n a p") #'anki-editor-push-notes)
  (which-key-add-key-based-replacements
    "C-c n a a" "Insert Anki card"
    "C-c n a t" "Select tags for card"
    "C-c n a p" "Push cards to Anki"))

(setq org-pomodoro-keep-killed-pomodoro-time t)

(setq org-pomodoro-play-sounds nil)

(setq alert-notifier-command (executable-find "terminal-notifier"))

(use-package! company
  :config
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet))))

(setq org-image-actual-width 500
      org-startup-with-inline-images t)

(nconc +org-capture-frame-parameters '((top . 0.5) (left . 0.5)))

(use-package! calibredb
  :init
  (map! :map doom-leader-search-map :desc "Search Calibre database" "c" #'calibredb)
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-format-icons-in-terminal t)
  (setq calibredb-download-dir "~/Downloads")
  (map! :map calibredb-search-mode-map
        :n "q"   'calibredb-search-quit
        :n "n"   'calibredb-virtual-library-next
        :n "N"   'calibredb-library-next
        :n "p"   'calibredb-virtual-library-previous
        :n "P"   'calibredb-library-previous
        :n "l"   'calibredb-virtual-library-list
        :n "o"   'calibredb-find-file
        :n "O"   'calibredb-find-file-other-frame
        :n "V"   'calibredb-open-file-with-default-tool
        :n "v"   'calibredb-view
        :n "d"   'calibredb-remove
        :n "D"   'calibredb-remove-marked-items
        :n "m"   'calibredb-mark-and-forward
        :n "s"   'calibredb-set-metadata-dispatch
        :n "e"   'calibredb-export-dispatch
        ;; :n "b"   'calibredb-catalog-bib-dispatch
        :n "a"   'calibredb-add
        :n "."   'calibredb-open-dired
        :n ","   'calibredb-quick-look
        :n "y"   'calibredb-yank-dispatch
        :n "u"   'calibredb-unmark-and-forward
        :n "DEL" 'calibredb-unmark-and-backward
        :n "s"   'calibredb-set-metadata-dispatch
        :n "?"   'calibredb-dispatch
        :n "/"   'calibredb-search-live-filter
        :n "j" 'calibredb-next-entry
        :n "k" 'calibredb-previous-entry
        :n "M-f"   'calibredb-toggle-favorite-at-point
        :n "M-x"   'calibredb-toggle-archive-at-point
        :n "M-h"   'calibredb-toggle-highlight-at-point
        :n "M-n"   'calibredb-show-next-entry
        :n "M-p"   'calibredb-show-previous-entry
        :n "R"   'calibredb-search-clear-filter
        :n "r"   'calibredb-search-refresh-and-clear-filter
        :n "<backtab>"   'calibredb-toggle-view
        :n "<tab>"   'calibredb-toggle-view-at-point
        :n "TAB"   'calibredb-toggle-view-at-point
        :n "RET" 'calibredb-find-file)
  (map! :map calibredb-show-mode-map
        :nie "q" 'calibredb-entry-quit
        :nie "?" 'calibredb-entry-dispatch
        :nie "RET" 'calibredb-search-ret))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq nov-text-width 80)

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "IBM Plex Serif"
                                           :height 1.2))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(use-package! nov-xwidget
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
  (setq! nov-xwidget-style-dark "
    body {
        writing-mode: horizontal-tb;
        // background: #000000 !important;
        color: #eee !important;
        font-size: 18px !important;
        text-align: left !important;
        width: 90% !important;
        height: 50% !important;
        position: absolute !important;
        left: 49% !important;
        top: 30% !important;
        transform: translate(-50%, -55%) !important;
        line-height: 1.5rem !important;
    }
    p {
        text-align: left !important;
        margin-bottom: 25px !important;
    }
    h1, h2, h3, h4, h5, h6 {
        /*color: #eee !important;*/
        border-bottom: 0px solid #eee !important;
        line-height: 1em;
    }
    pre, tr, td, div.warning {
        font-size: 1em;
        background: #272c35;
    }
    th {
        font-size: 1em;
        color: #eee !important;
    }

    span {
        font-size: 18px;
        color: #eee !important;
    }
    h1 {
        color: #ffaf69 !important;
    }
    h2 {
        color: #3fc6b7 !important;
    }
    h3 {
        color: #88d498 !important;
    }
    h4 {
        color: #80c3f0 !important;
    }
    h5 {
        color: #cccccc !important;
    }
    h6 {
        color: #cccccc !important;
    }

    /* Same font for all tags */
    a, em, caption, th, tr, td, h1, h2, h3, h4, h5, h6, p, body {
        font-family: \"IBM Plex Serif\", Georgia,Cambria,\"Times New Roman\",Times,serif !important;
    }
    code, pre {
        font-family: \"PragmataPro Mono\", Iosevka !important;
        font-size: 0.9rem !important;
    }
    :root {
        color-scheme: dark; /* both supported */
    }

    body, p.title  {
        color: #eee !important;
    }

    body a{
        color: #809fff !important;
    }

    body img {
        max-width: 100% !important;
        filter: brightness(.8) contrast(1.2);
    }
    .programlisting {
        font-size: 20px;
    }"))

(setq gt/hostname (car (split-string (system-name) "\\.")))

(set-irc-server! "sourcehut/libera"
  `(:port 6697
    :host "chat.sr.ht"
    :use-tls t
    :nick "gosha_"
    :realname "Gosha Tcherednitchenko"
    :channels ("#emacs" "#uxn" "#lisp")
    :sasl-username "gosha/liberachat@strogino"
    :sasl-password (lambda (&rest _) (+pass-get-secret "irc/bouncer"))))

(setq circe-notifications-alert-style 'message)
