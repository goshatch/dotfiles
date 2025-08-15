(setq user-full-name "Gosha Tcherednitchenko"
      user-mail-address "mail@gosha.net")

(setq undo-limit 80000000
      ;; evil-want-fine-undo t
      truncate-string-ellipsis "…")

(defvar gt/base-font-size 13
  "The base font size from which all others are calculated")

(setq doom-font
      (font-spec :family "PragmataPro Mono"
                 :size gt/base-font-size
                 :weight 'normal
                 :spacing 100))
(setq gt/ru-font
      (font-spec :family "Iosevka"
                 :size gt/base-font-size
                 :weight 'normal
                 :spacing 100))
(setq doom-variable-pitch-font
      (font-spec :family "PT Serif" :weight 'regular :height 160 :width 'normal))

(setq doom-symbol-font
      (if (featurep :system 'macos)
          (font-spec :family "Apple Color Emoji")
        (font-spec :family "Twitter Color Emoji"))
      )

(if (display-graphic-p)
    (progn
      (when (member "Noto Sans CJK JP" (font-family-list))
        (dolist (charset '(kana han))
          (set-fontset-font t charset (font-spec :family "Noto Sans CJK JP" :size gt/base-font-size) nil 'prepend)))
      (when (member "Noto Sans CJK TC" (font-family-list))
        (dolist (charset '(han cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "Noto Sans CJK TC" :size gt/base-font-size) nil 'append)))))

(if (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'cyrillic
     gt/ru-font))

(setq-default line-spacing 0.0)

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

(after! doom-ui
  (setq! auto-dark-themes '((modus-vivendi) (modus-operandi-tinted)))
  (auto-dark-mode))

(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
(define-key doom-leader-map (kbd "t m")
  'modus-themes-toggle)

;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(defvar x-gtk-use-system-tooltips use-system-tooltips)

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

(setq! doom-modeline-buffer-file-name-style 'relative-to-project)

(setq evil-escape-key-sequence "jj"
      evil-escape-delay 0.3)

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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

(setq avy-all-windows t)
(map! "C-c SPC" #'avy-goto-char-2)

(setq auth-sources '("~/.authinfo.gpg"))

(defun gt/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(use-package! magit
  :config
  (setq magit-log-section-commit-count 20))

(defun color-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(advice-add 'magit-process-filter :after #'color-buffer)

(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))

(global-evil-matchit-mode 1)

; FIXME: We really should not have to do this manually!
(setq typescript-indent-level 2)

(use-package! mise
 :config
 (add-hook 'doom-after-init-hook #'global-mise-mode))

(after! gptel
  (setq
   gptel-display-buffer-action t
   gptel-default-mode 'org-mode
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (gt/lookup-password :host "api.anthropic.com")))
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (gt/lookup-password :host "api.deepseek.com")
    :models '(deepseek-reasoner deepseek-chat deepseek-coder))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(deepseek-r1:latest))
  (add-to-list 'gptel-directives
               '(clojure-dev . "you're a senior clojure/clojurescript dev with strong fp discipline. respond in PURE code blocks except: (1) when identifying errors (add terse explanations), (2) when clarification is needed (ask briefly), or (3) when suggesting changes (provide git-style diffs). prioritize idiomatic clojure: immutable data, pure functions, thread-last macros where appropriate. flag any non-obvious performance implications or side effects. favor core functions over 3rd-party libs when reasonable."))
  (add-to-list 'gptel-directives
               '(haskell-dev . "You are an expert Haskell programming assistant with deep knowledge of functional programming paradigms, type theory, monads, and Haskell's standard libraries.

Your capabilities:
1. Generate syntactically correct and idiomatic Haskell code based on natural language descriptions
2. Debug existing Haskell code by identifying compiler errors, runtime issues, and logical flaws
3. Refactor code to improve performance, readability, and maintainability
4. Explain complex Haskell concepts with clear examples
5. Recommend appropriate libraries and language extensions for specific tasks

When analyzing or generating Haskell code, you should:
- Prioritize pure functional approaches with immutable data
- Leverage the type system to catch errors at compile time
- Use appropriate abstractions (functors, applicatives, monads) without overcomplicating
- Consider performance implications, especially regarding laziness and space leaks
- Follow Haskell community style guidelines

When I share code that has errors or issues, you should:
1. Identify specific problems, referencing GHC error messages if provided
2. Explain the underlying issues in clear, educational terms
3. Provide corrected versions with explanations of your changes
4. Suggest alternative approaches when appropriate

For complex tasks, break down your solution process into:
1. Understanding the problem requirements
2. Designing appropriate data structures and type signatures
3. Implementing core functionality with clear, documented code
4. Testing considerations, including edge cases and property-based tests

Always provide explanations alongside your code to help me learn and understand the functional programming concepts involved.")))
  ;; :bind
  ;; ("C-c g g" . gptel)
  ;; ("C-c g a" . gptel-add)
  ;; ("C-c g f" . gptel-add-file)
  ;; ("C-c g m" . gptel-menu)
  ;; ("C-c g s" . gptel-send)
  ;; ("C-c g o t" . gptel-org-set-topic)
  ;; ("C-c g o p" . gptel-org-set-properties))

(after! lsp-mode
  (require 'lsp-sorbet)
  (add-to-list 'lsp-disabled-clients 'sorbet-ls)

  (defun gt/project-has-sorbet-p ()
    "Does this project use Sorbet?"
    (or (locate-dominating-file default-directory "sorbet")
        (when-let* ((root (locate-dominating-file default-directory "Gemfile.lock"))
                    (gemfile-lock (expand-file-name "Gemfile.lock" root)))
          (with-temp-buffer
            (insert-file-contents gemfile-lock)
            (search-forward-regexp "^ *sorbet \\|^ *sorbet-static " nil t)))))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (when (gt/project-has-sorbet-p)
                         (if (file-exists-p "Gemfile")
                             '("bundle" "exec" "srb" "tc" "--lsp")
                           '("srb" "tc" "--lsp")))))
    :activation-fn (lambda (filename _mode)
                     (and (eq major-mode 'ruby-mode) (gt/project-has-sorbet-p)))
    :priority -1
    :add-on? t
    :server-id 'gt/sorbet-ls))

  (setq lsp-rubocop-use-bundler t
        lsp-sorbet-use-bundler t
        lsp-sorbet-as-add-on t)
                                        ; Use HTML lsp server for .html.erb files
  (add-to-list 'lsp-language-id-configuration '("\\.html\\.erb$" . "html")))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local lsp-enabled-clients '(ruby-lsp-ls gt/sorbet-ls))
            (lsp)))

(defun gt/setup-lsp-ui-peek ()
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(add-hook 'lsp-ui-mode-hook #'gt/setup-lsp-ui-peek)

(use-package! apheleia
  :ensure apheleia
  :config
  (setf (alist-get 'standard-clojure apheleia-formatters) '("standard-clj" "fix" "-"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojure-ts-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurec-mode apheleia-mode-alist) 'standard-clojure)
  (setf (alist-get 'clojurescript-mode apheleia-mode-alist) 'standard-clojure)
  (apheleia-global-mode +1))

(when (string= (system-name) "banqiao.local")
  (append '(clojure-mode
            clojurec-mode
            clojure-ts-mode
            clojurescript-mode)
          +format-on-save-disabled-modes))

(use-package! lsp-biome
  :after lsp-mode)

;; (after! lsp-mode
;;   ;; Function to check if ESLint config exists in project
;;   (defun gt/eslint-config-exists-p ()
;;     "Check if ESLint config exists in the current project."
;;     (or (locate-dominating-file default-directory ".eslintrc")
;;         (locate-dominating-file default-directory ".eslintrc.js")
;;         (locate-dominating-file default-directory ".eslintrc.json")
;;         (locate-dominating-file default-directory ".eslintrc.yml")
;;         (locate-dominating-file default-directory ".eslintrc.yaml")
;;         (locate-dominating-file default-directory "eslint.config.js")
;;         (locate-dominating-file default-directory "eslint.config.mjs")
;;         (locate-dominating-file default-directory "eslint.config.cjs")))

;;   ;; Hook to conditionally enable/disable ESLint
;;   (add-hook 'lsp-after-initialize-hook
;;             (lambda ()
;;               (when (or (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'typescript-ts-mode 'tsx-ts-mode)
;;                         (and (derived-mode-p 'web-mode)
;;                              (member (file-name-extension buffer-file-name) '("js" "jsx" "ts" "tsx"))))
;;                 (setq-local lsp-eslint-enable (gt/eslint-config-exists-p))))))

(setq-default doom-scratch-initial-major-mode 'lisp-interaction-mode)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package! adoc-mode)

(setq org-directory "~/org/")

(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . auto)))

(setq org-log-done 'time)

(defun gt/open-agenda ()
  (interactive)
  (org-agenda nil "a"))

(use-package! org
  :config
  (setq org-agenda-files
        (list (concat org-directory "work/")
              (concat org-directory "projects/"))
        org-agenda-start-with-log-mode t
        org-agenda-start-with-clockreport-mode t)
  :bind
  ("C-c a" . gt/open-agenda))

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

(require 'cl-lib)
(require 'calendar)  ;; for calendar-last-day-of-month

(defun gt/join-with-oxford (parts)
  (pcase (length parts)
    (0 "")
    (1 (car parts))
    (2 (format "%s and %s" (nth 0 parts) (nth 1 parts)))
    (_ (format "%s, and %s"
               (string-join (butlast parts) ", ")
               (car (last parts))))))

(defun gt/child-age (birth-date)
  "age since BIRTH-DATE:
- <1yr → if <7d → \"D days\", else \"W weeks and D days\"
- ≥1yr → \"Y years, M months, and D days\""
  (let* ((b      (parse-time-string birth-date))
         (by     (nth 5 b)) (bm (nth 4 b)) (bd (nth 3 b))
         (c      (decode-time (current-time)))
         (cy     (nth 5 c)) (cm (nth 4 c)) (cd (nth 3 c))
         (raw    (round (org-time-stamp-to-now birth-date)))
         (days   (abs raw)))
    (cl-destructuring-bind (y m d)
        (let* ((y (- cy by))
               (m (- cm bm))
               (d (- cd bd)))
          (when (< d 0)
            (let* ((pm   (if (= cm 1) 12 (1- cm)))
                   (py   (if (= cm 1) (1- cy) cy))
                   (mdays (car (calendar-last-day-of-month (list py pm)))))
              (setq d (+ d mdays)
                    m (1- m))))
          (when (< m 0)
            (setq m (+ m 12)
                  y (1- y)))
          (list y m d))
      (if (< y 1)
          (if (< days 7)
              (format "%d day%s" days (if (= days 1) "" "s"))
            (let* ((w     (floor days 7))
                   (d2    (mod days 7))
                   (parts (cl-remove-if-not
                           #'identity
                           (list
                            (and (> w 0)
                                 (format "%d week%s" w
                                         (if (= w 1) "" "s")))
                            (and (> d2 0)
                                 (format "%d day%s" d2
                                         (if (= d2 1) "" "s")))))))
              (gt/join-with-oxford parts)))
        (let ((parts (cl-remove-if-not
                      #'identity
                      (list
                       (and (> y 0) (format "%d year%s"   y (if (= y 1) "" "s")))
                       (and (> m 0) (format "%d month%s"  m (if (= m 1) "" "s")))
                       (and (> d 0) (format "%d day%s"    d (if (= d 1) "" "s")))))))
          (gt/join-with-oxford parts))))))

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

(defun gt/dailies-location-stats (directory)
  "Parse all org files in DIRECTORY and count occurrences of #+location: headers.
Returns an alist of (location . count) sorted by count in descending order."
  (interactive "DDirectory: ")
  (let ((org-files (directory-files-recursively directory "\\.org$"))
        (locations '()))

    ;; Process each org file
    (dolist (file org-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^#\\+location:\\s-*\\(.*\\)$" nil t)
          (let* ((location (string-trim (match-string 1)))
                 (existing (assoc location locations)))
            (if existing
                ;; Increment count if location already exists
                (setcdr existing (1+ (cdr existing)))
              ;; Otherwise add new location with count 1
              (push (cons location 1) locations))))))

    ;; Sort by count (descending)
    (setq locations (sort locations (lambda (a b) (> (cdr a) (cdr b)))))

    ;; Display results in a buffer
    (with-current-buffer (get-buffer-create "*Org Locations*")
      (erase-buffer)
      (insert "| Location | Count |\n")
      (insert "|----------|-------|\n")
      (dolist (loc locations)
        (insert (format "| %s | %d |\n" (car loc) (cdr loc))))
      (org-table-align)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))

    ;; Return locations alist
    locations))

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
  :ensure t
  :bind
  ("C-c n n" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n u" . org-roam-ui-open)
  ("C-c n l" . (lambda ()
                 (interactive)
                 (gt/dailies-location-stats
                  (concat org-roam-directory org-roam-dailies-directory))))
  ("C-c j j" . org-roam-dailies-goto-today)
  ("C-c j i" . org-roam-dailies-capture-today))

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

(use-package! typo
  :config
  (typo-global-mode 1)
  (add-hook 'text-mode-hook 'typo-mode))

(defun gt/visual-line-range ()
  (save-excursion
    (cons
     (progn (beginning-of-visual-line) (point))
     (progn (end-of-visual-line) (point)))))

;; (use-package! quail-russian-qwerty)

;; TODO: Fix this
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

(use-package! corfu
  :config
  (setq corfu-preselect 'first
        corfu-preview-current 'insert))

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

(after! ledger-mode
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode)))
