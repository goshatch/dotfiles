;;; org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;;; Org-roam utilities

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
                   (mdays (calendar-last-day-of-month pm py)))
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
  (let* ((query "SELECT id, title FROM nodes WHERE file LIKE '%daily%' AND file LIKE '%' || strftime('%m-%d', 'now') || '%' ORDER BY title DESC")
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

;;;; Basics 
;; Set the working directory for Org files.

(setq org-directory "~/org/")

;;;; Spacing
;; Add a blank line before every new heading and plain list items

(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . auto)))

;;;; TO-DO items
;; Log time items are closed

(setq org-log-done 'time)

;;;; Agenda
;; Build the agenda from work and project files, and add a global key binding
;; to the default agenda view:

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

;;;; Links DWIM
;; Code lifted from Emacs DWIM: do what ✨I✨ mean:
;; https://xenodium.com/emacs-dwim-do-what-i-mean/

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

;;;; Roam

;; Enable node link completion everywhere

(setq org-roam-completion-everywhere t)

;; Configure Roam buffer to show unlinked references as well

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

;; Use Xwidgets to open UI instead of system browser

(use-package! org-roam-ui
  :init
  (when (featurep 'xwidget-internal)
    (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))

;;;;; Journaling
;; Global hotkey to reach today's daily

(use-package! org-roam
  :bind
  ("C-c j j" . org-roam-dailies-goto-today)
  ("C-c j i" . org-roam-dailies-capture-today))

;; Set up a custom default template for dailies

;; Note: Helper functions gt/daily-location, gt/daily-weather, gt/daily-pregnancy-week-day,
;; gt/child-age, and gt/org-roam-on-this-day are defined in utils.el

(require 'cl-lib)
(require 'calendar)  ;; for calendar-last-day-of-month

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M> %?"
         :if-new (file+head
                  "%<%Y-%m-%d>.org"
                  "%[~/org/roam/templates/daily-template.org]"))))

;;;;; org-roam-ui

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;;;; Keybindings

(use-package! org-roam
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

;;;; Writing
;; Disable line numbers in org files and hide the emphasis markers. ~code~

(use-package! org
  :config
  (setq org-hide-emphasis-markers t
        org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :background "Transparent")
  (add-to-list 'org-todo-keyword-faces '("REVW" . +org-todo-onhold))
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)
)))

;; Word count:

(use-package! wc-mode
  :config
  (global-set-key "\C-cw" 'wc-mode))

;; NOTE: These are not the same
(setq doom-modeline-enable-word-count t)

;; Enable typo-mode for all text-mode buffers

(use-package! typo
  :config
  (typo-global-mode 1)
  (add-hook 'text-mode-hook 'typo-mode))

;;;; Anki
;; Quickly insert an Anki card

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

;; Tag autocomplete for Anki cards

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

;; Key bindings

(use-package! anki-editor
  :config
  (define-key org-mode-map (kbd "C-c n a a") #'gt/insert-anki-card)
  (define-key org-mode-map (kbd "C-c n a t") #'gt/anki-tags-autocomplete)
  (define-key org-mode-map (kbd "C-c n a p") #'anki-editor-push-notes)
  (which-key-add-key-based-replacements
    "C-c n a a" "Insert Anki card"
    "C-c n a t" "Select tags for card"
    "C-c n a p" "Push cards to Anki"))

;;;; Time Tracking

;;;;; Pomodoro
;; Keep the time spent on a killed pomodoro

(setq org-pomodoro-keep-killed-pomodoro-time t)

;; Don't play sounds on Pomodoro events (notifications are enough)

(setq org-pomodoro-play-sounds nil)

;;;;; Inline images
;; Set default inline image width to 500px, and show them on startup for files
;; that have them.

(setq org-image-actual-width 500
      org-startup-with-inline-images t)

;;;;; Capture frame parameters
;; Make sure the capture frame is centered on the screen

(nconc +org-capture-frame-parameters '((top . 0.5) (left . 0.5)))

;;; org.el ends here
