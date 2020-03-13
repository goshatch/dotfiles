(setq user-full-name "Gueorgui Tcherednitchenko"
      user-mail-address "gt@gueorgui.net")

;; (setq doom-font (font-spec :family "Source Code Pro" :size 13))

(setq doom-font (font-spec :family "Iosevka" :size 26))

(setq doom-unicode-font
      (if IS-MAC
          (font-spec :family "Apple Color Emoji")
        (font-spec :family "Twitter Color Emoji"))
      )

(setq doom-theme 'doom-one)

(setq evil-escape-key-sequence "jj"
      evil-escape-delay 0.3)

(setq frame-title-format '((:eval (projectile-project-name))))

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(setq org-ellipsis " ⤵")

(setq org-startup-with-inline-images t)

(setq org-directory "~/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/org/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq +org-capture-todo-file (org-file-path "work.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun gt/copy-tasks-from-inbox ()
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

(setq org-agenda-files (list org-index-file
                             (org-file-path "recurring-events.org")
                             (org-file-path "work.org")))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))

(defun gt/mark-done-and-archive ()
  "Mark the state of and org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-x C-s") 'gt/mark-done-and-archive)))

(setq org-log-done 'time)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-span 14)
(setq org-agenda-start-on-weekday nil)

(setq calendar-week-start-day 1)

(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                 (todo . " %i ")
                                 (tags . " %i ")
                                 (search . " %i ")))

(require 'org-habit)

(defun gt/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun gt/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("p" "Personal agenda"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'todo '("DONE" "PENDING" "BLOCKED")))
                 (org-agenda-overriding-header "Today's high-priority tasks:")))
          (agenda "")
          (todo "TODO"
                ((org-agenda-skip-function '(or (gt/org-skip-subtree-if-priority ?A)
                                                (gt/org-skip-subtree-if-habit)))
                 (org-agenda-overriding-header "Other tasks:")))
          (todo "PENDING"
                ((org-agenda-skip-function '(gt/org-skip-if-priority ?A))
                 (org-agenda-overriding-header "Waiting to hear about these:")))))))

(defun gt/dashboard ()
  (interactive)
  (gt/copy-tasks-from-inbox)
  (org-agenda nil "p"))

(global-set-key (kbd "C-c d") 'gt/dashboard)

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq org-capture-templates
      '(
        ("b" "Blog idea"
         entry
         (file "~/org/blog-ideas.org")
         "* %?\n")
        ("f" "Finished book"
         table-line
         (file "~/org/books-read.org")
         "| %^{Title} | %^{Author} | %u | %^{Rating} | %^{Notes}")
        ("v" "Travel log"
         table-line
         (file "~/org/travel.org")
         "| %^{Place} | %^{Country} | %^{Region} | %^{Date arrived}t | %^{Date left}t | %^{Notes}")
        ("c" "Coffee"
         table-line
         (file "~/org/coffee.org")
         "| %^{Day}t | %^{Beans brand} | %^{Country} | %^{Grams} | %^{Grinder setting} | %^{Seconds} | %^{Rating} | %^{Notes}")
        ("r" "Reading list"
         checkitem
         (file "~/org/reading-list.org"))
        ("w" "Watching list"
         checkitem
         (file "~/org/watching-list.org"))
        ("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n")
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file "Inbox")
         "* %U %?\n%i\n%a" :prepend t)
        ("z" "Business idea"
         entry
         (file "~/org/business-ideas.org")
         "* %?\n")

        ;; Will use {org-directory}/{+org-capture-projects-file} and store
        ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
        ;; support `:parents' to specify what headings to put them under, e.g.
        ;; :parents ("Projects")
        ("p" "Centralized templates for projects")
        ("pt" "Project todo" entry
         (function +org-capture-central-project-todo-file)
         "* TODO %?\n %i\n %a"
         :heading "Tasks"
         :prepend nil)
        ("pn" "Project notes" entry
         (function +org-capture-central-project-notes-file)
         "* %U %?\n %i\n %a"
         :heading "Notes"
         :prepend t)
        ("pc" "Project changelog" entry
         (function +org-capture-central-project-changelog-file)
         "* %U %?\n %i\n %a"
         :heading "Changelog"
         :prepend t)))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(defun gt/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (gt/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'gt/open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)

(global-evil-matchit-mode 1)

(setq-default standard-indent 2)

(setq-default js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(defun gt/typescript-mode-setup ()
  "Custom setup for Typescript mode"
  (setq flycheck-checker 'javascript-eslint)
  )
(add-hook 'typescript-mode-hook 'gt/typescript-mode-setup)

(setq-default css-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))

(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)

(setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")

(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'gfm-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)

(eval-when-compile
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'use-package))
(use-package mu4e
  :config
  (remove-hook 'mu4e-main-mode-hook 'evil-collection-mu4e-update-main-view))

(setq +mu4e-backend 'offlineimap)

(set-email-account! "Fastmail"
  '((mu4e-sent-folder       . "/Fastmail/Sent")
    (mu4e-drafts-folder     . "/Fastmail/Drafts")
    (mu4e-trash-folder      . "/Fastmail/Trash")
    (mu4e-refile-folder     . "/Fastmail/Archive")
    (smtpmail-smtp-user     . "gt@gueorgui.net")
    (user-mail-address      . "gt@gueorgui.net")
    (mu4e-compose-signature . "https://gueorgui.net"))
  t)

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.fastmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
