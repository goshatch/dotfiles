;;; org.banqiao.el --- Work-specific org configuration -*- lexical-binding: t; -*-

(setq org-clock-idle-time 15
      org-duration-format (quote h:mm))

;;;; Work log entry insertion
;; Function to insert a new log entry with template

(defun gt/insert-work-log-entry ()
  "Insert a new log entry under the Log heading with today's date and template.
Removes VISIBILITY property from previous first entry and adds it to new entry."
  (interactive)
  (save-excursion
    ;; Find the Log heading
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Log$" nil t)
      (error "Could not find Log heading"))

    ;; Move past the properties drawer if it exists
    (forward-line 1)
    (when (looking-at "^:PROPERTIES:")
      (re-search-forward "^:END:" nil t)
      (forward-line 1))

    ;; Check if there's already an entry for today
    (let ((today-heading (format "** [%s]" (format-time-string "%Y-%m-%d %a"))))
      (save-excursion
        (when (re-search-forward (regexp-quote today-heading) (save-excursion (outline-next-heading) (point)) t)
          (error "Log entry for today already exists"))))

    ;; Remove VISIBILITY property from the current first child (if it exists)
    (when (and (outline-next-heading)
               (= (org-current-level) 2))
      (org-delete-property "VISIBILITY"))

    ;; Go back to insert position (after Log properties)
    (goto-char (point-min))
    (re-search-forward "^\\* Log$" nil t)
    (forward-line 1)
    (when (looking-at "^:PROPERTIES:")
      (re-search-forward "^:END:" nil t)
      (forward-line 1))

    ;; Insert new entry
    (insert (format "** [%s]\n" (format-time-string "%Y-%m-%d %a")))
    (insert ":PROPERTIES:\n")
    (insert ":VISIBILITY: all\n")
    (insert ":END:\n")
    (insert "[0/6] Today's plan:\n")
    (insert "- [ ] [0/3] Admin\n")
    (insert "  - [ ] Update weekly goals if needed\n")
    (insert "  - [ ] Update timesheets\n")
    (insert "  - [ ] Update checkout\n")
    (insert "\n"))

  ;; Move cursor to after the last checkbox so user can start adding tasks
  (goto-char (point-min))
  (re-search-forward "^\\* Log$" nil t)
  (outline-next-heading)
  (re-search-forward "- \\[ \\] Update checkout" nil t)
  (end-of-line)
  (insert "\n- [ ] ")
  (message "New log entry created for %s" (format-time-string "%Y-%m-%d")))

;; Keybinding for inserting work log entry
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert work log entry" "w" #'gt/insert-work-log-entry)

;;; org.banqiao.el ends here
