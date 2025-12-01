;;; reading.el --- Reading and book management configuration -*- lexical-binding: t; -*-

;;;; Calibre library interaction

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

;;;; Use nov.el for EPUB files

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

;;; reading.el ends here
