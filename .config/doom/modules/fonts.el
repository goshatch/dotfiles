;;; fonts.el --- Typography configuration -*- lexical-binding: t; -*-

;; Let's provide a base font size, from which all others will be calculated.

(defvar gt/base-font-size 13
  "The base font size from which all others are calculated")

;; Setting up the default font faces:

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

;; If we are on a Mac, use the Apple emoji font; otherwise (on Linux)
;; use the Twitter Color Emoji font.

(setq doom-symbol-font
      (if (featurep :system 'macos)
          (font-spec :family "Apple Color Emoji")
        (font-spec :family "Twitter Color Emoji"))
      )

;; Specify a font for CJK text, for better performance — but only in GUI mode.

(if (display-graphic-p)
    (progn
      (when (member "Noto Sans CJK JP" (font-family-list))
        (dolist (charset '(kana han))
          (set-fontset-font t charset (font-spec :family "Noto Sans CJK JP" :size gt/base-font-size) nil 'prepend)))
      (when (member "Noto Sans CJK TC" (font-family-list))
        (dolist (charset '(han cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "Noto Sans CJK TC" :size gt/base-font-size) nil 'append)))))

;; TODO: Have a different config for Mac and Linux:
;; - On macOS, use the beautiful system fonts
;; - On Linux, use Noto Sans CJK

;; Force using the Russian-specific font (Iosevka) for cyrillic text:

(if (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'cyrillic
     gt/ru-font))

;; Add a little more line height:

(setq-default line-spacing 0.0)

;;; fonts.el ends here
