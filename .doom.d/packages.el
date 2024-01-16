;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t

(package! evil-matchit)
(package! modus-themes)
(package! olivetti)
(package! typo)
(package! nov :recipe (:type git
                       :host nil
                       :repo "https://depp.brause.cc/nov.el.git"
                       :files ("*.el")))

(package! lsp-sonarlint)
;; NOTE: Use the below if we need a local dev version
;; (package! lsp-sonarlint :recipe (:local-repo "/Users/gosha/repos/lsp-sonarlint"
;;                                  :build (:not compile)))

;; websocket is required by org-roam-ui
(package! websocket)
(package! org-roam-ui :recipe (:host github
                               :repo "org-roam/org-roam-ui"
                               :files ("*.el" "out")))

;; DAP mode
(package! dap-mode)

;; (package! map :pin "bb50dbaafc0f71743bd9ffd5784258a9fd682c20")

;; Russian QWERTY layout
(package!
  quail-russian-qwerty
  :recipe (:local-repo "/Users/gosha/repos/quail-russian-qwerty"
           :build (:not compile)))
  ;; :recipe (:host github
  ;;          :repo "goshatch/quail-russian-qwerty"
  ;;          :files ("*.el")))


;; Automatic dark/light switching
(package! auto-dark)

;; Word count in modeline
;; https://github.com/bnbeckwith/wc-mode
;; http://bnbeckwith.com/code/word-count-mode.html
(package! wc-mode :recipe (:host github
                           :repo "bnbeckwith/wc-mode"
                           :files ("*.el")))

(package! uxntal-mode :recipe (:host github
                               :repo "xaderfos/uxntal-mode"
                               :files ("*.el")))

(package! calibredb)
(package! nov-xwidget :recipe (:host github
                               :repo "chenyanming/nov-xwidget"
                               :files ("*.el")))

;; (package! lsp-ltex)

;; (package! doom-modeline :pin "918730eff72e")
