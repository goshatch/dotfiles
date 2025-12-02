;;; config.el --- Gosha's Doom Emacs Config -*- lexical-binding: t; -*-
;;;
;;;   ___        _         _                    
;;;  / __|___ __| |_  __ _( )___                
;;; | (_ / _ (_-< ' \/ _` |/(_-<                
;;;  \___\___/__/_||_\__,_|_/__/                
;;; |   \ ___  ___ _ __   | __|_ __  __ _ __ ___
;;; | |) / _ \/ _ \ '  \  | _|| '  \/ _` / _(_-<
;;; |___/\___/\___/_|_|_| |___|_|_|_\__,_\__/__/
;;;  / __|___ _ _  / _(_)__ _                   
;;; | (__/ _ \ ' \|  _| / _` |                  
;;;  \___\___/_||_|_| |_\__, |                  
;;;                     |___/                   

(defvar gt/hostname (car (split-string (system-name) "\\."))
  "The hostname of the current machine (without domain).")

(defvar gt/modules-dir (expand-file-name "modules/" doom-user-dir)
  "Directory containing configuration modules.")

(defun gt/load-module (module-name)
  "Load MODULE-NAME.el and optionally MODULE-NAME.<hostname>.el if it exists."
  (let* ((base-file (expand-file-name (concat module-name ".el") gt/modules-dir))
         (host-file (expand-file-name (concat module-name "." gt/hostname ".el") gt/modules-dir)))
    ;; Load base module
    (when (file-exists-p base-file)
      (load base-file nil 'nomessage))
    ;; Load hostname-specific override if it exists
    (when (file-exists-p host-file)
      (load host-file nil 'nomessage))))

(gt/load-module "defaults")
(gt/load-module "fonts")
(gt/load-module "theme")
(gt/load-module "ui")
(gt/load-module "vc")
(gt/load-module "projectile")
(gt/load-module "programming")
(gt/load-module "org")
(gt/load-module "reading")
(gt/load-module "ledger")

;;; config.el ends here
