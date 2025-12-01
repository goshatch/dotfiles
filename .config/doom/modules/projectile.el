;;; projectile.el --- Projectile configuration -*- lexical-binding: t; -*-

;; Automatically find projects in $HOME/repos

(setq projectile-project-search-path '("~/repos"))

;; Default action on opening a project is dired

(setq projectile-switch-project-action #'projectile-dired)

;; Recognize Rails/RSpec projects

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

;;; projectile.el ends here
