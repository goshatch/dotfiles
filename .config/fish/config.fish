set -U EDITOR nvim

function fish_greeting
  fortune
end

function bass_init
  bass export EDITOR=nvim
#  bass source ~/.nvm/nvm.sh
end

alias vim="nvim"
alias g="git"
alias be="bundle exec"
alias mux="tmuxinator"
alias ls="exa"
alias la="ls -la"
alias cat="bat"
alias more="bat"
alias dc="docker compose"
alias dcu="docker compose up -d"
alias dcr="docker compose run"
alias psql="pgcli"
alias lg="lazygit"
alias cop="bundle exec rubocop"


function conf
  git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME $argv
end

set -x FZF_DEFAULT_COMMAND "rg --files --follow"
set -x GOPATH $HOME/repos/go
set -x EMACSPATH $PATH
set -x BAT_THEME DarkNeon
set -x PLAYDATE_SDK_PATH $HOME/Developer/PlaydateSDK
set -x TEX_PATH "/Library/TeX/texbin"
set -x CPATH "/opt/homebrew/include" $CPATH
set -x LIBRARY_PATH "/opt/homebrew/lib" $LIBRARY_PATH

switch (uname)
  case Darwin
    set -x RUBY_CONFIGURE_OPTS "--with-openssl-dir=/usr/local/opt/openssl@1.1"
  case Linux
    set -x QT_STYLE_OVERRIDE Adwaita-Dark
    # set -x MOZ_ENABLE_WAYLAND 1
    alias open="xdg-open"
    alias pbcopy="xclip"
end

# This is needed to give priority to per project binstubs in a Rails project over system-wide rbenv-installed gems.
# A project directory must be marked as trusted with `git trust` first.
set -x PATH $HOME/.bin .git/safe/../../bin $HOME/.cargo/bin $GOPATH/bin $HOME/.emacs.d/bin $HOME/.yarn/bin $HOME/.config/yarn/global/node_modules/.bin $HOME/.rbenv/bin /opt/homebrew/bin $PLAYDATE_SDK_PATH/bin $TEX_PATH $PATH

bass_init
direnv hook fish | source

fish_vi_key_bindings
fish_vi_cursor

starship init fish | source

status --is-interactive; and . (rbenv init -|psub)

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /Users/gosha/.ghcup/bin $PATH # ghcup-env

set new_path (bash /Users/gosha/.indeed-kube-profile)
set -x PATH $new_path $PATH
# . "/Users/gosha/.indeed-kube-profile"
