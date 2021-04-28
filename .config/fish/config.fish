set -U EDITOR nvim

function fish_greeting
  fortune
end

function bass_init
  bass export EDITOR=nvim
  bass source ~/.nvm/nvm.sh
end

alias vim="nvim"
alias g="git"
alias be="bundle exec"
alias mux="tmuxinator"
alias ls="exa"
alias cat="bat"
alias more="bat"
alias dc="docker compose"
alias dcu="docker compose up -d"
alias dcr="docker compose run"

set -x FZF_DEFAULT_COMMAND "rg --files --follow"
set -x GOPATH $HOME/go
set -x EMACSPATH $PATH

switch (uname)
  case Darwin
    set -x RUBY_CONFIGURE_OPTS "--with-openssl-dir=/usr/local/opt/openssl@1.1"
  case Linux
    set -x QT_STYLE_OVERRIDE Adwaita-Dark
    # set -x MOZ_ENABLE_WAYLAND 1
    alias open="xdg-open"
    alias pbcopy="xclip"
end

status --is-interactive; and source (rbenv init -|psub)

bass_init
eval (direnv hook fish)

# This is needed to give priority to per project binstubs in a Rails project over system-wide rbenv-installed gems.
# A project directory must be marked as trusted with `git trust` first.
set -x PATH $HOME/.bin .git/safe/../../bin $GOPATH/bin $HOME/.emacs.d/bin $HOME/.yarn/bin $HOME/.config/yarn/global/node_modules/.bin $HOME/.rbenv/bin $PATH
