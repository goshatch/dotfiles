eval "$(starship init zsh)"
eval "$(direnv hook zsh)"
eval "$(rbenv init - zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias vim="nvim"
alias g="git"
alias be="bundle exec"
alias mux="tmuxinator"
alias ls="exa --icons"
alias la="ls -la"
alias ll="ls -l"
alias cat="bat"
alias more="bat"
alias dc="docker compose"
alias dcu="docker compose up -d"
alias dcr="docker compose run"
alias psql="pgcli"
alias lg="lazygit"
alias cop="bundle exec rubocop"
alias copm="git diff --name-only | grep '\.rb$' | xargs bundle exec rubocop"
alias copma="git diff --name-only | grep '\.rb$' | xargs bundle exec rubocop -A"
alias cops="git diff --cached --name-only --diff-filter=ACM | grep '\.rb$' | xargs bundle exec rubocop"
alias copsa="git diff --cached --name-only --diff-filter=ACM | grep '\.rb$' | xargs bundle exec rubocop -A"

export EDITOR=nvim

conf() {
    git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME "$@"
}

uxn() {
    uxnemu -2x ~/roms/"$1".rom "$@"
}

export FZF_DEFAULT_COMMAND="rg --files --follow"
export GOPATH="$HOME/repos/go"
export BAT_THEME=DarkNeon
export PLAYDATE_SDK_PATH="$HOME/Developer/PlaydateSDK"
export TEX_PATH="/Library/TeX/texbin"
export CPATH="/opt/homebrew/include:$CPATH"
export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"

kube_path=$(bash $HOME/.indeed-kube-profile)

export PATH="$HOME/.bin:\
.git/safe/../../bin:\
$HOME/.cargo/bin:\
$GOPATH/bin:\
$HOME/.emacs.d/bin:\
$HOME/.yarn/bin:\
$HOME/.config/yarn/global/node_modules/.bin:\
$HOME/.rbenv/bin:\
/opt/homebrew/bin:\
$PLAYDATE_SDK_PATH/bin:\
$TEX_PATH:\
$kube_path:\
$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Automatically run nvm use when switching to a dir with a .nvmrc file
autoload -U add-zsh-hook

load-nvmrc() {
  local nvmrc_path
  nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version
    nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$(nvm version)" ]; then
      nvm use
    fi
  elif [ -n "$(PWD=$OLDPWD nvm_find_nvmrc)" ] && [ "$(nvm version)" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}

add-zsh-hook chpwd load-nvmrc
load-nvmrc

. "/Users/gosha/.indeed-kube-profile"
