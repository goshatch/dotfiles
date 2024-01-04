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

export EDITOR=nvim

function conf {
    git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME "$@"
}

export FZF_DEFAULT_COMMAND="rg --files --follow"
export GOPATH="$HOME/repos/go"
export EMACSPATH=$PATH
export BAT_THEME=DarkNeon
export PLAYDATE_SDK_PATH="$HOME/Developer/PlaydateSDK"
export TEX_PATH="/Library/TeX/texbin"
export CPATH="/opt/homebrew/include:$CPATH"
export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"

new_path=$(bash $HOME/.indeed-kube-profile)
export PATH="$new_path:$PATH"

