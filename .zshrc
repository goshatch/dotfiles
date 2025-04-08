eval "$(starship init zsh)"
eval "$(direnv hook zsh)"
# eval "$(rbenv init - zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias vim="nvim"
alias g="git"
alias be="bundle exec"
alias mux="tmuxinator"
alias ls="eza --icons"
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
alias git-tree="git ls-files | tree --fromfile"

export EDITOR=nvim

conf() {
    git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME "$@"
}

uxn() {
    uxnemu -2x ~/roms/"$1".rom "$@"
}

export FZF_DEFAULT_COMMAND="rg --files --follow"
export GOPATH="$HOME/repos/go"
export BAT_THEME=ansi
export PLAYDATE_SDK_PATH="$HOME/Developer/PlaydateSDK"
export TEX_PATH="/Library/TeX/texbin"
export CPATH="/opt/homebrew/include:$CPATH"
export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"
export XDG_CONFIG_HOME="$HOME/.config"

export PATH="./bin:$HOME/.bin:\
.git/safe/../../bin:\
$HOME/.cargo/bin:\
$GOPATH/bin:\
$HOME/.qlot/bin:\
$HOME/.config/emacs/bin:\
$HOME/.yarn/bin:\
$HOME/.config/yarn/global/node_modules/.bin:\
/opt/homebrew/bin:\
$PLAYDATE_SDK_PATH/bin:\
$TEX_PATH:\
$PATH"

. "/Users/gosha/.indeed-kube-profile"

export MISE_SHELL=zsh
export __MISE_ORIG_PATH="$PATH"

mise() {
  local command
  command="${1:-}"
  if [ "$#" = 0 ]; then
    command /opt/homebrew/bin/mise
    return
  fi
  shift

  case "$command" in
  deactivate|s|shell)
    # if argv doesn't contains -h,--help
    if [[ ! " $@ " =~ " --help " ]] && [[ ! " $@ " =~ " -h " ]]; then
      eval "$(command /opt/homebrew/bin/mise "$command" "$@")"
      return $?
    fi
    ;;
  esac
  command /opt/homebrew/bin/mise "$command" "$@"
}

_mise_hook() {
  eval "$(/opt/homebrew/bin/mise hook-env -s zsh)";
}
typeset -ag precmd_functions;
if [[ -z "${precmd_functions[(r)_mise_hook]+1}" ]]; then
  precmd_functions=( _mise_hook ${precmd_functions[@]} )
fi
typeset -ag chpwd_functions;
if [[ -z "${chpwd_functions[(r)_mise_hook]+1}" ]]; then
  chpwd_functions=( _mise_hook ${chpwd_functions[@]} )
fi

if [ -z "${_mise_cmd_not_found:-}" ]; then
    _mise_cmd_not_found=1
    [ -n "$(declare -f command_not_found_handler)" ] && eval "${$(declare -f command_not_found_handler)/command_not_found_handler/_command_not_found_handler}"

    function command_not_found_handler() {
        if /opt/homebrew/bin/mise hook-not-found -s zsh -- "$1"; then
          _mise_hook
          "$@"
        elif [ -n "$(declare -f _command_not_found_handler)" ]; then
            _command_not_found_handler "$@"
        else
            echo "zsh: command not found: $1" >&2
            return 127
        fi
    }
fi

# bun completions
[ -s "/Users/gosha/.bun/_bun" ] && source "/Users/gosha/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
# BEGIN env Setup -- Managed by Ansible DO NOT EDIT.

# Setup INDEED_ENV_DIR earlier.
if [ -z "${INDEED_ENV_DIR}" ]; then
    export INDEED_ENV_DIR="/Users/gosha/env"
fi

# Single-brace syntax because this is required in bash and sh alike
if [ -e "${INDEED_ENV_DIR}/etc/indeedrc" ]; then
    . "${INDEED_ENV_DIR}/etc/indeedrc"
fi
# END env Setup -- Managed by Ansible DO NOT EDIT.

# Created by `pipx` on 2024-10-30 15:28:33
export PATH="$PATH:/Users/gosha/.local/bin"
eval "$(mise activate zsh)"
