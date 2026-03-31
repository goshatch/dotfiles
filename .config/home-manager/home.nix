{ config, pkgs, lib, ... }:

{
  home.stateVersion = "23.11";

  xdg = {
    enable = true;
    configHome = "${config.home.homeDirectory}/.config";
    dataHome = "${config.home.homeDirectory}/.local/share";
    cacheHome = "${config.home.homeDirectory}/.cache";
    stateHome = "${config.home.homeDirectory}/.local/state";
  };

  programs = {
    try = {
      enable = true;
      path = "~/repos/tries";
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      autosuggestion.enable = true;
      dotDir = config.home.homeDirectory;

      shellAliases = {
        vim = "nvim";
        g = "git";
        be = "bundle exec";
        mux = "tmuxinator";
        ls = "eza --icons";
        la = "ls -la";
        ll = "ls -l";
        cat = "bat";
        more = "bat";
        dc = "docker compose";
        dcu = "docker compose up -d";
        dcr = "docker compose run";
        lg = "lazygit";
        cop = "bundle exec rubocop";
        copm = "git diff --name-only | grep '\\.rb$' | xargs bundle exec rubocop";
        copma = "git diff --name-only | grep '\\.rb$' | xargs bundle exec rubocop -A";
        cops = "git diff --cached --name-only --diff-filter=ACM | grep '\\.rb$' | xargs bundle exec rubocop";
        copsa = "git diff --cached --name-only --diff-filter=ACM | grep '\\.rb$' | xargs bundle exec rubocop -A";
        git-tree = "git ls-files | tree --fromfile";
      };

      envExtra = ''
        export EDITOR=hx
        export GOPATH="$HOME/repos/go"
        export BAT_THEME=ansi
        export PLAYDATE_SDK_PATH="$HOME/Developer/PlaydateSDK"
        export TEX_PATH="/Library/TeX/texbin"
        export CPATH="/opt/homebrew/include:$CPATH"
        export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"
        export XDG_CONFIG_HOME="$HOME/.config"
        export JAVA_HOME="${pkgs.openjdk21}/lib/openjdk"

        export PATH="./bin:$HOME/.bin:\
        .git/safe/../../bin:\
        $HOME/.cargo/bin:\
        $HOME/.local/bin:\
        $GOPATH/bin:\
        $HOME/.qlot/bin:\
        $HOME/.config/emacs/bin:\
        $HOME/.claude/local:\
        $HOME/.nix-profile/bin:\
        /opt/homebrew/bin:\
        $PLAYDATE_SDK_PATH/bin:\
        $TEX_PATH:\
        $PATH"
      '';

      initContent = ''
        # homebrew
        eval "$(/opt/homebrew/bin/brew shellenv)"
        
        # dotfiles git wrapper
        conf() {
          git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME "$@"
        }

        # uxn wrapper
        uxn() {
          uxnemu -2x "$HOME/roms/$1.rom" "$@"
        }

        # ghcup
        [ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

        # fetch trunk and rebase
        greb() {
          local trunk=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@')
          trunk="''${trunk:-master}"
          git fetch origin "$trunk":"$trunk" && git rebase "$trunk"
        }

        # opam
        [[ ! -r "/Users/gosha/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
      '';
    };
    
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    
    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = lib.mkMerge [
        (builtins.fromTOML (builtins.readFile "${pkgs.starship}/share/starship/presets/nerd-font-symbols.toml"))
        {
          bun.symbol = lib.mkForce "Bun ";
        }
      ];
    };
    
    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    mise = {
      enable = true;
      globalConfig = {
        tools = {
          ruby = "latest";
          node = "lts";
        };
        settings = {
          experimental = true;
          idiomatic_version_file_enable_tools = ["ruby"];
        };
      };
      enableZshIntegration = true;
    };

    tmux = {
      enable = true;
      baseIndex = 1;
      escapeTime = 0;
      historyLimit = 50000;
      mouse = true;
      prefix = "C-a";
      terminal = "tmux-256color";
      extraConfig = ''
        set -ag terminal-overrides ",*:Tc,*:RGB"
        set -g pane-base-index 1
        set -g renumber-windows on
        set -g status-position bottom
        set-hook -g client-dark-theme {
          set-option -g status-style bg=#404040,fg=#f0f0f0
          set-option -g status-left "#[bg=#404040,fg=#f0f0f0,bold]#{?client_prefix,,  tmux  }#[bg=#79a8ff,fg=#1e1e1e,bold]#{?client_prefix,  tmux  ,}"
          set-option -g status-right "#S"
          set-option -g window-status-format " #I:#W "
          set-option -g window-status-current-format "#[bg=#79a8ff,fg=#1e1e1e] #I:#W#{?window_zoomed_flag,  , }"
        }

        set-hook -g client-light-theme {
          set-option -g status-style bg=#cab9b2,fg=#0a0a0a
          set-option -g status-left "#[bg=#cab9b2,fg=#0a0a0a,bold]#{?client_prefix,,  tmux  }#[bg=#3548cf,fg=#efe9dd,bold]#{?client_prefix,  tmux  ,}"
          set-option -g status-right "#S"
          set-option -g window-status-format " #I:#W "
          set-option -g window-status-current-format "#[bg=#3548cf,fg=#efe9dd] #I:#W#{?window_zoomed_flag,  , }"
        }
        bind | split-window -h
        bind - split-window -v
        unbind '"'
        unbind %
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R
        bind r source-file ~/.config/tmux/tmux.conf \; display "reloaded"
      '';
    };
  };

  home.activation.configureDotfilesRepo = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if [ ! "$(${pkgs.git}/bin/git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME config --local --get status.showUntrackedFiles 2>/dev/null)" = "no" ]; then
      $DRY_RUN_CMD ${pkgs.git}/bin/git --git-dir=$HOME/repos/dotfiles/ --work-tree=$HOME config --local status.showUntrackedFiles no
    fi
  '';

  home.packages = with pkgs; [
    git
    jujutsu
    delta
    eza
    bat
    tree
    pgcli
    lazygit
    tmuxinator
    rubocop
    gh
    bun
    pnpm
    openjdk21
    typescript-language-server
    nix-direnv
    pass
    cmake
    ripgrep
    emacs-lsp-booster
    # haskell.compiler.ghc98
    haskell-language-server
    # haskellPackages.cabal-install
    # haskellPackages.hoogle
    # stack
  ];
}
