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
    nushell = {
      enable = true;
     
      settings = {
        show_banner = false;
      };

      shellAliases = {
        ll = "ls -l";
        la = "ls -la";
        vim = "nvim";
        g = "git";
        be = "bundle exec";
        cat = "bat";
        more = "bat";
        gg = "lazygit";
        git-tree = "git ls-files | tree --fromfile";
      };

      environmentVariables = {
        EDITOR = "hx";
        BAT_THEME = "ansi";
        XDG_CONFIG_HOME = "${config.home.homeDirectory}/.config";
        XDG_DATA_HOME = "${config.home.homeDirectory}/.local/share";
        XDG_CACHE_HOME = "${config.home.homeDirectory}/.cache";
        XDG_STATE_HOME = "${config.home.homeDirectory}/.local/state";
      };

      extraEnv = ''
        $env.GOPATH = $"($env.HOME)/repos/go"
        $env.PLAYDATE_SDK_PATH = $"($env.HOME)/Developer/PlaydateSDK"
        $env.TEX_PATH = "/Library/TeX/texbin"

        $env.CPATH = if ($env.CPATH? | is-empty) { 
            "/opt/homebrew/include" 
        } else { 
            $"/opt/homebrew/include:($env.CPATH)" 
        }        
        $env.LIBRARY_PATH = if ($env.LIBRARY_PATH? | is-empty) { 
            "/opt/homebrew/lib" 
        } else { 
            $"/opt/homebrew/lib:($env.LIBRARY_PATH)" 
        }
        
        $env.JAVA_HOME = (do { /usr/libexec/java_home -v 21 } | complete | get stdout | str trim)
        
        $env.PATH = ($env.PATH | split row (char esep) | prepend [
          "./bin"
          ($env.HOME | path join ".bin")
          ".git/safe/../../bin"
          ($env.HOME | path join ".nix-profile/bin")
          "/nix/var/nix/profiles/default/bin"
          ($env.HOME | path join ".cargo/bin")
          ($env.HOME | path join "repos/go/bin")
          ($env.HOME | path join ".qlot/bin")
          ($env.HOME | path join ".config/emacs/bin")
          ($env.HOME | path join ".claude/local")
          "/opt/homebrew/bin"
          ($env.HOME | path join "Developer/PlaydateSDK/bin")
          "/Library/TeX/texbin"
          ($env.HOME | path join ".ghcup/bin")
          ($env.HOME | path join ".cabal/bin")
        ] | where ($it | path exists))
      '';

      extraConfig = ''
        use std/config light-theme
        $env.config.color_config = (light-theme)

        # dotfiles git wrapper
        def conf [...args] {
          git $"--git-dir=($env.HOME)/repos/dotfiles" $"--work-tree=($env.HOME)" ...$args
        }

        # uxn wrapper  
        def uxn [rom: string, ...args] {
          uxnemu -2x $"($env.HOME)/roms/($rom).rom" ...$args
        }
      '';
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      autosuggestion.enable = true;

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
        psql = "pgcli";
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
        export JAVA_HOME="$(${pkgs.coreutils}/bin/env /usr/libexec/java_home -v 21)"

        export PATH="./bin:$HOME/.bin:\
        .git/safe/../../bin:\
        $HOME/.cargo/bin:\
        $GOPATH/bin:\
        $HOME/.qlot/bin:\
        $HOME/.config/emacs/bin:\
        $HOME/.claude/local: \
        /opt/homebrew/bin:\
        $PLAYDATE_SDK_PATH/bin:\
        $TEX_PATH:\
        $PATH"
      '';

      initContent = ''
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
      '';
    };
    
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNushellIntegration = true;
      nix-direnv.enable = true;
    };
    
    starship = {
      enable = true;
      enableNushellIntegration = true;
      settings = lib.mkMerge [
          (builtins.fromTOML (builtins.readFile "${pkgs.starship}/share/starship/presets/nerd-font-symbols.toml"))
          {
            add_newline = false;
            bun = {
              symbol = lib.mkForce "󰬉 ";
            };
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
      };
      settings = {
        experimental = true;
        idiomatic_version_file_enable_tools = ["ruby"];
      };
      enableZshIntegration = true;
      enableNushellIntegration = true;
    };

    tmux = {
      enable = true;
      baseIndex = 1;
      escapeTime = 0;
      historyLimit = 50000;
      mouse = true;
      prefix = "C-a";
      shell = "${pkgs.nushell}/bin/nu";
      extraConfig = ''
        set -g pane-base-index 1
        set -g renumber-windows on
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

  home.packages = with pkgs; [
    helix
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
    typescript-language-server
    nix-direnv
    # haskell.compiler.ghc98
    # haskell-language-server
    # haskellPackages.cabal-install
    # haskellPackages.hoogle
    # stack
  ];
}
