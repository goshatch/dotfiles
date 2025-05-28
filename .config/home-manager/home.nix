{ config, pkgs, ... }:

{
  home.stateVersion = "23.11";

  programs = {
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
        export PATH="$HOME/.qlot/bin:$HOME/.config/emacs/bin:$PATH"

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

        # work stuff
        [ -f "$HOME/.indeed-kube-profile" ] && source "$HOME/.indeed-kube-profile"
      '';
    };
    
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    
    starship = {
      enable = true;
      settings = {
        add_newline = false;
      };
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
        };
      };
      enableZshIntegration = true;
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
    haskell.compiler.ghc96
    haskellPackages.cabal-install
    haskellPackages.hoogle
    haskell-language-server
    stack
  ];
}
