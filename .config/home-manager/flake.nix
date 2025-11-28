{
  description = "home-manager config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    try.url = "github:tobi/try";
  };

  outputs = { nixpkgs, home-manager, emacs-lsp-booster, try, ... }: {
    homeConfigurations.strogino = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        system = "aarch64-darwin";
        overlays = [ emacs-lsp-booster.overlays.default ];
      };
      modules = [
        try.homeModules.default
        ./home.nix
        {
          home.username = "gosha";
          home.homeDirectory = "/Users/gosha";
          home.packages = [ home-manager.packages.aarch64-darwin.default ];
        }
      ];
    };
    homeConfigurations.banqiao = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        system = "aarch64-darwin";
        overlays = [ emacs-lsp-booster.overlays.default ];
      };
      modules = [
        try.homeModules.default
        ./home.nix
        {
          home.username = "gosha";
          home.homeDirectory = "/Users/gosha";
          home.packages = [ home-manager.packages.aarch64-darwin.default ];
        }
      ];
    };
  };
}
