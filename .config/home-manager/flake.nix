{
  description = "home-manager config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    try.url = "github:tobi/try";
  };

  outputs = { nixpkgs, home-manager, try, ... }: {
    homeConfigurations.strogino = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [
        try.homeManagerModules.default
        ./home.nix
        {
          home.username = "gosha";
          home.homeDirectory = "/Users/gosha";
        }
      ];
    };
    homeConfigurations.banqiao = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [
        try.homeManagerModules.default
        ./home.nix
        {
          home.username = "gosha";
          home.homeDirectory = "/Users/gosha";
        }
      ];
    };
  };
}
