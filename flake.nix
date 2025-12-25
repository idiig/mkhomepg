{
  description = "Dev environment my home page";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # Expose outputs for all major platforms
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      perSystem = { pkgs, inputs', lib, system, ... }: {
        # Override the provider's default devShell while reusing it as a base
        devShells.default = lib.mkForce (pkgs.mkShell {
          packages = with pkgs; [
            gnum4
            gnumake
            guile
            emacs
          ];
          
          shellHook = ''
          '';
        });
      };
    };
}
