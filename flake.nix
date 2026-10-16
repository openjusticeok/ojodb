{
  description = "ojodb — query the Open Justice Oklahoma database (R package)";

  inputs = {
    nixpkgs.url = "github:rstats-on-nix/nixpkgs/r-daily";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      perSystem = { pkgs, ... }:
        let
          # Runtime deps (DESCRIPTION: Imports + Depends).
          rImports = with pkgs.rPackages; [
            cli
            dbplyr
            DBI
            digest
            dplyr
            glue
            lubridate
            pool
            purrr
            rlang
            RPostgres
            stringi
            stringr
            tibble
            tidyr
            withr
            yaml
          ];

          rDev = with pkgs.rPackages; [
            knitr
            rmarkdown
            devtools
            roxygen2
            testthat
            rcmdcheck
            pkgload
            pkgbuild
          ];

          ojodb = pkgs.rPackages.buildRPackage {
            name = "ojodb";
            src = ./.;
            propagatedBuildInputs = rImports;
            nativeBuildInputs = with pkgs.rPackages; [ knitr rmarkdown ];
          };
        in
        {
          packages = {
            inherit ojodb;
            default = ojodb;
          };

          devShells.default = pkgs.mkShell {
            name = "ojodb-dev";
            buildInputs = [ pkgs.R ] ++ rImports ++ rDev;
            packages = with pkgs; [ jarl arf air-formatter ];
            shellHook = ''
              echo "ojodb dev shell — R -e 'devtools::load_all()'"
            '';
          };
        };
    };
}
