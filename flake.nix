{
  description = "a-peirogon website built with Hakyll";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Definimos el entorno de Haskell con TODAS las librerías necesarias
        ghcWithPackages = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          hakyll
          pandoc
          pandoc-types       # Necesario para manipular el AST (Walk)
          skylighting        # Para detección de sintaxis
          cryptohash-sha256  # Para el hash SHA256
          base16-bytestring  # Para codificar el hash
          hashable           # Utilidad general
          process            # Para ejecutar scripts externos
          directory
          filepath
        ]);

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # 1. El compilador de Haskell con las librerías listas
            ghcWithPackages

            # 2. Herramientas del sistema necesarias para compilar librerías C (zlib)
            zlib
            zlib.dev
            pkg-config

            # 3. Herramientas externas que usa tu site.hs (Runtime)
            python3Packages.pygments  # Provee el comando 'pygmentize'
            pdf2svg                   # Convierte PDF a SVG para TikZ

            # Entorno LaTeX completo para TikZ (incluye standalone, tikz, etc.)
            (texlive.combine {inherit (texlive) scheme-medium standalone preview pgf;})

            # Utilidad para recargar automáticamente
            entr
          ];

          shellHook = ''
            export PKG_CONFIG_PATH=${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH

            echo "=========================================="
            echo "   Entorno NixOS cargado correctamente"
            echo "   - Haskell + Deps: Listo"
            echo "   - Pygments:       $(which pygmentize)"
            echo "   - LaTeX/TikZ:     Detectado"
            echo "=========================================="
            echo "Para compilar rápido usa:"
            echo "  ghc --make site.hs && ./site build"
          '';
        };
      }
    );
}
