{
  description =
    "Typed PEGs with an implicit stack: Agda strong-normalization proof and Racket prototype";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Agda with the standard library. The proof in src/pest-sn typechecks
        # with the Agda and agda-stdlib pinned by this flake's nixpkgs.
        agdaWithStdlib =
          pkgs.agda.withPackages (p: [ p.standard-library ]);

        # Everything needed to typecheck the proof, run the grammar experiments
        # and rebuild the paper.
        #
        # z3 is NOT required: the prototype infers types through the Rosette
        # back-end, and Rosette bundles its own solver. It is included here only
        # so that the corpus runners can also print the independent Z3 column.
        common = [
          agdaWithStdlib
          pkgs.racket
          pkgs.z3
          pkgs.gnumake
          pkgs.git
        ];

        # Harvesting the real grammars again (make harvest) needs Python and
        # network access; the translated grammars are committed, so this is
        # optional.
        harvest = [ pkgs.python3 ];

        # Rebuilding the PDF from the literate Agda source (make paper).
        paper = [
          (pkgs.texlive.combine {
            inherit (pkgs.texlive)
              scheme-medium elsarticle latexmk lineno semantic lkproof mathtools
              unicode-math newunicodechar fontspec lm dejavu-otf tex-gyre-math
              listings xcolor hyperref amsmath amsfonts xifthen ifmtarg
              xstring environ trimspaces catchfile polytable lazylist ucs;
          })
          pkgs.dejavu_fonts
        ];
      in {
        devShells.default = pkgs.mkShell {
          packages = common ++ harvest ++ paper;

          shellHook = ''
            echo "typed-peg-stack development environment"
            echo "  agda    : $(agda --version | head -1)"
            echo "  racket  : $(racket --version)"
            echo "  z3      : $(z3 --version)  (optional, cross-check only)"
            echo
            echo "  make verify       typecheck the Agda proof"
            echo "  make experiments  run both grammar corpora"
            echo "  make paper        rebuild the PDF"
          '';
        };

        # A minimal shell for reviewers who only want to check the proof.
        devShells.agda = pkgs.mkShell {
          packages = [ agdaWithStdlib pkgs.gnumake ];
        };

        # `nix build` typechecks the formalization and fails if it does not.
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "pest-sn";
          version = "1.0";
          src = ./src/pest-sn;
          nativeBuildInputs = [ agdaWithStdlib ];
          buildPhase = ''
            agda src/Pest/MainTheorem.agda
          '';
          installPhase = ''
            mkdir -p $out
            cp -r src $out/
          '';
        };

        checks.default = self.packages.${system}.default;
      });
}
