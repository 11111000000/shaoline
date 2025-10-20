{
  description = "Shaoline.el - build, lint and run ERT tests in batch";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
  in
  {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [
          emacs-nox
          ripgrep
          gnumake
          git
        ];
        shellHook = ''
          echo "Shaoline devShell"
          echo "Run tests:    make test"
          echo "Byte-compile: make byte-compile"
          echo "Checkdoc:     make checkdoc"
          echo "QA sweep:     make qa"
          echo "Lint (installs package-lint from MELPA): make lint"
          echo "Or via flake apps:"
          echo "  nix run .#tests"
          echo "  nix run .#byte-compile"
          echo "  nix run .#checkdoc"
          echo "  nix run .#lint"
        '';
      };
    });

    # nix run .#tests, .#byte-compile, .#lint
    apps = forAllSystems (pkgs:
      let
        emacs = pkgs.emacs-nox;
        testsDrv = pkgs.writeShellApplication {
          name = "shaoline-tests";
          runtimeInputs = [ emacs pkgs.gnumake ];
          text = ''
            set -euo pipefail
            exec make test
          '';
        };
        byteCompileDrv = pkgs.writeShellApplication {
          name = "shaoline-byte-compile";
          runtimeInputs = [ emacs pkgs.gnumake ];
          text = ''
            set -euo pipefail
            exec make byte-compile
          '';
        };
        checkdocDrv = pkgs.writeShellApplication {
          name = "shaoline-checkdoc";
          runtimeInputs = [ emacs pkgs.gnumake ];
          text = ''
            set -euo pipefail
            exec make checkdoc
          '';
        };
        lintDrv = pkgs.writeShellApplication {
          name = "shaoline-lint";
          runtimeInputs = [ emacs pkgs.gnumake ];
          text = ''
            set -euo pipefail
            # Uses Makefile 'lint' target (installs package-lint from MELPA)
            exec make lint
          '';
        };
      in rec {
        tests = { type = "app"; program = "${testsDrv}/bin/shaoline-tests"; };
        byte-compile = { type = "app"; program = "${byteCompileDrv}/bin/shaoline-byte-compile"; };
        checkdoc = { type = "app"; program = "${checkdocDrv}/bin/shaoline-checkdoc"; };
        lint = { type = "app"; program = "${lintDrv}/bin/shaoline-lint"; };
        default = tests;
      });

    # nix flake check
    checks = forAllSystems (pkgs:
      let
        emacs = pkgs.emacs-nox;
      in {
        # Run ERT for all test suites (no network)
        ert = pkgs.runCommand "shaoline-ert" { buildInputs = [ emacs pkgs.ripgrep ]; } ''
          set -euo pipefail
          cp -r ${./lisp} ./lisp
          cp -r ${./test} ./test
          ${emacs}/bin/emacs -Q --batch -L lisp -l ert \
            -l test/shaoline-core-test.el \
            -l test/shaoline-cached-segment-test.el \
            -l test/shaoline-effects-test.el \
            -l test/shaoline-strategy-test.el \
            -f ert-run-tests-batch-and-exit
          touch $out
        '';

        # Ensure sources byte-compile cleanly (no network)
        byte-compile = pkgs.runCommand "shaoline-byte-compile" { buildInputs = [ emacs ]; } ''
          set -euo pipefail
          cp -r ${./lisp} ./lisp
          ${emacs}/bin/emacs -Q --batch -L lisp -f batch-byte-compile lisp/*.el
          touch $out
        '';
      });
  };
}
