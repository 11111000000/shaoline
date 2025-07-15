;;; Generated package description from shaoline.el  -*- no-byte-compile: t -*-
(define-package "shaoline" "2.0" "." '((emacs "27.1")) :kind vc :commit "83f67f8cdba4e953b766673a48000b22b9e3e183")

;; For NIXOS configuration: Make dependencies optional.
;; Install optional packages via nix-env, e.g., nix-env -iA nixpkgs.emacsPackages.all-the-icons
;; Shaoline will load them lazily if available, without polluting global space.
;; For flake integration, consider adding to your home-manager or system config:
;; {
;;   environment.systemPackages = [ pkgs.emacsPackages.shaoline ];
;;   # Add optional deps as needed.
;; }

;; Optional: shaoline-enable-dynamic-segments (customize to disable battery/time for minimal deps)
