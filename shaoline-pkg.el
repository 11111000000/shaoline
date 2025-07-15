;;; Generated package description from shaoline.el  -*- no-byte-compile: t -*-
(define-package "shaoline" "1.0" "No description available." '((emacs "27.1")) :kind vc :commit "fe51de2db6584f5446eab3fd3359ff37bf258a21")

;; For NIXOS configuration: Make dependencies optional.
;; Install optional packages via nix-env, e.g., nix-env -iA nixpkgs.emacsPackages.all-the-icons
;; Shaoline will load them lazily if available, without polluting global space.
;; For flake integration, consider adding to your home-manager or system config:
;; {
;;   environment.systemPackages = [ pkgs.emacsPackages.shaoline ];
;;   # Add optional deps as needed.
;; }

;; Optional: shaoline-enable-dynamic-segments (customize to disable battery/time for minimal deps)
