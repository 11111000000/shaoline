;;; shaoline.el --- Functional minimalist echo-area modeline -*- lexical-binding: t; -*-

;; Version: 2.1.1

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;;; Commentary:

;; In the land of the rising buffer, modelines wandered the world in excess—
;; until Shaoline arrived, shaved its segments, and sat quietly in the echo area.
;; The true path has neither timers nor distractions.
;;
;; Shaoline-echo is a trimmed-down edition of the original: a Shaolin monk
;; among modelines. There is no child-frame and no in-window modeline —
;; the only rendering target is the echo area (the minibuffer). The architecture
;; remains functional: each segment is a pure function, unaffected by worldly
;; state, and the only side effects are writing to the echo area and (optionally)
;; hiding the regular `mode-line` (so your modeline too may attain Nirvana).
;;
;; Key features (as written upon a bamboo leaf):
;;   • Minimalist, declarative segment configuration. Add, remove, or swap
;;     segments like stones in the Zen garden.
;;   • No polling timers — the modeline is refreshed only by the wind
;;     (or `post-command-hook`, etc.).
;;   • Global minor mode `shaoline-mode` toggles everything with serenity.
;;   • Optional hiding of the traditional mode line, for those seeking emptiness.
;;   • A unit-test-friendly “core” — the composing function is a silent
;;     observer, causing no side effects (Wu Wei by design).
;;
;; Quick start:
;;
;;   (require 'shaoline-echo)
;;   (shaoline-mode 1)
;;
;; See the README on GitHub for scrolls and stories about writing custom segments.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'rx))
;; Core requires: macros and msg-engine first for definitions.
(require 'shaoline-macros)
(require 'shaoline-msg-engine)

;; project-specific libraries are only needed by segments (see shaoline-segments.el);
;; keeping the core truly “pure”.


;; ----------------------------------------------------------------------------
;; Customization Group and Variables
;;
;; All is possible. Customize, or leave as it is.

(defgroup shaoline nil
  "Functional minimalist echo-area modeline."
  :group 'convenience
  :prefix "shaoline-")

(defcustom shaoline-icon-width 2
  "Width in characters to pad or truncate all-the-icons icons for consistent spacing.
Increase if your favorite icons are wider."
  :type 'integer
  :group 'shaoline)


(defcustom shaoline-debug nil
  "If non-nil, log shaoline activity into `shaoline--log-buffer'. Seek inner debugness."
  :type 'boolean
  :group 'shaoline)

(defcustom shaoline-enable-dynamic-segments t
  "If non-nil, enable dynamic segments like time and battery.
When disabled, these segments return empty strings and do not load any dependencies.
This purifies Shaoline for minimal setups, aligning with Wu Wei."
  :type 'boolean
  :group 'shaoline)

(defvar shaoline--log-buffer "*shaoline-logs*"
  "Debug buffer for logs. Quiet scroll, if you wish to listen.")

(defun shaoline--log (fmt &rest args)
  "Write formatted FMT/ARGS message to `shaoline--log-buffer' if `shaoline-debug' is non-nil.
Zen masters say: A log unread is a tree falling in a silent forest."
  (when shaoline-debug
    (let ((msg (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create shaoline--log-buffer)
        (goto-char (point-max))
        (insert msg "\n")))))

(defcustom shaoline-available-segments
  '((shaoline-segment-icon-and-buffer . "Icon and buffer name")
    (shaoline-segment-buffer-name     . "Buffer name only")
    (shaoline-segment-major-mode-icon . "Major-mode icon only")
    (shaoline-segment-project-name    . "Project name (if any)")
    (shaoline-segment-git-branch      . "Git branch")
    (shaoline-segment-battery         . "Battery state")
    (shaoline-segment-digital-clock   . "Digital clock (only time, e.g. 21:43)")
    (shaoline-segment-moon-phase      . "Moon phase icon")
    (shaoline-segment-echo-message    . "Recent noticed message")
    (shaoline-segment-position        . "Line and column position")
    (shaoline-segment-modified        . "Buffer modified status")
    (shaoline-segment-major-mode      . "Major mode with optional icon")
    (shaoline-segment-encoding        . "File encoding and EOL type")
    (shaoline-segment-minor-modes     . "Compact enabled minor modes")
    (shaoline-segment-flycheck        . "Flycheck/Flymake errors/warnings")
    (shaoline-segment-vcs-state       . "Git/VCS state indicator"))
  "Plenum of segments that may be included in shaoline. For drag/drop in customize."
  :type '(alist :key-type symbol :value-type string)
  :group 'shaoline)

(defcustom shaoline-segments
  '((:left shaoline-segment-position
           shaoline-segment-major-mode-icon
           shaoline-segment-minor-modes
           shaoline-segment-buffer-name
           shaoline-segment-modified)

    (:center shaoline-segment-echo-message)

    (:right
     shaoline-segment-project-name
     shaoline-segment-git-branch
     shaoline-segment-battery
     shaoline-segment-input-method
     shaoline-segment-digital-clock
     shaoline-segment-moon-phase))

  "Alist describing segments for :left, :center and :right.
Each entry is a list of segment function symbols for that side.
May be configured in Custom (see shaoline-available-segments)."
  :type '(alist
          :key-type (choice (const :left) (const :center) (const :right))
          :value-type (set (choice
                            (const shaoline-segment-icon-and-buffer)
                            (const shaoline-segment-project-name)
                            (const shaoline-segment-git-branch)
                            (const shaoline-segment-battery)
                            (const shaoline-segment-digital-clock)
                            (const shaoline-segment-moon-phase)
                            (const shaoline-segment-echo-message)
                            (const shaoline-segment-modified)
                            (const shaoline-segment-major-mode)
                            (const shaoline-segment-encoding)
                            (const shaoline-segment-minor-modes)
                            (const shaoline-segment-flycheck)
                            (const shaoline-segment-vcs-state)
                            (const shaoline-segment-position))))
  :group 'shaoline)

(defcustom shaoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "Hooks that tell Shaoline to recompute and display the modeline."
  :type '(repeat symbol)
  :group 'shaoline)

(defcustom shaoline-autohide-modeline t
  "When non-nil, the traditional mode-line is hidden while `shaoline-mode' is active.

You can exclude certain major-modes from hiding using `shaoline-exclude-modes`."
  :type 'boolean
  :group 'shaoline)

(defcustom shaoline-exclude-modes
  '(magit-status-mode org-agenda-mode gnus-summary-mode compilation-mode dired-mode)
  "List of major modes in which classic mode-line should *not* be hidden by shaoline-mode."
  :type '(repeat symbol)
  :group 'shaoline)

(defcustom shaoline-right-padding 0
  "Extra spaces appended to the right edge of the shaoline. Sometimes, a little emptiness is all you need."
  :type 'integer
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; Faces
;;
;; Outward form: all faces dwell here.

(defface shaoline-base-face
  '((t :inherit default
       :height 1.0
       :box nil
       :underline nil
       :overline nil
       :inverse-video nil
       :extend t))
  "Base face for shaoline. The foundation for all modeline elements."
  :group 'shaoline)

(defface shaoline-echo-face
  '((t :inherit (font-lock-string-face shaoline-base-face) :height 1.0))
  "Face for the echo message segment (slightly smaller)."
  :group 'shaoline)

(defface shaoline-time-face
  `((t :inherit default  ;; Inherit from default to adapt to themes
       :height 1.0
       :bold nil
       :family "Digital Display"
       :foreground ,(or (face-foreground 'success nil t) "#00aa00")  ;; Foreground, computed at load time
       :background ,(or (face-background 'shadow nil t) "#002200")))  ;; Background, computed at load time
  "Face for the time segment, adapting to the current theme at load time.
For full dynamic adaptation, reload after theme changes."
  :group 'shaoline)

(defface shaoline-moon-face
  `((t :inherit default
       :height 1.0
       :bold nil
       :foreground ,(or (face-foreground 'warning nil t) "yellow")))  ;; Foreground, computed at load time
  "Face for the moon phase, adapting to the theme at load time.
For full dynamic adaptation, reload after theme changes."
  :group 'shaoline)

(defface shaoline-battery-face
  '((t :inherit (shaoline-base-face)))
  "Face for battery level segment."
  :group 'shaoline)

(defface shaoline-buffer-face
  '((t :inherit (shaoline-base-face) :height 1.0))
  "Face for the buffer name segment."
  :group 'shaoline)

(defface shaoline-project-face
  '((t :inherit (font-lock-keyword-face shaoline-base-face) :height 1.0))
  "Face for the project name segment."
  :group 'shaoline)

(defface shaoline-modified-face
  '((t :inherit (warning shaoline-base-face) :height 1.0))
  "Face for the modified indicator segment."
  :group 'shaoline)

(defface shaoline-git-face
  '((t :inherit (font-lock-type-face shaoline-base-face) :height 1.0))
  "Face for the git branch segment."
  :group 'shaoline)

(defface shaoline-mode-face
  '((t :inherit (font-lock-type-face shaoline-base-face) :height 1.0))
  "Face for the major mode segment."
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; Internal State
;;
;; All inner workings, global state, and persistence dwell here.

(defvar shaoline--segment-table (make-hash-table :test 'eq)
  "Hash-table storing registered segment functions.")

(defvar shaoline--last-str ""
  "Last rendered string, used to avoid unnecessary redisplay. Repetition is not enlightenment.")

;; Message state handled separately (already required above).

;; Load segments after variables are defined.
(require 'shaoline-segments)

(defvar shaoline--default-mode-line-format-backup nil
  "Backup of `default-mode-line-format' when the modeline is hidden (restored when Shaoline is disabled).")

(defvar shaoline--resize-mini-windows-backup nil
  "Backup of `resize-mini-windows' when `shaoline-mode` toggles. Settings are always restored on exit.")

(defvar-local shaoline--saved-mode-line-format nil
  "Buffer-local backup of `mode-line-format` for use while `shaoline-mode` is hiding the classic modeline.
Restored exactly as it was when the mode is toggled off.")

;; ----------------------------------------------------------------------------
;; Infrastructure (UI, advice, minor mode) — lazy-loaded via minor mode.
;; Users require 'shaoline, then toggle shaoline-mode to load infra.

;; React to new color theme: recalculate faces.
(defun shaoline-update-faces (&rest _)
  "Update Shaoline faces to match current theme."
  (custom-set-faces
   `(shaoline-time-face
     ((t :height 1.0
         :bold nil
         :family "Digital Display"
         :foreground "#00aa00"
         :background ,(or (face-background 'shadow nil t) "#002200"))))
   `(shaoline-moon-face
     ((t :inherit default
         :height 1.0
         :bold nil
         :foreground ,(or (face-foreground 'warning nil t) "yellow"))))))

;; Enable harmony after theme changes.
(add-hook 'enable-theme-functions #'shaoline-update-faces)
(add-hook 'disable-theme-functions #'shaoline-update-faces)

;; ----------------------------------------------------------------------------
;; Segment Definition Macros: в shaoline-macros.el

(require 'shaoline-macros)

;; ----------------------------------------------------------------------------
;; Segment Helper Functions
;;
;; Helpers: arranging segments, unseen.

(defun shaoline--apply-segment (fn buffer)
  "Call segment FN with BUFFER (or with no args if not needed) and return its string.
Errors or missing segment yield an empty string (quiet fail), diagnostics go to log if `shaoline-debug`."
  (if (fboundp fn)
      (condition-case err
          (let* ((arity (help-function-arglist fn t))
                 (res (if arity
                          (funcall fn buffer)
                        (funcall fn))))
            (if (stringp res) res ""))
        (error
         (let ((msg (format "[SEGMENT ERROR: %s in %s]" err fn)))
           (when shaoline-debug
             (shaoline--log "%s" msg)
             (shaoline--log "Traceback: %s" (with-output-to-string (backtrace))))
           "")))
    (when shaoline-debug
      (shaoline--log "[VOID SEGMENT: %s (not defined)]" fn))
    ""))

(defun shaoline--collect-segments (side buffer)
  "Render all segments for SIDE (:left, :center, :right) within BUFFER.
Only non-empty and valid string results are included."
  (cl-loop for fn in (cdr (assq side shaoline-segments))
           for str = (shaoline--apply-segment fn buffer)
           when (and (stringp str)
                     (not (string-empty-p str)))
           collect str))

;; ----------------------------------------------------------------------------
;; Core: Center Width Calculation (Pure)
;;
;; As much as needed, nothing more.

(defun shaoline-available-center-width (&optional buffer window width)
  "Return the maximum width available for the center segment (excluding right-padding).
Calculates based on the length of left and right segments, including any necessary spacing."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         (raw-width
          (let ((w (or width
                       (let* ((mini (minibuffer-window))
                              (mini-width (and (window-live-p mini)
                                               (window-width mini))))
                         (or mini-width (frame-width))))))
            (or (and (numberp w) w) 80)))
         (left   (mapconcat #'identity (shaoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (shaoline--collect-segments :right buffer) " "))
         (left-w (string-width left))
         (right-w (string-width right))
         (space-left  (if (and (not (string-empty-p left))
                               (not (string-empty-p right)))
                          1 0))
         (space-center-left (if (and (not (string-empty-p left)))
                                1 0))
         (space-center-right (if (not (string-empty-p right))
                                 1 0)))
    (max 0 (- raw-width
              left-w
              right-w
              (if (not (string-empty-p left)) 1 0)
              (if (not (string-empty-p right)) 1 0)
              shaoline-right-padding))))

;; ----------------------------------------------------------------------------
;; Core: Compose Modeline (Pure, No Side Effects)
;;
;; Pure computation. No action, no world. Suitable everywhere.
(defun shaoline-compose-modeline (&optional buffer window width)
  "Return the Shaoline string for the echo area.
The right segment is *always* visually pressed to the right edge (with right-padding),
no matter how many icons/emoji it contains.
If its contents get shorter, the gap appears *to the left* of the rightmost character, not to the right edge."
  (let* ((buffer (or buffer (current-buffer)))
         (window (or window (selected-window)))
         (echo-width
          (let ((w (or width
                       (let* ((mini (minibuffer-window))
                              (mini-w (and (window-live-p mini)
                                           (window-width mini))))
                         (or mini-w (frame-width))))))
            (or w 80)))
         (left   (mapconcat #'identity (shaoline--collect-segments :left buffer) " "))
         (center0 (mapconcat #'identity (shaoline--collect-segments :center buffer) " "))
         (right  (mapconcat #'identity (shaoline--collect-segments :right buffer) " "))
         (gap-left (if (string-empty-p left) "" " "))
         (right-width (string-width right))
         ;; Куда прижимаем правый сегмент:
         (right-edge (- echo-width shaoline-right-padding))
         ;; Левая граница right: столько, чтобы правый край был чётко на границе.
         (align-to (- right-edge right-width))
         ;; сколько осталось для центра?
         (avail (max 0 (- align-to
                          (string-width left)
                          (length gap-left))))
         (center (if (and (stringp center0) (> (string-width center0) avail))
                     (truncate-string-to-width center0 avail 0 ?\s "...")
                   center0)))
    (concat
     left
     gap-left
     center
     ;; Подальше вперёд к границе ровно на align-to
     (propertize " " 'display `(space :align-to ,align-to))
     right
     (make-string shaoline-right-padding ?\s))))

;; ----------------------------------------------------------------------------
;; Segments are elsewhere.

;; User-defined segments: stones in your garden
(let ((user-file
       (or
        (expand-file-name "shaoline-user-segments.el" user-emacs-directory)
        (and load-file-name
             (expand-file-name "shaoline-user-segments.el" (file-name-directory load-file-name))))))
  (when (and user-file (file-exists-p user-file))
    (condition-case err
        (load user-file nil t)
      (error (shaoline--log "Could not load user segments: %s" err)))))

;; ----------------------------------------------------------------------------
;; Load infrastructure lazily when mode is enabled (avoids circular deps).
;; This is a stub; actual minor mode is in shaoline-infra.el and autoloaded.

(provide 'shaoline)
;;; shaoline.el ends here
