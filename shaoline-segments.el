;;; shaoline-segments.el --- Shaoline's Segment Garden: practical, functional, literate -*- lexical-binding: t; -*-

;; Version: 2.2.2

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; ------------------------------------------------------------------------
;; Introduction (Garden Gate)
;;
;; Each segment is like a plant in a well-tended garden: a simple, pure function,
;; thriving on its own but best in harmony with others.
;;
;; This file is the Segment Garden. Add, remove, or rearrange segments as you need;
;; each one provides some useful piece of modeline information.
;;
;; Guidelines for tending the garden:
;;  - Each section includes commentary with practical advice.
;;  - Segments strive for pure, functional style—no unnecessary "weeds" (side effects, heavy dependencies).
;;  - Add your own "flowers" (segments), but keep the garden clean and maintainable.

;; ------------------------------------------------------------------------
;; Dependencies — the garden's soil
;;
;; All segments depend on the foundational macros and message engine.
(require 'shaoline-macros)
(require 'shaoline-msg-engine)
(eval-when-compile
  (defvar shaoline-enable-dynamic-segments t "Shadow variable for compile-time checks."))


;; -------------------------------------------------------------------------
;; "Just Water" — Buffer name only
;;
;; Sometimes you want simplicity: only the buffer’s name, no icon, no distraction.
;; -------------------------------------------------------------------------
;; Buffer name only (without icon).

(shaoline-define-segment shaoline-segment-buffer-name (buffer)
  "Only the buffer name, coloured as `shaoline-buffer-face'."
  (propertize (buffer-name buffer) 'face 'shaoline-buffer-face))

;; -------------------------------------------------------------------------
;; "Single Petal" — Icon for major-mode (no text)
;;
;; For minimalists: just the icon representing buffer mode, nothing else.
;; -------------------------------------------------------------------------
;; Major-mode icon only (without text).

(shaoline-define-simple-segment shaoline-segment-major-mode-icon
                                "Only the all-the-icons glyph that corresponds to `major-mode'."
                                (when (and shaoline-enable-dynamic-segments
                                           (featurep 'all-the-icons)
                                           major-mode)
                                  (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
                                  (all-the-icons-icon-for-mode major-mode :height 0.9)))

;; ----------------------------------------------------------------------------
;; "Neighboring Garden" — Project Name, if available (TTL-cached)
;;
;; Public segment is TTL-cached!
;; The uncached, original implementation is available as `shaoline--segment-project-name-raw`
;; for extension/testing.
;; ----------------------------------------------------------------------------
;;;###autoload
(shaoline-define-cached-segment shaoline-segment-project-name shaoline-project-name-ttl
  "Project name, if available. Cached for `shaoline-project-name-ttl' seconds (see \\[customize-group] RET shaoline-caching).
If you want the immediate, non-cached value (for tests/use-cases that demand up-to-date info),
call `shaoline--segment-project-name-raw` directly."
  (shaoline--segment-project-name-raw))

;; ----------------------------------------------------------------------------
;; "Frost on the Branch" — Current Git Branch
;;
;; Version control helps track history. If in a Git repo, show the branch name.
;; ----------------------------------------------------------------------------
;; Git branch.

(shaoline-define-simple-segment shaoline-segment-git-branch
                                "Current Git branch."
                                (when (and (featurep 'vc-git) (buffer-file-name))
                                  (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
                                    (when branch
                                      (concat
                                       (when (and shaoline-enable-dynamic-segments (featurep 'all-the-icons))
                                         (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
                                         (all-the-icons-octicon "git-branch" :v-adjust 0 :height 1.0 :face 'shaoline-git-face))
                                       " "
                                       (propertize branch 'face 'shaoline-git-face))))))

;; ----------------------------------------------------------------------------
;; "Echo in the Clearing" — Central message (echo-area)
;;
;; Shows the most recent user message, holding it in the center until a new one appears.
;; Useful as a persistent notification or feedback area.
;; ----------------------------------------------------------------------------
;; Echo-area message.

(shaoline-define-simple-segment shaoline-segment-echo-message
                                "Show the latest user `message` persistently (until a new non-empty), handling multi-line.
Truncates long or multi-line messages gracefully. Width managed by the modeline."
                                (let ((msg (shaoline-msg-current)))
                                  (if (and msg (not (string-empty-p msg)))
                                      (let* ((lines (split-string msg "\n"))
                                             (first (car lines))
                                             (rest (cdr lines))
                                             (raw-width (shaoline-available-center-width))
                                             (safe-width (or (and (numberp raw-width) raw-width) 80))
                                             (truncated (if rest
                                                            (concat (truncate-string-to-width first (max 1 (- safe-width 4)) nil nil "...")
                                                                    " [more]")
                                                          (truncate-string-to-width first (max 1 safe-width) nil nil "..."))))
                                        (propertize truncated 'face 'shaoline-echo-face))
                                    "")))

;; ----------------------------------------------------------------------------
;; "Firefly in the Leaves" — Battery Status (TTL-cached)
;;
;; Public segment is TTL-cached!
;; The uncached, original implementation is available as `shaoline--segment-battery-raw`
;; for extension/testing.
;; ----------------------------------------------------------------------------
;;;###autoload
(shaoline-define-cached-segment shaoline-segment-battery shaoline-battery-ttl
  "Show battery percentage and charging status. Cached for `shaoline-battery-ttl' seconds (see \\[customize-group] RET shaoline-caching).
If you want the immediate, non-cached value (for tests/use-cases that demand up-to-date info),
call `shaoline--segment-battery-raw` directly."
  (shaoline--segment-battery-raw))

;; ----------------------------------------------------------------------------
;; Uncached Raw Segment: Battery
;; ----------------------------------------------------------------------------
(shaoline-define-simple-segment shaoline--segment-battery-raw
                                "Show battery percentage and charging status. Returns 'N/A' if unavailable (RAW, uncached)."
                                (if (not shaoline-enable-dynamic-segments)
                                    ""  ;; Returns empty if dynamic segments are disabled.
                                  (unless (featurep 'battery) (require 'battery nil t))
                                  (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
                                  (let ((safe-n-a
                                         (propertize
                                          (if (featurep 'all-the-icons)
                                              (concat (all-the-icons-faicon "battery-empty" :face 'shaoline-battery-face :v-adjust 0 :height 0.75) " N/A")
                                            "N/A")
                                          'face '(:inherit shaoline-battery-face :slant italic))))
                                    (condition-case nil
                                        (if (and (fboundp 'battery) battery-status-function)
                                            (with-timeout (0.7 safe-n-a)
                                              (let ((data (funcall battery-status-function)))
                                                (cond
                                                 ((and (listp data) (cl-every #'consp data))
                                                  (let* ((percent (or (cdr (assoc 112 data))   ; ?p
                                                                      (cdr (assoc "percentage" data))
                                                                      (cdr (assoc "perc" data))
                                                                      (cdr (assoc "capacity" data))))
                                                         (status (or (cdr (assoc 66 data))     ; ?b
                                                                     (cdr (assoc "status" data))
                                                                     (cdr (assoc "charging" data))
                                                                     (cdr (assoc "state" data))))
                                                         (icon
                                                          (cond
                                                           ((not (featurep 'all-the-icons)) "")
                                                           ((and percent (string-match "\\([0-9]+\\)" percent))
                                                            (let* ((n (string-to-number (match-string 1 percent)))
                                                                   (icon-face
                                                                    (cond
                                                                     ((and status (string-match-p "\\`discharging\\'" status) (< n 25)) '(:foreground "#aa0000"))
                                                                     ((and status (string-match-p "\\`charging\\'" status)) '(:foreground "#007700"))
                                                                     (t 'shaoline-battery-face))))
                                                              (cond
                                                               ((>= n 90) (all-the-icons-faicon "battery-full" :face icon-face :v-adjust 0 :height 0.75))
                                                               ((>= n 70) (all-the-icons-faicon "battery-three-quarters" :face icon-face :v-adjust 0 :height 0.75))
                                                               ((>= n 40) (all-the-icons-faicon "battery-half" :face icon-face :v-adjust 0 :height 0.75))
                                                               ((>= n 10) (all-the-icons-faicon "battery-quarter" :face icon-face :v-adjust 0 :height 0.75))
                                                               (t          (all-the-icons-faicon "battery-empty" :face icon-face :v-adjust 0 :height 0.75)))))
                                                           ((and status (string-match-p "full" status)) (all-the-icons-faicon "battery-full" :face 'shaoline-battery-face :v-adjust 0 :height 0.75))
                                                           ((and status (string-match-p "\\<ac\\>" status)) (all-the-icons-octicon "plug" :face 'shaoline-battery-face))
                                                           ((and status (string-match-p "charging" status)) (all-the-icons-faicon "bolt" :face '(:foreground "#007700") :v-adjust 0))
                                                           ((and status (string-match-p "discharging" status)) (all-the-icons-faicon "battery-empty" :face '(:foreground "#aa0000") :v-adjust 0 :height 0.75))
                                                           (t ""))))
                                                    (if percent
                                                        (concat
                                                         (if (and (stringp icon) (not (string-empty-p icon))) (concat icon " "))
                                                         (propertize (concat (replace-regexp-in-string "%" "" percent) "%")
                                                                     'face 'shaoline-battery-face))
                                                      (propertize
                                                       (if (featurep 'all-the-icons)
                                                           (concat (all-the-icons-faicon "battery-empty" :face 'shaoline-battery-face :v-adjust 0 :height 0.75) " No battery")
                                                         "No battery")
                                                       'face '(:inherit shaoline-battery-face :slant italic)))))
                                                 ((and (stringp data) (not (string-empty-p data)))
                                                  (propertize data 'face 'shaoline-battery-face))
                                                 (t safe-n-a))))
                                          safe-n-a)
                                      (error safe-n-a)))))

;; ----------------------------------------------------------------------------
;; "Sign at the Garden Gate" — Current major-mode (with optional icon)
;;
;; Shows the editing mode for the current buffer—helpful context at a glance.
;; ----------------------------------------------------------------------------
;; Major mode.

(shaoline-define-simple-segment shaoline-segment-major-mode
                                "Major mode segment, optionally with icon."
                                (let ((icon
                                       (when (and shaoline-enable-dynamic-segments (featurep 'all-the-icons) major-mode)
                                         (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
                                         (all-the-icons-icon-for-mode major-mode :height 0.9))))
                                  (concat
                                   (when (and icon (stringp icon))
                                     (concat icon " "))
                                   (propertize (format-mode-line mode-name)
                                               'face 'shaoline-mode-face))))

;; ----------------------------------------------------------------------------
;; "Heaven Above the Garden" — Time and Moon Phase
;;
;; Adds a digital clock and the current moon phase, if you want a cosmic touch
;; on your modeline. These segments require dynamic updates and extra packages.
;;
;; --------------------------------------------------------------------------
;; Accurate moon-phase helper (uses calendar.el + lunar.el).
;; Caches result for the current calendar date, so heavy astronomy
;; runs at most once per day.

(require 'calendar nil t)
(require 'lunar   nil t)                   ; lazy ‒ only when segment used

(defconst shaoline--synodic-month 29.530588853
  "Mean length of a synodic month, in (solar-earth) days.")

(defvar shaoline--moon-cache '(nil . 0)
  "Cons cell (\"YYYY-MM-DD\" . IDX) – cached moon phase for today.")

(defun shaoline--abs-now ()
  "Return the current absolute day number as a *float* (Emacs astronomical)."
  (let* ((tm   (decode-time (current-time)))
         (sec  (nth 0 tm))
         (min  (nth 1 tm))
         (hour (nth 2 tm)))
    (+ (calendar-absolute-from-gregorian (calendar-current-date))
       (/ (+ sec (* 60 min) (* 3600 hour)) 86400.0))))
;; ------------------------------------------------------------------
;; Moon-phase helpers (top-level after the definition above)

(defun shaoline--moon-phase-idx (&optional _ignored)
  "Return accurate moon-phase index 0‥7 for *today*.
Falls back to 0 on any error; result is cached for the day."
  (let ((today (format-time-string "%F")))
    (if (equal today (car shaoline--moon-cache))
        (cdr shaoline--moon-cache)
      (let ((idx
             (ignore-errors
               (let* ((abs-now  (shaoline--abs-now))
                      (next-new (lunar-new-moon-on-or-after abs-now))
                      (prev-new (- next-new shaoline--synodic-month))
                      (age      (- abs-now prev-new)))
                 (mod (floor (* age (/ 8.0 shaoline--synodic-month))) 8)))))
        (setq shaoline--moon-cache (cons today (or idx 0)))
        (cdr shaoline--moon-cache)))))



(shaoline-define-simple-segment shaoline-segment-digital-clock
                                "Show current digital clock, e.g. ' 21:43 '."
                                (if (not shaoline-enable-dynamic-segments)
                                    ""
                                  (propertize (format-time-string " %H:%M ") 'face 'shaoline-time-face)))
;; ------------------------------------------------------------------
;; Compatibility alias (deprecated, kept for tests & configs that
;; still reference `shaoline-segment-time').
(shaoline-define-simple-segment shaoline-segment-time
                                "Deprecated alias – use `shaoline-segment-digital-clock'."
                                (shaoline-segment-digital-clock))

(shaoline-define-simple-segment shaoline-segment-moon-phase
                                "Show current moon phase as an icon.
In GUI frames use the Unicode glyphs 🌑…🌘.
In TTY (or when glyphs are unavailable) fall back to simple ASCII:
N c Q g F G q C  (new, crescent, 1-st quarter, gibbous, full …)."
                                (if (not shaoline-enable-dynamic-segments)
                                    ""
                                  (let* ((idx (shaoline--moon-phase-idx))
                                         (icons (if (display-graphic-p)
                                                    ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"] ; GUI / Unicode
                                                  ["N"  "c"  "Q"  "g"  "F"  "G"  "q"  "C"])) ; TTY / ASCII
                                         (moon (aref icons idx)))
                                    (propertize moon 'face 'shaoline-moon-face))))

;; ----------------------------------------------------------------------------
;; "Raindrop on a Leaf" — Buffer Modified Indicator
;;
;; Shows a "*" if the buffer has unsaved changes—simple, universal.
;; ----------------------------------------------------------------------------
;; Modified status.

(shaoline-define-simple-segment shaoline-segment-modified
                                "Show '*' if buffer is modified."
                                (when (and (buffer-modified-p)
                                           (buffer-file-name))
                                  (propertize "*" 'face 'shaoline-modified-face)))

;; ----------------------------------------------------------------------------
;; "Island of Calm" — Visual Spacer
;;
;; A blank segment, used to pad the modeline or separate components.
;; ----------------------------------------------------------------------------
;; Visual spacer.

(shaoline-define-simple-segment shaoline-segment-emptiness
                                "A blank segment."
                                " ")

(shaoline-define-simple-segment shaoline-segment-position
                                "Show current line and column position."
                                (propertize (format "%d:%d" (line-number-at-pos) (current-column)) 'face 'shaoline-mode-face))

;; ----------------------------------------------------------------------------
;; Each stone in its place, each segment a reflection on the pond.
;; Extend the modeline garden with your own well-crafted segments.

;; ----------------------------------------------------------------------------
;; Encoding and EOL.

(shaoline-define-simple-segment shaoline-segment-encoding
                                "Show file encoding and EOL type."
                                (let* ((coding (symbol-name (or buffer-file-coding-system 'undecided)))
                                       (coding (if (string-match-p "utf-8" coding) "UTF8" coding))
                                       (eol
                                        (pcase (coding-system-eol-type (or buffer-file-coding-system 'undecided))
                                          (0 "LF")
                                          (1 "CRLF")
                                          (2 "CR")
                                          (_ ""))))
                                  (when buffer-file-name
                                    (propertize (format "%s %s" coding eol) 'face 'shaoline-mode-face))))

;; ----------------------------------------------------------------------------
;; Minor Modes Summary (Compact)
;;
;; Displays icons for important or special minor-modes like read-only, flycheck, etc.
;; Customize `shaoline-minor-modes-icon-map` for your needs.
(defface shaoline-minor-modes-face
  '((t :inherit (shaoline-base-face) :height 0.7 :foreground "gray60"))
  "Face for the minor modes segment in Shaoline (smaller)."
  :group 'shaoline)

(defcustom shaoline-minor-modes-icon-map
  '(("read-only-mode"            . "🛡️")     ;; shield for read-only
    ("overwrite-mode"            . "✏️")   ;; pencil barred
    ("auto-save-mode"            . "⏳")   ;; memo with hourglass
    ;; -- New additions start here --
    ("aggressive-indent-mode"    . "⚡")     ;; auto-indentation
    ("abbrev-mode"               . "✂️")     ;; abbreviation
    ("company-mode"              . "🤝")     ;; auto-completion
    ("yas-minor-mode"            . "🧩")     ;; snippets
    ("evil-mode"                 . "😈")     ;; vim emulation
    ("smartparens-mode"          . "()")     ;; smart parenthesis
    ("show-paren-mode"           . "🔲")     ;; highlight parens
    ("rainbow-mode"              . "🌈")     ;; color highlighting
    ("undo-tree-mode"            . "🌲")     ;; undo-tree
    ("which-key-mode"            . "❓")     ;; key hints
    ("electric-pair-mode"        . "⛓️")    ;; electric pairs
    ("hl-line-mode"              . "📏")     ;; highlight line
    ("display-line-numbers-mode" . "#️⃣")    ;; line numbers
    ("org-indent-mode"           . "⮞")     ;; org indentation
    ;; -- New additions end here --
    ("visual-line-mode"          . "↩️")     ;; return arrow
    ("ws-butler-mode"            . "␣")    ;; visible space + NO
    ("indent-tabs-mode"          . "⇄⇆")     ;; tab exchange
    ("editorconfig-mode"         . "📐")     ;; set square
    ("god-mode"                  . "🦶")     ;; foot/step to denote 'god walking'
    ("god-local-mode"            . "👣")     ;; footprints for local god-mode
    ("projectile-mode"           . "🌌")     ;; milky way for 'project/universe'
    ("envrc-mode"                . "🔰")     ;; green badge for env
    ("flyspell-mode"             . "🪶")     ;; feather/quill
    ("spell-fu-mode"             . "🪄")     ;; magic wand = 'fu'
    ("lsp-mode"                  . "🧠")     ;; brain for smart editor
    ("eglot-managed-mode"        . "🎛️")     ;; control panel
    ("flycheck-mode"             . "🩺")     ;; stethoscope (diagnostics)
    ("flymake-mode"              . "🔬")     ;; microscope
    ("gptel-mode"                . "🤖💬")     ;; speech balloon for AI chat
    ("gptel-aibo-mode"           . "🐶")   ;; robot and dog
    ("org-drill-mode"            . "🔔")     ;; bell for reminders
    ("olivetti-mode"             . "📃")     ;; page for writing
    ("org-fancy-priorities-mode" . "🚦")     ;; traffic light for priorities
    ("org-auto-tangle-mode"      . "🧵"))    ;; thread for tangle
  "Mapping minor mode variable names (as strings) to an icon/emoji for Shaoline.
Customize this to control which minor modes are shown and what icons are used."
  :type '(alist :key-type string :value-type string)
  :group 'shaoline)

(defvar shaoline--minor-mode-face-cache (make-hash-table :test 'equal)
  "Cache for shaoline per-minor-mode faces.")

(defun shaoline--minor-mode-get-face (name)
  "Return interned face symbol for NAME, creating if absent. Color is deterministic by name."
  (or (gethash name shaoline--minor-mode-face-cache)
      (let* ((facesym (intern (concat "shaoline-minor-m-face-" name)))
             (color (let* ((hue (/ (mod (sxhash name) 360.0) 360.0))
                           (sat 0.7)
                           (lum 0.55))
                      (apply #'color-rgb-to-hex (color-hsl-to-rgb hue sat lum))))
             (doc (format "Face for %s icon in Shaoline minor modes." name)))
        (unless (facep facesym)
          (eval `(defface ,facesym '((t :inherit shaoline-minor-modes-face :foreground ,color :weight bold)) ,doc :group 'shaoline)))
        (puthash name facesym shaoline--minor-mode-face-cache)
        facesym)))

(defvar shaoline--minor-modes-cache-str ""
  "Cached string for minor-modes segment.")
(defvar shaoline--minor-modes-cache-ts 0
  "Last cache time (float-time) for shaoline--minor-modes-cache-str.")
(defcustom shaoline-minor-modes-cache-ttl 0.5
  "How many seconds to cache shaoline-segment-minor-modes result."
  :type 'number
  :group 'shaoline)

(shaoline-define-simple-segment shaoline-segment-minor-modes
                                "Show ONLY critically important minor modes, icons as Unicode emoji/symbols (with deterministic color, c 0.5s)."
                                (let ((now (float-time)))
                                  (if (and shaoline--minor-modes-cache-str
                                           (< (- now shaoline--minor-modes-cache-ts) shaoline-minor-modes-cache-ttl))
                                      shaoline--minor-modes-cache-str
                                    (let* ((icon-map shaoline-minor-modes-icon-map)
                                           (seen (make-hash-table :test 'equal))
                                           (modes
                                            (delq nil
                                                  (mapcar
                                                   (lambda (sym)
                                                     (let* ((name (symbol-name sym))
                                                            (icon (cdr (assoc name icon-map))))
                                                       (when (and icon
                                                                  (boundp sym)
                                                                  (symbol-value sym)
                                                                  (not (gethash name seen)))
                                                         (puthash name t seen)
                                                         (propertize icon 'face (shaoline--minor-mode-get-face name)))))
                                                   minor-mode-list))))
                                      (let ((str (when modes
                                                   (propertize (concat "(" (mapconcat #'identity modes "") ")")
                                                               'face 'shaoline-minor-modes-face))))
                                        (setq shaoline--minor-modes-cache-str str
                                              shaoline--minor-modes-cache-ts now)
                                        str)))))


;; ----------------------------------------------------------------------------
;; Flycheck/Flymake Status (Errors/Warnings)
;;
;; Displays current linting/compiler errors and warnings count for feedback.

(shaoline-define-simple-segment shaoline-segment-flycheck
                                "Show Flycheck/Flymake error and warning counts if available."
                                (cond
                                 ((and (bound-and-true-p flycheck-mode)
                                       (fboundp 'flycheck-count-errors)
                                       flycheck-current-errors)
                                  (let* ((counts (flycheck-count-errors flycheck-current-errors))
                                         (err (or (cdr (assq 'error counts)) 0))
                                         (warn (or (cdr (assq 'warning counts)) 0)))
                                    (propertize (format "E:%d W:%d" err warn) 'face 'shaoline-modified-face)))
                                 ((and (bound-and-true-p flymake-mode)
                                       (fboundp 'flymake-diagnostics))
                                  (let* ((all (flymake-diagnostics))
                                         (err (cl-count-if (lambda (d) (eq (flymake-diagnostic-type d) :error)) all))
                                         (warn (cl-count-if (lambda (d) (eq (flymake-diagnostic-type d) :warning)) all)))
                                    (propertize (format "E:%d W:%d" err warn) 'face 'shaoline-modified-face)))
                                 (t "")))

;; ----------------------------------------------------------------------------
;; VCS State Extension (Git indicator)
;;
;; Quickly shows whether the buffer is edited, added, removed, etc. in git.

(shaoline-define-simple-segment shaoline-segment-vcs-state
                                "Show Git status short indicator (+ for unstaged, ! for unstaged+staged, nothing for clean)."
                                (when (and (featurep 'vc-git) (buffer-file-name))
                                  (let* ((state (vc-state (buffer-file-name))))
                                    (pcase state
                                      (`edited   (propertize "+" 'face 'shaoline-git-face))
                                      (`added    (propertize "+" 'face 'shaoline-git-face))
                                      (`removed  (propertize "!" 'face 'shaoline-git-face))
                                      (`conflict (propertize "✗" 'face 'error))
                                      (`missing  (propertize "?" 'face 'warning))
                                      (`up-to-date "")
                                      (_ "")))))

;; ----------------------------------------------------------------------------
;; Input Method (Layout/Language Indicator)
;;
;; Indicates active input method, such as "EN" for English or a code for the current layout.
(shaoline-define-simple-segment shaoline-segment-input-method
                                "Show current input method title (layout) if any, else 'EN'."
                                (let ((indicator
                                       (cond
                                        (current-input-method
                                         (or current-input-method-title current-input-method))
                                        (t "EN"))))
                                  (propertize indicator 'face 'shaoline-mode-face)))

;; ---------------------------------------------------------------------------
;; Cached wrappers for heavy / I/O-intensive segments (with customizable TTL)
;; ---------------------------------------------------------------------------

(defgroup shaoline-caching nil
  "Customization group for TTL-caching of heavy shaoline segments."
  :group 'shaoline)

(defcustom shaoline-project-name-ttl 2
  "How many seconds to cache the project name segment. Lower for more frequent updates, higher for less disk I/O."
  :type 'number
  :group 'shaoline-caching)

(defcustom shaoline-battery-ttl 5
  "How many seconds to cache the battery segment. Increase if your system battery state changes slowly."
  :type 'number
  :group 'shaoline-caching)

;; The original, uncached implementations are still valuable for other code (or tests).
;; Define shaoline--segment-project-name-raw as the true project-name getter (uncached).
(defun shaoline--segment-project-name-raw ()
  "Return the current project name using project.el or Projectile if available. Returns nil if not found."
  (cond
   ((fboundp 'project-current)
    (let ((proj (project-current)))
      (when proj
        (if (fboundp 'project-name)
            (project-name proj)
          ;; Fallback (Emacs ≤ 28 doesn't have project-name)
          (file-name-nondirectory
           (directory-file-name (car (project-roots proj))))))))
   ((and (featurep 'projectile) (fboundp 'projectile-project-name))
    (projectile-project-name))
   (t nil)))

(unless (fboundp 'shaoline--segment-battery-raw)
  (defalias 'shaoline--segment-battery-raw #'shaoline-segment-battery))

;; ------------------------------------------------------------------
;;;###autoload
(shaoline-define-cached-segment shaoline-segment-project-name shaoline-project-name-ttl
  "Project name, if available. Cached for `shaoline-project-name-ttl' seconds (see \\[customize-group] RET shaoline-caching)."
  (let ((name (shaoline--segment-project-name-raw)))
    (when name
      (propertize name 'face 'shaoline-project-face))))

;; ------------------------------------------------------------------
;;;###autoload
(shaoline-define-cached-segment shaoline-segment-battery shaoline-battery-ttl
  "Show battery percentage and charging status. Cached for `shaoline-battery-ttl' seconds (see \\[customize-group] RET shaoline-caching)."
  (shaoline--segment-battery-raw))


(provide 'shaoline-segments)
;;; shaoline-segments.el ends here
