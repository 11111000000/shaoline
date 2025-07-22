;;; shaoline-segments.el --- Standard segments for Shaoline modeline -*- lexical-binding: t; -*-

;; Version: 2.1.1

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; Segments require macros and msg-engine (core vars from shaoline.el are assumed loaded).
(require 'shaoline-macros)
(require 'shaoline-msg-engine)
(eval-when-compile
  (defvar shaoline-enable-dynamic-segments t "Shadow variable for compile-time checks."))

;; ----------------------------------------------------------------------------
;; Buffer icon and name.

(shaoline-define-segment shaoline-segment-icon-and-buffer (buffer)
  "Colored buffer icon (by major mode, as in tabs) and buffer name."
  (let* ((icon
          (when (and shaoline-enable-dynamic-segments (featurep 'all-the-icons))
            (unless (featurep 'all-the-icons) (require 'all-the-icons nil t))
            (let* ((mode (buffer-local-value 'major-mode buffer))
                   (raw (all-the-icons-icon-for-mode mode :height 0.9)))
              (cond
               ((stringp raw) raw)
               ((buffer-file-name buffer)
                (all-the-icons-icon-for-file
                 (buffer-file-name buffer) :height 0.9))
               (t (all-the-icons-faicon "file-o" :height 0.9))))))
         (name (buffer-name buffer))
         (text (if icon (concat icon " " name) name)))
    (add-face-text-property
     (if icon (length icon) 0) (length text)
     'shaoline-buffer-face 'append
     text)
    text))

;; -------------------------------------------------------------------------
;; Buffer name only (without icon).

(shaoline-define-segment shaoline-segment-buffer-name (buffer)
  "Only the buffer name, coloured as `shaoline-buffer-face'."
  (propertize (buffer-name buffer) 'face 'shaoline-buffer-face))

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
;; Project name.

(shaoline-define-simple-segment shaoline-segment-project-name
                                "Project name, if available."
                                (unless (featurep 'projectile) (require 'projectile nil t))
                                (let* ((project
                                        (cond
                                         ((and (featurep 'projectile) (projectile-project-name))
                                          (projectile-project-name))
                                         ((fboundp 'project-current)
                                          (when-let ((pr (project-current)))
                                            (file-name-nondirectory (directory-file-name (car (project-roots pr))))))
                                         (t nil))))
                                  (when (and project (not (string= "-" project)))
                                    (propertize project 'face 'shaoline-project-face))))

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
;; Battery.

(shaoline-define-simple-segment
 shaoline-segment-battery
 "Show battery percentage and charging status. Returns 'N/A' if unavailable."
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
;; Time and moon phase.

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
;; Modified status.

(shaoline-define-simple-segment shaoline-segment-modified
                                "Show '*' if buffer is modified."
                                (when (and (buffer-modified-p)
                                           (buffer-file-name))
                                  (propertize "*" 'face 'shaoline-modified-face)))

;; ----------------------------------------------------------------------------
;; Visual spacer.

(shaoline-define-simple-segment shaoline-segment-emptiness
                                "A blank segment."
                                " ")

(shaoline-define-simple-segment shaoline-segment-position
                                "Show current line and column position."
                                (propertize (format "%d:%d" (line-number-at-pos) (current-column)) 'face 'shaoline-mode-face))

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
;; Minor modes summary (compact).
(defface shaoline-minor-modes-face
  '((t :inherit (shaoline-base-face) :height 0.7 :foreground "gray60"))
  "Face for the minor modes segment in Shaoline (smaller)."
  :group 'shaoline)

(defcustom shaoline-minor-modes-icon-map
  '(("read-only-mode"            . "🔒")
    ("overwrite-mode"            . "⛔")
    ("auto-save-mode"            . "💾")
    ("visual-line-mode"          . "↩")
    ("ws-butler-mode"            . "🚫␣")
    ("indent-tabs-mode"          . "⇆")
    ("dtrt-indent-mode"          . "↕")
    ("editorconfig-mode"         . "☰")
    ("god-mode"                  . "🧘")
    ("god-local-mode"            . "🧘")
    ("corfu-mode"                . "⚇")
    ("vertico-mode"              . "V")
    ("projectile-mode"           . "🚀")
    ("envrc-mode"                . "⛺")
    ("flyspell-mode"             . "🔤")
    ("spell-fu-mode"             . "📚")
    ("lsp-mode"                  . "🦾")
    ("eglot-managed-mode"        . "🦾")
    ("flycheck-mode"             . "✅")
    ("flymake-mode"              . "🧪")
    ("gptel-mode"                . "🤖")
    ("gptel-aibo-mode"           . "🐕‍🦺")
    ("org-drill-mode"            . "🦉")
    ("olivetti-mode"             . "✍")
    ("org-fancy-priorities-mode" . "⚡")
    ("org-auto-tangle-mode"      . "🧶"))
  "Mapping minor mode variable names (as strings) to an icon/emoji for Shaoline.
Customize this to control which minor modes are shown and what icons are used."
  :type '(alist :key-type string :value-type string)
  :group 'shaoline)

(shaoline-define-simple-segment shaoline-segment-minor-modes
                                "Show ONLY critically important minor modes, as Unicode emoji/symbols (portable), each with a unique color.
Set and extend what to show via `shaoline-minor-modes-icon-map'."
                                (let* ((icon-map shaoline-minor-modes-icon-map)
                                       (seen (make-hash-table :test 'equal))
                                       ;; Helper: deterministic color for a string (from hash)
                                       (shaoline--minor-mode-face-for
                                        (lambda (name)
                                          (let* ((facesym (intern (concat "shaoline-minor-m-face-" name)))
                                                 (color
                                                  (let* ((hue (/ (mod (sxhash name) 360.0) 360.0))
                                                         (sat 0.7)
                                                         (lum 0.55))
                                                    (apply #'color-rgb-to-hex (color-hsl-to-rgb hue sat lum))))
                                                 (doc (format "Face for %s icon in Shaoline minor modes." name)))
                                            (unless (facep facesym)
                                              (eval `(defface ,facesym '((t :inherit shaoline-minor-modes-face :foreground ,color :weight bold))
                                                       ,doc :group 'shaoline)))
                                            facesym)))
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
                                                     (propertize icon 'face (funcall shaoline--minor-mode-face-for name)))))
                                               minor-mode-list))))
                                  (when modes
                                    (propertize
                                     (concat "" (mapconcat #'identity modes "") "")
                                     'face 'shaoline-minor-modes-face))))


;; ----------------------------------------------------------------------------
;; Flycheck/Flymake status (errors/warnings).

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
;; VCS state extension (Git dirty/clean/basic symbol).

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

;; Input method (layout/language indicator)
(shaoline-define-simple-segment shaoline-segment-input-method
                                "Show current input method title (layout) if any, else 'EN'."
                                (let ((indicator
                                       (cond
                                        (current-input-method
                                         (or current-input-method-title current-input-method))
                                        (t "EN"))))
                                  (propertize indicator 'face 'shaoline-mode-face)))

(provide 'shaoline-segments)
;;; shaoline-segments.el ends here
