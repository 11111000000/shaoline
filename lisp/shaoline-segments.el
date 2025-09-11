;;; shaoline-segments.el --- Segment garden for Shaoline 3.0 Dao -*- lexical-binding: t; -*-

;; Version: 3.0.0

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; 花園 Garden of Segments — Each segment is a pure function,
;; like flowers in a zen garden. Simple, functional, harmonious.
;;
;; Tao compatibility:
;;   • Uses shaoline-define-segment from core
;;   • TTL caching via shaoline--cached-call
;;   • No external dependencies on old macros
;;   • Pure functional approach

;;; Code:

(require 'shaoline)
(require 'subr-x) ;; string-empty-p, string-trim, etc.
(require 'async nil :noerror)
(eval-when-compile
  (require 'calendar nil t)
  (require 'lunar nil t))

;; ----------------------------------------------------------------------------
;; 一 Core Dependencies — Essential Elements
;; ----------------------------------------------------------------------------

;; Optional dependencies loaded lazily
(declare-function all-the-icons-icon-for-mode "ext:all-the-icons")
(declare-function all-the-icons-octicon "ext:all-the-icons")
(declare-function all-the-icons-faicon "ext:all-the-icons")
(declare-function projectile-project-name "ext:projectile")
(declare-function project-current "project")
(declare-function project-name "project")
(declare-function project-roots "project")
(declare-function vc-git--symbolic-ref "vc-git")

;; ---------------------------------------------------------------------------
;; Icon helper – local to Shaoline
;; ---------------------------------------------------------------------------
(defun shaoline--icon (generator icon &rest args)
  "Render ICON via GENERATOR with unified metrics (height 0.9, no v-adjust)
and raise the baseline by a customizable offset.

GENERATOR is any `all-the-icons-*' function.
ICON      is its first argument.
ARGS      are forwarded, but any :height or :v-adjust supplied by the caller
are overridden to guarantee a consistent box size unless overridden by :raise.
A :raise keyword argument can be provided to adjust the vertical alignment.
If :raise is not provided, the default offset of −0.15 em is used."
  (when (featurep 'all-the-icons)
    (let* ((raise-offset (or (plist-get args :raise) -0.15))
           ;; Remove :raise from ARGS so it isn't passed to the generator
           (filtered-args
            (cl-loop for (k v) on args by #'cddr
                     unless (eq k :raise)
                     append (list k v)))
           (str (apply generator icon
                       :height 0.9 :v-adjust 0
                       filtered-args)))
      (when (stringp str)
        (propertize str 'display `(raise ,raise-offset))))))


;; ----------------------------------------------------------------------------
;; 二 Buffer Information — Identity and State
;; ----------------------------------------------------------------------------

(defcustom shaoline-buffer-name-max-length 30
  "Maximum length (in characters) for the buffer name segment.

When nil or 0, do not truncate.
When truncation occurs, an ellipsis character (…) is appended."
  :type '(choice (const :tag "No truncation" nil)
                 (integer :tag "Max characters"))
  :group 'shaoline)

(shaoline-define-segment shaoline-segment-buffer-name ()
  "Buffer name with yang energy."
  (let* ((name (buffer-name))
         (maxlen shaoline-buffer-name-max-length)
         (display (if (and (integerp maxlen)
                           (> maxlen 0)
                           (> (length name) maxlen))
                      (concat (substring name 0 (max 0 (1- maxlen))) "…")
                    name)))
    (propertize display 'face 'shaoline-buffer-face)))

(shaoline-define-segment shaoline-segment-modified ()
  "Modified indicator — the asterisk of change."
  (when (and (buffer-modified-p) (buffer-file-name))
    (propertize "*" 'face 'shaoline-modified-face)))

(shaoline-define-segment shaoline-segment-major-mode ()
  "Major mode with stable caching — changes only on mode switch."
  (shaoline--stable-cached-call
   "major-mode"
   (list major-mode (display-graphic-p) shaoline-enable-dynamic-segments)
   (lambda ()
     (let ((icon (when (and shaoline-enable-dynamic-segments
                            (display-graphic-p)
                            (featurep 'all-the-icons)
                            major-mode)
                   (shaoline--icon #'all-the-icons-icon-for-mode major-mode))))
       (concat
        (when (and icon (stringp icon) (not (string-empty-p icon)))
          (concat icon " "))
        (propertize (format-mode-line mode-name)
                    'face 'shaoline-mode-face))))))

(shaoline-define-segment shaoline-segment-major-mode-icon ()
  "Major mode icon only (GUI) or abbreviated text (TTY)."
  (shaoline--cached-call
   (shaoline--cache-key "mode-icon" major-mode (display-graphic-p))
   shaoline-cache-ttl
   (lambda ()
     (cond
      ;; GUI with icons
      ((and (display-graphic-p)
            shaoline-enable-dynamic-segments
            (featurep 'all-the-icons)
            major-mode)
       (shaoline--icon #'all-the-icons-icon-for-mode major-mode))
      ;; TTY fallback
      (major-mode
       (propertize (format-mode-line mode-name) 'face 'shaoline-mode-face))
      (t "")))))

;; ----------------------------------------------------------------------------
;; 三 Position and Navigation — Where We Are
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-position (&optional &key with-column)
  "Current position.  По умолчанию только строка; колонку добавляем,
когда WITH-COLUMN ненулев."
  (propertize
   (if with-column
       (format "[%d:%d]" (line-number-at-pos) (current-column))
     (format "[%d]" (line-number-at-pos)))
   'face 'shaoline-mode-face))

(shaoline-define-segment shaoline-segment-encoding ()
  "File encoding and EOL type."
  (when buffer-file-name
    (let* ((coding (symbol-name (or buffer-file-coding-system 'undecided)))
           (coding (if (string-match-p "utf-8" coding) "UTF8" coding))
           (eol (pcase (coding-system-eol-type (or buffer-file-coding-system 'undecided))
                  (0 "LF")
                  (1 "CRLF")
                  (2 "CR")
                  (_ ""))))
      (propertize (format "%s %s" coding eol) 'face 'shaoline-mode-face))))

;; ----------------------------------------------------------------------------
;; 四 Project and Version Control — Collaborative Context
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-project-name ()
  "Project name — ultra-stable caching until directory change."
  (shaoline--stable-cached-call
   "project"
   default-directory  ; Only changes when we switch directories
   (lambda ()
     (when-let ((name (cond
                       ((fboundp 'project-current)
                        (when-let ((proj (project-current)))
                          (if (fboundp 'project-name)
                              (project-name proj)
                            (file-name-nondirectory
                             (directory-file-name (car (project-roots proj)))))))
                       ((and (featurep 'projectile) (fboundp 'projectile-project-name))
                        (projectile-project-name))
                       (t nil))))
       (propertize name 'face 'shaoline-project-face)))))

;; Robust Git helpers — repo detection and fast invalidation
(defun shaoline--git-top-level (dir)
  "Return top-level Git directory for DIR or nil.
Avoids remote TRAMP paths and gracefully handles errors."
  (when (and dir (not (file-remote-p dir)) (executable-find "git"))
    (let ((default-directory dir))
      (condition-case nil
          (car (process-lines "git" "rev-parse" "--show-toplevel"))
        (error nil)))))

(defun shaoline--git-depsig (dir)
  "Return cache dependency signature for Git HEAD in DIR.
Signature changes immediately when branch changes or new commit is checked out."
  (when-let ((toplevel (shaoline--git-top-level dir)))
    (let* ((default-directory toplevel)
           (gitdir
            (condition-case nil
                (car (process-lines "git" "rev-parse" "--git-dir"))
              (error ".git")))
           (gitdir (expand-file-name gitdir toplevel))
           (head (expand-file-name "HEAD" gitdir))
           (head-mtime (when (file-exists-p head)
                         (file-attribute-modification-time
                          (file-attributes head))))
           (ref-file
            (when (and (file-exists-p head)
                       (with-temp-buffer
                         (insert-file-contents head)
                         (goto-char (point-min))
                         (when (looking-at "ref: \\(.+\\)")
                           (expand-file-name (match-string 1) gitdir))))))
           (ref-mtime (when (and ref-file (file-exists-p ref-file))
                        (file-attribute-modification-time
                         (file-attributes ref-file)))))
      (list toplevel head-mtime ref-mtime))))

(defun shaoline--git-current-branch (dir)
  "Return current branch name for DIR, or detached short SHA if not on a branch."
  (when-let ((toplevel (shaoline--git-top-level dir)))
    (let ((default-directory toplevel))
      (or
       (condition-case nil
           (car (process-lines "git" "symbolic-ref" "--quiet" "--short" "HEAD"))
         (error nil))
       (condition-case nil
           (let ((sha (car (process-lines "git" "rev-parse" "--short" "HEAD"))))
             (format "detached:%s" sha))
         (error nil))))))

(shaoline-define-segment shaoline-segment-git-branch ()
  "Current Git branch with icon. Works in non-file buffers (e.g., Dired)."
  (let* ((dir (or (and (buffer-file-name) (file-name-directory (buffer-file-name)))
                  default-directory))
         (deps (shaoline--git-depsig dir)))
    (when deps
      (shaoline--stable-cached-call
       "git-branch"
       deps
       (lambda ()
         (when-let ((branch (shaoline--git-current-branch dir)))
           (concat
            (when (and shaoline-enable-dynamic-segments
                       (display-graphic-p)
                       (featurep 'all-the-icons))
              (concat (shaoline--icon #'all-the-icons-octicon "git-branch" :face 'shaoline-git-face) " "))
            (propertize branch 'face 'shaoline-git-face))))))))

(shaoline-define-segment shaoline-segment-vcs-state ()
  "Git status indicator."
  (when (and (featurep 'vc-git) (buffer-file-name))
    (let ((state (vc-state (buffer-file-name))))
      (pcase state
        (`edited (propertize "+" 'face 'shaoline-git-face))
        (`added (propertize "+" 'face 'shaoline-git-face))
        (`removed (propertize "!" 'face 'shaoline-git-face))
        (`conflict (propertize "✗" 'face 'error))
        (`missing (propertize "?" 'face 'warning))
        (_ "")))))

;; ----------------------------------------------------------------------------
;; 五 Messages and Communication — Voice of the System
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-echo-message ()
  "Last useful message from Messages buffer, excluding Shaoline's own content.

Also falls back to `current-message' to catch echo-only messages that
are not logged to *Messages* (e.g., when `inhibit-message' or
`message-log-max' suppress logging). Prefers stored message if it is
recent (<= 10s)."
  (let* ((stored (shaoline-msg-current))
         (raw (or (and stored (<= (shaoline-msg-age) 10.0) stored)
                  (current-message))))
    (when (and (stringp raw) (not (string-empty-p (string-trim raw))))
      ;; Skip Shaoline's own messages (marked) and key-hints
      (unless (or (get-text-property 0 'shaoline-origin raw)
                  (string-match-p "^Key:" raw))
        (let* ((oneline (if (string-match "\n" raw)
                            (concat (car (split-string raw "\n")) " [more]")
                          raw)))
          (propertize oneline 'face 'shaoline-echo))))))

(shaoline-define-segment shaoline-segment-current-keys ()
  "Display current prefix key combination being typed.

If the user is entering an extended command via \\[execute-extended-command],
collapse the ongoing literal input so that instead of
\"M-x s h a o l i n e - m o d e RET\" we simply show \"M-x …\"."
  (shaoline--log "shaoline-segment-current-keys: called")
  (let* ((raw (progn
                (shaoline--log "shaoline-segment-current-keys: about to call shaoline--get-current-keys")
                (shaoline--get-current-keys)))
         ;; Collapse long M-x sequences     ────────────────────────────────
         (keys (cond
                ((and raw (string-match-p "\\`M-x [[:graph:]]" raw))
                 "M-x …")
                (t raw))))
    (shaoline--log "shaoline-segment-current-keys: after processing, keys=%S" keys)
    (when (and keys (not (string-empty-p keys)))
      (shaoline--log "shaoline-segment-current-keys: will return [%s]" keys)
      (propertize (format "[%s]" keys) 'face 'shaoline-current-keys-face))))

;; ----------------------------------------------------------------------------
;; 六 Time and Cosmic Elements — Dynamic Universe
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-time ()
  "Digital clock display."
  (when shaoline-enable-dynamic-segments
    (propertize (format-time-string "%H:%M") 'face 'shaoline-time-face)))

(shaoline-define-segment shaoline-segment-digital-clock ()
  "Digital clock with padding."
  (when shaoline-enable-dynamic-segments
    (propertize (format-time-string " %H:%M ") 'face 'shaoline-time-face)))

(defcustom shaoline-day-date-with-year nil
  "Whether to include year in date segment."
  :type 'boolean
  :group 'shaoline)

(shaoline-define-segment shaoline-segment-day-date (&rest args)
  "Date segment with optional year."
  (when shaoline-enable-dynamic-segments
    (let* ((with-year (or (plist-get args :with-year) shaoline-day-date-with-year))
           (fmt (if with-year "%d.%m.%Y" "%d.%m")))
      (propertize (format-time-string fmt) 'face 'shaoline-date-face))))

;; Moon phase calculation — improved: use lunar.el when available
(defconst shaoline--synodic-month 29.530588853
  "Mean synodic month length in days (synodic month).")

(defun shaoline--jd-now ()
  "Return astronomical Julian Day for current local time."
  (+ (/ (float-time) 86400.0) 2440587.5))

(defun shaoline--moon-fraction-using-lunar ()
  "Return cons (fraction . info) using lunar.el for precise new-moon times.
fraction is 0..1 (0=new moon), info contains :prev, :next, :span, :age."
  (when (require 'lunar nil t)
    (let* ((jd (shaoline--jd-now))
           (next (lunar-new-moon-on-or-after jd))
           (prev (lunar-new-moon-on-or-after (- jd shaoline--synodic-month))))
      (when (> prev jd)
        (setq prev (lunar-new-moon-on-or-after (- jd (* 2 shaoline--synodic-month)))))
      (let* ((span (- next prev))
             (age  (- jd prev))
             (frac (max 0.0 (min 1.0 (/ age span)))))
        (cons frac (list :prev prev :next next :span span :age age))))))

(defun shaoline--moon-fraction-fallback ()
  "Return cons (fraction . nil) using calibrated epoch fallback."
  (let* ((jd (shaoline--jd-now))
         (epoch 2451550.1) ; 2000-01-06 18:14 UTC, a known new moon
         (cycle (mod (/ (- jd epoch) shaoline--synodic-month) 1.0)))
    (cons cycle nil)))

(defun shaoline--moon-phase8 ()
  "Return plist (:idx 0..7 :percent 0..100 :info ALIST) for the current moon."
  (let* ((res (or (shaoline--moon-fraction-using-lunar)
                  (shaoline--moon-fraction-fallback)))
         (frac (car res))
         (info (cdr res))
         (idx (min 7 (max 0 (floor (* frac 8.0)))))
         (pct (max 0 (min 100 (floor (+ 0.5 (* frac 100.0)))))))
    (list :idx idx :percent pct :info info)))

(shaoline-define-segment shaoline-segment-moon-phase ()
  "Moon phase indicator (8-step), with lunar.el precision when available."
  (when shaoline-enable-dynamic-segments
    (shaoline--cached-call
     (shaoline--cache-key "moon" (floor (/ (float-time) 3600)))
     3600.0 ; Cache for one hour
     (lambda ()
       (let* ((data (shaoline--moon-phase8))
              (idx (plist-get data :idx))
              (pct (plist-get data :percent))
              (glyphs (if (display-graphic-p)
                          ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"]
                        ["N" "WC" "FQ" "WG" "F" "WG-" "LQ" "WC-"]))
              (labels ["New" "Waxing crescent" "First quarter" "Waxing gibbous"
                       "Full" "Waning gibbous" "Last quarter" "Waning crescent"])
              (txt (aref glyphs idx))
              (desc (aref labels idx))
              (hint (format "%s — %d%% illuminated" desc pct)))
         (propertize txt 'face 'shaoline-yin 'help-echo hint))))))

;; ----------------------------------------------------------------------------
;; 七 System Information — Hardware and Status
;; ----------------------------------------------------------------------------

;; Battery formatter (rewritten – balanced parens, clearer logic)
(defun shaoline--format-battery (data fallback)
  "Return a colour-coded battery string built from DATA.

When DATA is the alist produced by `battery-status-function'
choose an icon and a face according to charging state and
percentage:

  charging / AC / full  → shaoline-battery-charging-face (green)
  below 25 % and discharging → shaoline-battery-critical-face (red)
  otherwise                     → shaoline-battery-face (grey)

Normalises buggy backends that report tenths (e.g. 951 → 95.1),
clamps to 0..100 and rounds.

If DATA is already a string just colourise it; otherwise return
FALLBACK."
  (cond
   ;; ------------------------------------------------------------------
   ;; Alist from `battery-status-function'
   ;; ------------------------------------------------------------------
   ((and (listp data) (cl-every #'consp data))
    (let* ((status (or (cdr (assq ?B data))             ; uppercase key
                       (cdr (assq ?b data))             ; lowercase key
                       (cdr (assq 'status data))        ; symbol
                       (cdr (assq :status data))        ; keyword
                       (cdr (assoc "status" data))))    ; string
           ;; Extract and normalise percentage (share logic with *-safe)
           (raw   (shaoline--battery-extract-percentage data))
           (norm  (cond
                   ((and raw (> raw 1000)) (/ raw 100.0)) ; 944 → 94.4
                   ((and raw (> raw 100))  (/ raw 10.0))  ; 951 → 95.1
                   (t raw)))
           (pct   (and norm (max 0 (min 100 (floor (+ 0.5 norm))))))
           ;; Normalized status and charging detection --------------------
           (st (and status (downcase (format "%s" status))))
           (charging-p (and st (string-match-p "\\`\\(charging\\|ac\\|full\\)" st)))
           ;; Face decision ------------------------------------------------
           (face (cond
                  (charging-p 'shaoline-battery-charging-face)
                  ((and pct (< pct 25))
                   'shaoline-battery-critical-face)
                  (t 'shaoline-battery-face)))
           ;; Icon decision ------------------------------------------------
           (icon (when (and (featurep 'all-the-icons)
                            pct
                            (display-graphic-p))
                   (let* ((glyph (cond ((>= pct 90) "battery-full")
                                       ((>= pct 70) "battery-three-quarters")
                                       ((>= pct 40) "battery-half")
                                       ((>= pct 10) "battery-quarter")
                                       (t           "battery-empty"))))
                     (shaoline--icon #'all-the-icons-faicon glyph :face face :raise 0)))))
      (if pct
          (concat
           (when (and icon (not (string-empty-p icon))) (concat icon " "))
           (propertize (format "%d%%" pct) 'face face))
        fallback)))
   ;; ------------------------------------------------------------------
   ;; Already formatted string
   ;; ------------------------------------------------------------------
   ((stringp data)
    (propertize data 'face 'shaoline-battery-face))
   ;; ------------------------------------------------------------------
   ;; Anything else
   ;; ------------------------------------------------------------------
   (t fallback)))

(defcustom shaoline-battery-update-interval 60.0
  "Seconds to cache battery status between refreshes."
  :type 'float
  :group 'shaoline)

(defcustom shaoline-battery-prefer-linux-sysfs t
  "Prefer the fast non-DBus backend when available."
  :type 'boolean
  :group 'shaoline)

;; Robust percentage extraction and formatting for battery backends
(defun shaoline--battery-extract-percentage (data)
  "Extract numeric percentage from DATA returned by `battery-*' backends.
Supports char-key alists like ((?p . \"81\") ...) and keyword-based plists."
  (cond
   ;; Standard Emacs battery alist: (?p . \"81\")
   ((and (listp data)
         (or (assq ?p data) (assoc ?p data)))
    (string-to-number (cdr (or (assq ?p data) (assoc ?p data)))))
   ;; Keyword/word-based maps: (:percentage 81) or ('percentage . \"81\")
   ((and (listp data)
         (or (assq 'percentage data) (assoc 'percentage data)))
    (let ((v (cdr (or (assq 'percentage data) (assoc 'percentage data)))))
      (cond ((numberp v) v)
            ((stringp v) (string-to-number v))
            (t nil))))
   (t nil)))

(defun shaoline--format-battery-safe (data fallback)
  "Format battery DATA into a percent string, normalizing out-of-range values.
If DATA cannot be parsed, return FALLBACK."
  (condition-case _err
      (let* ((raw (shaoline--battery-extract-percentage data))
             ;; Normalize weird scales: 944 -> 94.4, 12345 -> 123.45, etc.
             (norm (cond
                    ((and raw (> raw 1000)) (/ raw 100.0))
                    ((and raw (> raw 100)) (/ raw 10.0))
                    (t raw)))
             ;; Clamp to [0..100] and round to int
             (pct (and norm
                       (max 0 (min 100 (floor (+ 0.5 norm))))))
             (txt (and pct (format "%d%%" pct))))
        (if txt
            (propertize txt 'face 'shaoline-battery-face)
          fallback))
    (error fallback)))

;; Battery cache variables
(defvar shaoline--segment-battery-cache ""
  "Cached battery segment string.")

(shaoline-define-segment shaoline-segment-battery ()
  "Battery status without heavy D-Bus polling; cached."
  (if (not shaoline-enable-dynamic-segments)
      ""
    (shaoline--cached-call
     "battery-status"
     shaoline-battery-update-interval
     (lambda ()
       ;; Lazy load battery
       (require 'battery nil t)
       (let ((fallback (propertize "N/A" 'face 'shaoline-battery-face)))
         (if (and (fboundp 'battery)
                  (or (boundp 'battery-status-function)
                      (fboundp 'battery-linux-sysfs)))
             (let* ((status-fn
                     (cond
                      ((and shaoline-battery-prefer-linux-sysfs
                            (fboundp 'battery-linux-sysfs))
                       #'battery-linux-sysfs)
                      ((and (boundp 'battery-status-function)
                            battery-status-function)
                       battery-status-function)
                      (t nil)))
                    (data (when status-fn (funcall status-fn)))
                    ;; Use the full formatter to restore icon and charging colour
                    (formatted (shaoline--format-battery data fallback)))
               ;; Cache and return
               (setq shaoline--segment-battery-cache formatted)
               (let ((cache (shaoline--state-get :cache)))
                 (puthash "battery-status"
                          (cons formatted (float-time))
                          cache))
               shaoline--segment-battery-cache)
           fallback))))))

;; ----------------------------------------------------------------------------
;; 八 Development and Tools — Coding Context
;; ----------------------------------------------------------------------------

(defvar shaoline--minor-modes-cache ""
  "Cached minor modes display.")

(defvar shaoline--minor-modes-timestamp 0
  "Last update time for minor modes cache.")

(defcustom shaoline-minor-modes-cache-ttl 1.0
  "Cache TTL for minor modes segment."
  :type 'float
  :group 'shaoline)

(defcustom shaoline-minor-modes-icon-map
  '(("read-only-mode" . "🔒")
    ("overwrite-mode" . "✏️")
    ("company-mode" . "🤝")
    ("yas-minor-mode" . "🧩")
    ("flycheck-mode" . "🩺")
    ("flymake-mode" . "🔬")
    ("lsp-mode" . "🧠")
    ("eglot-managed-mode" . "🎛️")
    ("god-mode" . "⚡")
    ("evil-mode" . "😈")
    ("projectile-mode" . "🌌")
    ("magit-auto-revert-mode" . "🔄"))
  "Icon mapping for minor modes."
  :type '(alist :key-type string :value-type string)
  :group 'shaoline)

(defcustom shaoline-minor-modes-text-map
  '(("read-only-mode" . "RO")
    ("overwrite-mode" . "OW")
    ("company-mode" . "CO")
    ("yas-minor-mode" . "YS")
    ("flycheck-mode" . "FC")
    ("flymake-mode" . "FM")
    ("lsp-mode" . "LS")
    ("eglot-managed-mode" . "EG")
    ("god-mode" . "GO")
    ("evil-mode" . "EV")
    ("projectile-mode" . "PJ"))
  "Text abbreviations for minor modes."
  :type '(alist :key-type string :value-type string)
  :group 'shaoline)

(shaoline-define-segment shaoline-segment-minor-modes ()
  "Display active minor modes."
  (let ((now (float-time)))
    (if (< (- now shaoline--minor-modes-timestamp) shaoline-minor-modes-cache-ttl)
        shaoline--minor-modes-cache
      (setq shaoline--minor-modes-timestamp now
            shaoline--minor-modes-cache
            (let* ((gui-p (display-graphic-p))
                   (icon-map shaoline-minor-modes-icon-map)
                   (text-map shaoline-minor-modes-text-map)
                   (modes (delq nil
                                (mapcar
                                 (lambda (mode-pair)
                                   (when (and (boundp (car mode-pair))
                                              (symbol-value (car mode-pair)))
                                     (let* ((mode-name (symbol-name (car mode-pair)))
                                            (display (if gui-p
                                                         (cdr (assoc mode-name icon-map))
                                                       (cdr (assoc mode-name text-map)))))
                                       (when display
                                         (propertize display 'face 'shaoline-yin)))))
                                 minor-mode-alist))))
              (if modes
                  (propertize (concat "(" (mapconcat #'identity modes "") ")")
                              'face 'shaoline-yin)
                ""))))))

(shaoline-define-segment shaoline-segment-flycheck ()
  "Flycheck/Flymake error and warning counts."
  (cond
   ((and (bound-and-true-p flycheck-mode)
         (fboundp 'flycheck-count-errors)
         flycheck-current-errors)
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (err (or (cdr (assq 'error counts)) 0))
           (warn (or (cdr (assq 'warning counts)) 0)))
      (when (or (> err 0) (> warn 0))
        (propertize (format "E:%d W:%d" err warn) 'face 'shaoline-modified-face))))
   ((and (bound-and-true-p flymake-mode)
         (fboundp 'flymake-diagnostics))
    (let* ((all (flymake-diagnostics))
           (err (cl-count-if (lambda (d) (eq (flymake-diagnostic-type d) :error)) all))
           (warn (cl-count-if (lambda (d) (eq (flymake-diagnostic-type d) :warning)) all)))
      (when (or (> err 0) (> warn 0))
        (propertize (format "E:%d W:%d" err warn) 'face 'shaoline-modified-face))))
   (t "")))

(shaoline-define-segment shaoline-segment-input-method ()
  "Current input method indicator."
  (propertize (or current-input-method-title "EN")
              'face 'shaoline-mode-face))

;; ----------------------------------------------------------------------------
;; AI / LLM — gptel Integration
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-gptel-model ()
  "Display the current gptel model (never empty)."
  ;; Ensure gptel is loaded if available, but don't error if not in load-path.
  (unless (featurep 'gptel)
    (ignore-errors
      (when-let ((lib (locate-library "gptel")))
        (load lib nil 'nomessage))))
  (let* ((model
          (cond
           ((and (boundp 'gptel-model) (symbolp gptel-model))
            (symbol-name gptel-model))
           ((and (boundp 'gptel-model) (stringp gptel-model))
            gptel-model)
           ((and (fboundp 'gptel--model-name) (boundp 'gptel-model))
            (ignore-errors (gptel--model-name gptel-model)))
           (t nil))))
    (shaoline--log "gptel-seg: feature=%s model=%S backend=%s gui=%s"
                   (featurep 'gptel)
                   model
                   (and (boundp 'gptel-backend)
                        (ignore-errors (gptel-backend-name gptel-backend)))
                   (display-graphic-p))
    (let* ((model-str (or (and (stringp model) (not (string-empty-p model)) model)
                          (and (boundp 'gptel-backend)
                               (ignore-errors (gptel-backend-name gptel-backend)))
                          ;; Final fallback so the segment is never empty:
                          "gptel"))
           ;; Always prefer a reliable emoji robot in GUI to avoid font issues
           (icon (and (display-graphic-p) "🤖")))
      (concat
       (when (and icon (stringp icon) (not (string-empty-p icon)))
         (concat icon " "))
       (propertize model-str 'face 'shaoline-gptel-face)))));; ----------------------------------------------------------------------------
;; 九 Utility Segments — Simple Building Blocks
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-emptiness ()
  "Empty space for layout."
  " ")

;; ----------------------------------------------------------------------------
;; 十 Segment Registration — Backwards Compatibility
;; ----------------------------------------------------------------------------

;; Register all segments in the old table for compatibility
(let ((segments '(shaoline-segment-buffer-name
                  shaoline-segment-modified
                  shaoline-segment-major-mode
                  shaoline-segment-major-mode-icon
                  shaoline-segment-position
                  shaoline-segment-encoding
                  shaoline-segment-project-name
                  shaoline-segment-git-branch
                  shaoline-segment-vcs-state
                  shaoline-segment-echo-message
                  shaoline-segment-current-keys
                  shaoline-segment-time
                  shaoline-segment-digital-clock
                  shaoline-segment-day-date
                  shaoline-segment-moon-phase
                  shaoline-segment-battery
                  shaoline-segment-minor-modes
                  shaoline-segment-flycheck
                  shaoline-segment-input-method
                  shaoline-segment-emptiness)))
  (dolist (segment segments)
    (when (fboundp segment)
      (puthash segment (symbol-function segment) shaoline--segment-table))))

(provide 'shaoline-segments)
;;; shaoline-segments.el ends here
