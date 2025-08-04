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
and raise the baseline by −0.15 em.

GENERATOR is any `all-the-icons-*' function.
ICON      is its first argument.
ARGS      are forwarded, but any :height / :v-adjust supplied by the caller
are ignored to guarantee consistent box size."
  (when (featurep 'all-the-icons)
    (let* ((str (apply generator icon
                       ;; force metrics
                       :height 0.9 :v-adjust 0
                       args)))
      (when (stringp str)
        (propertize str 'display '(raise -0.15))))))

;; ----------------------------------------------------------------------------
;; 二 Buffer Information — Identity and State
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-buffer-name ()
  "Buffer name with yang energy."
  (let ((name (buffer-name)))
    (propertize name 'face 'shaoline-buffer-face)))

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

(shaoline-define-segment shaoline-segment-git-branch ()
  "Current Git branch with icon."
  (shaoline--cached-call
   (shaoline--cache-key "git-branch" default-directory (buffer-file-name))
   3.0
   (lambda ()
     (when (and (featurep 'vc-git) (buffer-file-name))
       (when-let ((branch (vc-git--symbolic-ref (buffer-file-name))))
         (concat
          (when (and shaoline-enable-dynamic-segments
                     (display-graphic-p)
                     (featurep 'all-the-icons))
            (concat (shaoline--icon #'all-the-icons-octicon "git-branch" :face 'shaoline-git-face) " "))
          (propertize branch 'face 'shaoline-git-face)))))))

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
  "Last useful message from Messages buffer, excluding Shaoline's own content."
  (when-let ((msg (shaoline-msg-current)))
    (unless (get-text-property 0 'shaoline-origin msg)
      (let ((cleaned-msg (if (string-match "\n" msg)
                             (concat (car (split-string msg "\n")) " [more]")
                           msg)))
        ;; Always show the last useful message, not current keys
        (when (and (stringp cleaned-msg)
                   (not (string-empty-p (string-trim cleaned-msg)))
                   (not (string-match-p "^Key:" cleaned-msg)))
          (propertize cleaned-msg 'face 'shaoline-echo))))))

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

;; Moon phase calculation (simplified for Dao architecture)
(defconst shaoline--synodic-month 29.530588853
  "Mean synodic month length in days.")

(defvar shaoline--moon-cache '(nil . 0)
  "Cached moon phase: (date . phase-index).")

(defun shaoline--moon-phase-idx ()
  "Calculate current moon phase index 0-7."
  (let ((today (format-time-string "%F")))
    (if (equal today (car shaoline--moon-cache))
        (cdr shaoline--moon-cache)
      (let* ((days-since-epoch (/ (float-time) 86400.0))
             (moon-cycles (/ days-since-epoch shaoline--synodic-month))
             (phase-in-cycle (- moon-cycles (floor moon-cycles)))
             (phase-idx (floor (* phase-in-cycle 8))))
        (setq shaoline--moon-cache (cons today phase-idx))
        phase-idx))))

(shaoline-define-segment shaoline-segment-moon-phase ()
  "Moon phase indicator."
  (when shaoline-enable-dynamic-segments
    (shaoline--cached-call
     (shaoline--cache-key "moon" (format-time-string "%F"))
     86400.0 ; Cache for one day
     (lambda ()
       (let* ((idx (shaoline--moon-phase-idx))
              (phase (if (display-graphic-p)
                         (aref ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"] idx)
                       (aref ["N" "FQ" "F" "G" "F" "G" "LQ" "C"] idx))))
         (propertize phase 'face 'shaoline-yin))))))

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

If DATA is already a string just colourise it; otherwise return
FALLBACK."
  (cond
   ;; ------------------------------------------------------------------
   ;; Alist from `battery-status-function'
   ;; ------------------------------------------------------------------
   ((and (listp data) (cl-every #'consp data))
    (let* ((raw-pct  (or (cdr (assoc 112 data))      ; ?p
                         (cdr (assoc "percentage" data))))
           (pct-str  (cond ((numberp raw-pct)  (number-to-string raw-pct))
                           ((stringp raw-pct)  raw-pct)
                           (t nil)))
           (status   (or (cdr (assoc 66 data))       ; ?b
                         (cdr (assoc "status" data))))
           ;; Face decision ------------------------------------------------
           (face (cond
                  ((and status (string-match-p "\\`\\(Charging\\|AC\\|Full\\)" status))
                   'shaoline-battery-charging-face)
                  ((and pct-str
                        (< (string-to-number
                            (replace-regexp-in-string "[^0-9]" "" pct-str))
                           25))
                   'shaoline-battery-critical-face)
                  (t 'shaoline-battery-face)))
           ;; Icon decision ------------------------------------------------
           (icon (when (and (featurep 'all-the-icons)
                            pct-str
                            (display-graphic-p))
                   (let* ((n (string-to-number
                              (replace-regexp-in-string "[^0-9]" "" pct-str)))
                          (glyph (cond ((>= n 90) "battery-full")
                                       ((>= n 70) "battery-three-quarters")
                                       ((>= n 40) "battery-half")
                                       ((>= n 10) "battery-quarter")
                                       (t          "battery-empty"))))
                     (shaoline--icon #'all-the-icons-faicon glyph :face face)))))
      (if pct-str
          (concat
           (when (and icon (not (string-empty-p icon))) (concat icon " "))
           (propertize (format "%s%%"
                               (replace-regexp-in-string "[^0-9]" "" pct-str))
                       'face face))
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

;; Battery cache variables
(defvar shaoline--segment-battery-cache ""
  "Cached battery segment string.")

(shaoline-define-segment shaoline-segment-battery ()
  "Battery status with async support."
  (if (not shaoline-enable-dynamic-segments)
      ""
    (shaoline--cached-call
     "battery-status"
     1.0 ; Cache for 5 seconds
     (lambda ()
       ;; Lazy load battery
       (require 'battery nil t)
       (let ((fallback (propertize "N/A" 'face 'shaoline-battery-face)))
         (if (and (fboundp 'battery)
                  (boundp 'battery-status-function)
                  battery-status-function)
             ;; Всегда сперва считаем синхронно, чтобы сразу показать результат,
             ;; а затем — обновляем асинхронно, если возможно.
             (let* ((data (funcall battery-status-function))
                    (formatted (shaoline--format-battery data fallback)))
               ;; Кешируем моментальный результат
               (setq shaoline--segment-battery-cache formatted)
               ;; Одновременно обновляем глобальный кэш shaoline--state
               (let ((cache (shaoline--state-get :cache)))
                 (puthash "battery-status"
                          (cons formatted (float-time))
                          cache))
               ;; Запускаем фоновое обновление (не блокирует UI)
               (when (fboundp 'async-start)
                 (async-start
                  (lambda ()
                    (require 'battery)
                    (funcall battery-status-function))
                  (lambda (data)
                    (setq shaoline--segment-battery-cache
                          (shaoline--format-battery data fallback))
                    ;; Обновляем запись в кэше, чтобы новое значение сразу
                    ;; использовалось даже до истечения TTL.
                    (let ((cache (shaoline--state-get :cache)))
                      (puthash "battery-status"
                               (cons shaoline--segment-battery-cache (float-time))
                               cache))
                    ;; Принудительно перерисовываем Shaoline с новыми данными
                    (when (fboundp 'shaoline-update)
                      (shaoline-update t)))))
               ;; Возвращаем либо свежий, либо уже закешированный результат
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
