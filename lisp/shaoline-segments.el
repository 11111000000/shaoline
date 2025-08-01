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

;; ----------------------------------------------------------------------------
;; 二 Buffer Information — Identity and State
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-buffer-name ()
  "Buffer name with yang energy."
  (propertize (buffer-name) 'face 'shaoline-buffer-face))

(shaoline-define-segment shaoline-segment-modified ()
  "Modified indicator — the asterisk of change."
  (when (and (buffer-modified-p) (buffer-file-name))
    (propertize "*" 'face 'shaoline-modified-face)))

(shaoline-define-segment shaoline-segment-major-mode ()
  "Major mode with optional icon."
  (shaoline--cached-call
   (shaoline--cache-key "major-mode" major-mode (display-graphic-p))
   shaoline-cache-ttl
   (lambda ()
     (let ((icon (when (and shaoline-enable-dynamic-segments
                            (display-graphic-p)
                            (featurep 'all-the-icons)
                            major-mode)
                   (all-the-icons-icon-for-mode major-mode :height 0.9))))
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
       (all-the-icons-icon-for-mode major-mode :height 0.8))
      ;; TTY fallback
      (major-mode
       (propertize (format-mode-line mode-name) 'face 'shaoline-mode-face))
      (t "")))))

;; ----------------------------------------------------------------------------
;; 三 Position and Navigation — Where We Are
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-position ()
  "Current position as line:column."
  (propertize (format "%d:%d" (line-number-at-pos) (current-column))
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
  "Project name using project.el or Projectile."
  (shaoline--cached-call
   (shaoline--cache-key "project" default-directory)
   5.0 ; Cache for 5 seconds
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
            (concat (all-the-icons-octicon "git-branch" :v-adjust 0 :height 1.0
                                           :face 'shaoline-git-face) " "))
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
  "Current captured message, excluding Shaoline's own content."
  (when-let ((msg (shaoline-msg-current)))
    (unless (get-text-property 0 'shaoline-origin msg)
      (if (string-match "\n" msg)
          (propertize (concat (car (split-string msg "\n")) " [more]") 'face 'shaoline-echo)
        (propertize msg 'face 'shaoline-echo)))))

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

;; Battery cache variables
(defvar shaoline--segment-battery-cache ""
  "Cached battery segment string.")

(defun shaoline--format-battery (data fallback)
  "Format battery DATA into display string."
  (cond
   ((and (listp data) (cl-every #'consp data))
    (let* ((percent (or (cdr (assoc 112 data))   ; ?p
                        (cdr (assoc "percentage" data))))
           (status (or (cdr (assoc 66 data))     ; ?b
                       (cdr (assoc "status" data))))
           (icon (when (and (featurep 'all-the-icons) percent (display-graphic-p))
                   (let ((n (string-to-number (replace-regexp-in-string "[^0-9]" "" percent))))
                     (cond
                      ((>= n 90) (all-the-icons-faicon "battery-full" :height 0.75))
                      ((>= n 70) (all-the-icons-faicon "battery-three-quarters" :height 0.75))
                      ((>= n 40) (all-the-icons-faicon "battery-half" :height 0.75))
                      ((>= n 10) (all-the-icons-faicon "battery-quarter" :height 0.75))
                      (t (all-the-icons-faicon "battery-empty" :height 0.75)))))))
      (if percent
          (concat
           (when (and icon (stringp icon) (not (string-empty-p icon)))
             (concat icon " "))
           (propertize (format "%s%%" (replace-regexp-in-string "[^0-9]" "" percent))
                       'face 'shaoline-battery-face))
        fallback)))
   ((stringp data) (propertize data 'face 'shaoline-battery-face))
   (t fallback)))

(shaoline-define-segment shaoline-segment-battery ()
  "Battery status with async support."
  (if (not shaoline-enable-dynamic-segments)
      ""
    (shaoline--cached-call
     "battery-status"
     10.0 ; Cache for 10 seconds
     (lambda ()
       (let ((fallback (propertize "N/A" 'face 'shaoline-battery-face)))
         (if (and (fboundp 'battery)
                  (boundp 'battery-status-function)
                  battery-status-function)
             ;; Use async if available, otherwise sync
             (if (fboundp 'async-start)
                 (progn
                   (async-start
                    (lambda ()
                      (require 'battery)
                      (funcall ',battery-status-function))
                    (lambda (data)
                      (let ((formatted (shaoline--format-battery data fallback)))
                        (setq shaoline--segment-battery-cache formatted)
                        (when (fboundp 'shaoline--update)
                          (shaoline--update)))))
                   ;; Return cached or placeholder while loading
                   (if (string-empty-p shaoline--segment-battery-cache)
                       (propertize "Batt..." 'face 'shaoline-battery-face)
                     shaoline--segment-battery-cache))
               ;; Synchronous fallback
               (shaoline--format-battery (funcall battery-status-function) fallback))
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
