;;; shaoline-segments.el --- Standard segments for Shaoline modeline -*- lexical-binding: t; -*-

;; Version: 2.1.1

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

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

(defun shaoline--moon-phase-idx (&optional date)
  "Return moon phase index 0..7 for DATE."
  (unless (featurep 'calendar) (require 'calendar nil t))
  (let* ((d        (or date (calendar-current-date)))
         (abs-day  (float (calendar-absolute-from-gregorian d)))
         (synodic  29.530588853)
         (age      (- abs-day (* (floor (/ abs-day synodic)) synodic)))
         (idx      (mod (floor (* age (/ 8.0 synodic))) 8)))
    idx))

(shaoline-define-simple-segment shaoline-segment-time
  "Show current time and moon phase."
  (if (not shaoline-enable-dynamic-segments)
      ""  ;; Returns an empty string if dynamic segments are disabled.
    (let* ((time (propertize (format-time-string " %H:%M ") 'face 'shaoline-time-face))
           (phase-number (shaoline--moon-phase-idx))
           (phases ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
           (moon (propertize (aref phases phase-number) 'face 'shaoline-moon-face)))
      (concat time " " moon "  "))))

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
  (propertize (format "L%d:C%d" (line-number-at-pos) (current-column)) 'face 'shaoline-mode-face))

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
(shaoline-define-simple-segment shaoline-segment-minor-modes
  "Show ONLY critically important minor modes, as Unicode emoji/symbols (portable)."
  (let* (
         ;; Each entry: ("minor-mode-symbol" . "emoji/symbol")
         (icon-map
          `(
            ;; Core text/safety
            ("read-only-mode"            . "🔒")
            ("overwrite-mode"            . "⛔")
            ("auto-save-mode"            . "💾")
            ;; Indentation, whitespace, etc.
            ("visual-line-mode"          . "↩")
            ("ws-butler-mode"            . "🚫␣")
            ("indent-tabs-mode"          . "⇆")
            ("dtrt-indent-mode"          . "↕")
            ("editorconfig-mode"         . "☰")
            ;; Completion/navigation
            ("god-mode"                  . "🧘")
            ("corfu-mode"                . "⚇")
            ("vertico-mode"              . "V")
            ("projectile-mode"           . "🚀")
            ("envrc-mode"                . "⛺")
            ;; Spell and language tools
            ("flyspell-mode"             . "🔤")
            ("spell-fu-mode"             . "📚")
            ;; LSP and code-checking
            ("lsp-mode"                  . "🦾")
            ("eglot-managed-mode"        . "⧉")
            ("flycheck-mode"             . "✅")
            ("flymake-mode"              . "🧪")
            ;; AI & productivity
            ("gptel-mode"                . "🤖")
            ("gptel-aibo-mode"           . "🐕‍🦺")
            ("org-drill-mode"            . "🦉")
            ;; Visual/coding helpers
            ("rainbow-mode"              . "🌈")
            ("highlight-parentheses-mode" . "🟦")
            ;; Org and writing
            ("olivetti-mode"             . "✍")
            ("org-modern-mode"           . "🗐")
            ("org-fancy-priorities-mode" . "⚡")
            ("org-auto-tangle-mode"      . "🧶")
            ))
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
                       icon)))
                 minor-mode-list))))
    (when modes
      (propertize (concat "[" (mapconcat #'identity modes "") "]")
                  'face 'shaoline-mode-face))))



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

(provide 'shaoline-segments)
;;; shaoline-segments.el ends here
