;;; shaoline.el --- Functional minimalist echo-area modeline -*- lexical-binding: t; -*-

;; Version: 3.0.0

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;;; Commentary:

;; 道可道，非常道 — The Dao that can be expressed is not the eternal Dao
;;
;; Shaoline 3.0 follows the path of Wu Wei (無為) — effortless action.
;; Pure functions compose reality; impure effects merely manifest it.
;;
;; Architecture of Emptiness:
;;   • Pure Core (此文件) — functional composition, no side effects
;;   • Effect Layer (shaoline-effects.el) — isolated world changes
;;   • Strategy Layer (shaoline-strategy.el) — adaptive behaviors
;;   • Mode Interface (shaoline-mode.el) — user interaction gateway
;;
;; Like water flowing around rocks, Shaoline adapts without forcing.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'rx))

;; ----------------------------------------------------------------------------
;; 一 Fundamental Variables — The Unchanging Essence
;; ----------------------------------------------------------------------------

(defgroup shaoline nil
  "Functional minimalist echo-area modeline following the Dao."
  :group 'convenience
  :prefix "shaoline-")

;; Core behavior flags
(defcustom shaoline-mode-strategy 'adaptive
  "Strategy for Shaoline behavior adaptation.
- adaptive: Automatically choose between yin/yang based on context
- yin: Passive mode (pure, manual updates only, no hooks/advice)
- yang: Active mode (always visible, hooks, advice)"
  :type '(choice (const adaptive) (const yin) (const yang))
  :group 'shaoline)

(defcustom shaoline-segments
  '((:left shaoline-segment-major-mode  shaoline-segment-buffer-name shaoline-segment-modified)
    (:center shaoline-segment-echo-message)
    (:right  shaoline-segment-current-keys shaoline-segment-position shaoline-segment-time))
  "Segment configuration following the Three Treasures pattern.
Structure: ((:left segment ...) (:center segment ...) (:right segment ...))"
  :type 'sexp
  :group 'shaoline)

;; Performance wisdom
(defcustom shaoline-update-debounce 0.15
  "Seconds to wait between updates. Increased for better performance."
  :type 'float
  :group 'shaoline)

(defcustom shaoline-cache-ttl 2.0
  "Default TTL for segment caches. Impermanence with purpose."
  :type 'float
  :group 'shaoline)

;; Feature toggles (consolidated from legacy)
(defcustom shaoline-enable-dynamic-segments t
  "Enable dynamic segments like time and battery that update automatically."
  :type 'boolean
  :group 'shaoline)

(defcustom shaoline-debug nil
  "Enable debug logging for troubleshooting."
  :type 'boolean
  :group 'shaoline)

(defcustom shaoline-right-margin 2
  "Fixed right margin in characters for right-aligned segments."
  :type 'integer
  :group 'shaoline)

(defcustom shaoline-hide-modeline t
  "When non-nil, hide traditional modelines in all buffers.
The original modeline-format of each buffer is saved and restored
when Shaoline is disabled or this option is turned off."
  :type 'boolean
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; 二 State Container — The Still Water
;; ----------------------------------------------------------------------------

(defvar shaoline--state
  (list :last-content ""
        :cache (make-hash-table :test 'equal)
        :timers '()
        :hooks '()
        :strategy nil
        :last-left-width 0
        :last-right-width 0)
  "Central state container. All flows through here.")

(defun shaoline--state-get (key)
  "Retrieve KEY from central state."
  (plist-get shaoline--state key))

(defun shaoline--state-put (key value)
  "Store VALUE at KEY in central state."
  (setq shaoline--state (plist-put shaoline--state key value)))

;; ----------------------------------------------------------------------------
;; 三 Message Engine — Capturing User Intent
;; ----------------------------------------------------------------------------

(defvar shaoline--last-message nil)
(defvar shaoline--last-message-time 0)

(defun shaoline-msg-save (content)
  "Capture message CONTENT like dew on morning leaves."
  (when (and content (not (string-empty-p content)))
    (setq shaoline--last-message (copy-sequence content)
          shaoline--last-message-time (float-time))))

(defun shaoline-msg-current ()
  "Return current captured message, or nil."
  shaoline--last-message)

(defun shaoline-msg-age ()
  "Return age of current message in seconds."
  (- (float-time) shaoline--last-message-time))

(defun shaoline-msg-clear ()
  "Clear captured message."
  (setq shaoline--last-message nil
        shaoline--last-message-time 0))

;; ----------------------------------------------------------------------------
;; 四 Faces — Outer Appearance Following Inner Nature
;; ----------------------------------------------------------------------------

(defface shaoline-base
  '((t :inherit default :height 1.0))
  "Base face — foundation like earth."
  :group 'shaoline)

(defface shaoline-yin
  '((t :inherit shaoline-base :foreground "gray70"))
  "Yin face — passive, receptive."
  :group 'shaoline)

(defface shaoline-yang
  '((t :inherit shaoline-base :foreground "white" :weight bold))
  "Yang face — active, projecting."
  :group 'shaoline)

(defface shaoline-echo
  '((t :inherit font-lock-string-face))
  "Face for echo messages — the voice of the system."
  :group 'shaoline)

;; Segment-specific faces (moved from compatibility layer)
(defface shaoline-buffer-face
  '((t :inherit shaoline-yang))
  "Face for buffer name display."
  :group 'shaoline)

(defface shaoline-mode-face
  '((t :inherit shaoline-yin))
  "Face for mode information display."
  :group 'shaoline)

(defface shaoline-modified-face
  '((t :inherit warning))
  "Face for modification indicators."
  :group 'shaoline)

(defface shaoline-git-face
  '((t :inherit font-lock-variable-name-face))
  "Face for git information."
  :group 'shaoline)

(defface shaoline-project-face
  '((t :inherit font-lock-function-name-face))
  "Face for project name."
  :group 'shaoline)

(defface shaoline-time-face
  '((t :inherit shaoline-yin))
  "Face for time display."
  :group 'shaoline)

(defface shaoline-date-face
  '((t :inherit shaoline-yin))
  "Face for date display."
  :group 'shaoline)

(defface shaoline-battery-face
  '((t :inherit shaoline-yin))
  "Face for battery information."
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; 五 Segment Registry — Garden of Pure Functions
;; ----------------------------------------------------------------------------

(defvar shaoline--segment-registry (make-hash-table)
  "Registry of available segments.")

;; Compatibility table for old segment system
(defvar shaoline--segment-table (make-hash-table)
  "Segment table for compatibility with old macro system.")

(defmacro shaoline-define-segment (name args docstring &rest body)
  "Register segment NAME as pure function."
  (declare (indent defun))
  `(progn
     (defun ,name ,args ,docstring ,@body)
     (puthash ',name #',name shaoline--segment-registry)
     (puthash ',name #',name shaoline--segment-table) ; Compatibility
     ',name))

(defun shaoline--call-segment (segment-spec)
  "Call SEGMENT-SPEC safely, returning string or empty."
  (condition-case err
      (let* ((spec (if (consp segment-spec) segment-spec (list segment-spec)))
             (fn (car spec))
             (args (cdr spec)))
        (if (functionp fn)
            (let ((result (apply fn args)))
              (if (stringp result) result ""))
          ""))
    (error "")))

;; ----------------------------------------------------------------------------
;; 六 Core Composition — Wu Wei in Action
;; ----------------------------------------------------------------------------

(defun shaoline--collect-side (side)
  "Collect all segments for SIDE (:left, :center, :right)."
  (let* ((segments (cdr (assoc side shaoline-segments)))
         (results (mapcar #'shaoline--call-segment
                          (or segments
                              (plist-get shaoline-segments side)
                              '()))))
    (shaoline--log "shaoline--collect-side %s in buffer: %s: %S" side (buffer-name) results)
    results))

(defun shaoline--calculate-layout (left center right width)
  "Calculate optimal layout for segments given total WIDTH.
Returns (left-str center-str right-str) as pure function."
  (let* ((left-str (string-join (remove "" left) " "))
         (right-str (string-join (remove "" right) " "))
         (center-str (string-join (remove "" center) " "))
         (left-w (string-width left-str))
         (right-w (string-width right-str))
         (available (max 0 (- width left-w right-w 2))) ; 2 for spacing
         (truncated-center
          (if (> (string-width center-str) available)
              (truncate-string-to-width center-str available nil nil "…")
            center-str)))
    (list left-str truncated-center right-str)))

(defun shaoline--compose-line (left center right width)
  "Compose final modeline string with perfect right alignment."
  (let* ((layout (shaoline--calculate-layout left center right width))
         (left-str (nth 0 layout))
         (center-str (nth 1 layout))
         (right-str (nth 2 layout))
         (left-w (string-width left-str))
         (right-w (string-width right-str))
         (gap-left (if (string-empty-p left-str) "" " "))
         (gap-right (if (string-empty-p right-str) "" " "))
         (result
          (concat
           left-str
           gap-left
           center-str
           (when (not (string-empty-p right-str))
             (propertize " " 'display
                         `(space :align-to (- right ,(+ right-w shaoline-right-margin)))))
           right-str)))
    ;; Store rendered segment widths for shaoline-available-center-width
    (shaoline--state-put :last-left-width left-w)
    (shaoline--state-put :last-right-width right-w)
    (shaoline--log "shaoline--compose-line result in buffer: %s: %S" (buffer-name) result)
    result))

(defun shaoline-compose (&optional width)
  "纯 Pure composition function — the heart of Shaoline.
Takes no global state, produces modeline string."
  (let* ((target-width (or width (frame-width) 80))
         (left (shaoline--collect-side :left))
         (center (shaoline--collect-side :center))
         (right (shaoline--collect-side :right)))
    (shaoline--compose-line left center right target-width)))

;; ----------------------------------------------------------------------------
;; 七 Basic Segments — Essential Building Blocks
;; ----------------------------------------------------------------------------

(shaoline-define-segment shaoline-segment-buffer-name ()
  "Buffer name with natural coloring."
  (let ((name (buffer-name)))
    (shaoline--log "shaoline-segment-buffer-name called in buffer: %s, value: %s" name name)
    (propertize name 'face 'shaoline-buffer-face)))

(shaoline-define-segment shaoline-segment-modified ()
  "Modified indicator — simple asterisk."
  (when (and (buffer-modified-p) (buffer-file-name))
    (propertize "*" 'face 'shaoline-modified-face)))

(shaoline-define-segment shaoline-segment-position ()
  "Current position as line:column."
  (propertize (format "%d:%d" (line-number-at-pos) (current-column))
              'face 'shaoline-mode-face))

(shaoline-define-segment shaoline-segment-echo-message ()
  "Current captured message, excluding Shaoline's own content."
  (when-let ((msg (shaoline-msg-current)))
    (unless (get-text-property 0 'shaoline-origin msg)
      (propertize msg 'face 'shaoline-echo))))

(shaoline-define-segment shaoline-segment-time ()
  "Simple time display."
  (when shaoline-enable-dynamic-segments
    (propertize (format-time-string "%H:%M") 'face 'shaoline-time-face)))

;; ----------------------------------------------------------------------------
;; 八 Cache System — Intelligent Non-Action
;; ----------------------------------------------------------------------------

(defun shaoline--cache-key (&rest elements)
  "Generate cache key from ELEMENTS."
  (mapconcat (lambda (e) (format "%s" e)) elements "|"))

(defun shaoline--cached-call (key ttl-seconds thunk)
  "Call THUNK with caching — avoid unnecessary work."
  (let* ((cache (shaoline--state-get :cache))
         (entry (gethash key cache))
         (now (float-time)))
    (if (and entry (< (- now (cdr entry)) ttl-seconds))
        (car entry)  ; Return cached
      (let ((result (funcall thunk)))
        ;; Only cache non-empty results
        (when (and result (not (and (stringp result) (string-empty-p result))))
          (puthash key (cons result now) cache))
        result))))

;; ----------------------------------------------------------------------------
;; 九 Content Deduplication — Avoiding Unnecessary Display
;; ----------------------------------------------------------------------------

(defun shaoline--content-changed-p (new-content)
  "Check if NEW-CONTENT actually differs from last display."
  (let ((last (shaoline--state-get :last-content)))
    (not (string-equal new-content last))))

(defun shaoline--should-display-p (content)
  "Return non-nil when Shaoline should (re)display CONTENT.

In always-visible mode, overrides foreign messages even when content unchanged."
  (and content
       (stringp content)
       (not (string-empty-p content))
       (or (shaoline--content-changed-p content)
           (and (shaoline--resolve-setting 'always-visible)
                (let ((cur (current-message)))
                  (or (null cur)
                      (not (get-text-property 0 'shaoline-origin cur))))))
       (not (shaoline--echo-area-busy-p))))

;; ----------------------------------------------------------------------------
;; 十 Strategy Protocol — Adaptation Without Force
;; ----------------------------------------------------------------------------

(defvar shaoline--strategies
  '((yin    . ((update-method . manual)
               (use-hooks     . nil)
               (use-advice    . nil)
               (use-timers    . nil)
               (always-visible . nil)
               (hide-modelines . nil)))
    (yang   . ((update-method . automatic)
               (use-hooks     . t)
               (use-advice    . t)
               (use-timers    . t)
               (always-visible . t)
               (hide-modelines . t)))
    (adaptive . ((update-method . context)
                 (use-hooks     . adaptive)
                 (use-advice    . adaptive)
                 (use-timers    . adaptive)
                 (always-visible . adaptive)
                 (hide-modelines . adaptive))))
  "Strategy definitions for different operational modes.")

(defun shaoline--get-strategy-setting (setting)
  "Get SETTING from current strategy."
  (let* ((strategy (or (shaoline--state-get :strategy)
                       shaoline-mode-strategy))
         (config (cdr (assq strategy shaoline--strategies))))
    (cdr (assq setting config))))

(defun shaoline--adaptive-decision (setting)
  "Make adaptive decision for SETTING based on context."
  (pcase setting
    ('use-hooks (not (file-remote-p default-directory))) ; No hooks for remote files
    ('use-advice (display-graphic-p))          ; Advice in GUI only
    ('use-timers (< (buffer-size) 100000))     ; Timers for small buffers
    ('always-visible nil)
    ('hide-modelines shaoline-hide-modeline)
    (_ nil)))

(defun shaoline--resolve-setting (setting)
  "Resolve SETTING value, handling adaptive mode."
  (let ((value (shaoline--get-strategy-setting setting)))
    (if (eq value 'adaptive)
        (shaoline--adaptive-decision setting)
      value)))

;; ----------------------------------------------------------------------------
;; 十一 Utility Functions — Helper Wisdom
;; ----------------------------------------------------------------------------

;; Echo-area utilisation -------------------------------------------------------

(defun shaoline--echo-area-busy-p ()
  "Return non-nil when the echo area is currently reserved for
user input.

This covers both real minibuffer interaction and incremental
search (`isearch-mode'), the latter of which uses the echo area
without activating a minibuffer window."
  (or (minibufferp)
      (active-minibuffer-window)
      (and (boundp 'isearch-mode) isearch-mode)))

;; Legacy function bridge
(defun shaoline--update ()
  "Legacy update function bridge."
  (when (fboundp 'shaoline-update)
    (shaoline-update)))

;; Public initialization function (useful feature from legacy)
;;;###autoload
(defun shaoline-initialize ()
  "Initialize Shaoline mode — convenience function."
  (interactive)
  (shaoline-mode 1))

(defvar shaoline--log-max-lines 1000
  "Maximum number of lines kept in *shaoline-logs* buffer.")

(defun shaoline--log (fmt &rest args)
  "If `shaoline-debug' is non-nil, log a debug message to *shaoline-logs* buffer."
  (when shaoline-debug
    (let* ((buf (get-buffer-create "*shaoline-logs*"))
           (ts (format-time-string "[%Y-%m-%d %H:%M:%S] "))
           (msg (apply #'format fmt args)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert ts msg "\n")
          ;; Trim old lines if too long:
          (when (> (count-lines (point-min) (point-max)) shaoline--log-max-lines)
            (goto-char (point-min))
            (forward-line (- (count-lines (point-min) (point-max)) shaoline--log-max-lines))
            (delete-region (point-min) (point))))))))

;;;###autoload
(defun shaoline-show-logs ()
  "Display the Shaoline debug log buffer."
  (interactive)
  (let ((buf (get-buffer-create "*shaoline-logs*")))
    (with-current-buffer buf
      (read-only-mode 1)
      (goto-char (point-max)))
    (pop-to-buffer buf)))

(provide 'shaoline)
;;; shaoline.el ends here
