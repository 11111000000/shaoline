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
(require 'exwm nil :noerror)            ; Optional, only when EXWM is present

;; ----------------------------------------------------------------------------
;; 一 Fundamental Variables — The Unchanging Essence
;; ----------------------------------------------------------------------------

(defgroup shaoline nil
  "Functional minimalist echo-area modeline following the Dao."
  :group 'convenience
  :prefix "shaoline-")

;; Global mode variable
(defvar shaoline-mode nil
  "Non-nil when Shaoline mode is active.")

;; Core behavior flags
(defcustom shaoline-mode-strategy 'yang
  "Strategy for Shaoline behavior adaptation.
- adaptive: Automatically choose between yin/yang based on context
- yin: Passive mode (pure, manual updates only, no hooks/advice)
- yang: Active mode (always visible, hooks, advice)"
  :type '(choice (const adaptive) (const yin) (const yang))
  :group 'shaoline)

(defcustom shaoline-segments
  '((:left
     shaoline-segment-major-mode-icon
     shaoline-segment-buffer-name
     shaoline-segment-modified)
    (:center shaoline-segment-echo-message)
    (:right
     shaoline-segment-input-method
     shaoline-segment-current-keys
     shaoline-segment-position
     shaoline-segment-project-name
     shaoline-segment-git-branch
     shaoline-segment-battery
     shaoline-segment-time
     shaoline-segment-moon-phase
     ))
  "Segment configuration following the Three Treasures pattern.
Structure: ((:left segment ...) (:center segment ...) (:right segment ...))"
  :type 'sexp
  :group 'shaoline)

;; Performance wisdom
(defcustom shaoline-update-debounce 0.25
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

(defcustom shaoline-right-margin 1
  "Fixed right margin in characters for right-aligned segments.
When `shaoline-with-tray' is non-nil this value is overridden
dynamically so you normally do not need to change it by hand."
  :type 'integer
  :group 'shaoline)

(defcustom shaoline-with-tray t
  "Non-nil means automatically align Shaoline with the EXWM system tray.

When enabled Shaoline measures the tray frame’s pixel width,
converts it to character cells for the current frame and
temporarily sets `shaoline-right-margin' to that value each time
the line is composed."
  :type 'boolean
  :group 'shaoline)

(defun shaoline--exwm-tray-char-width ()
  "Return EXWM system-tray width in *character cells* for the current frame.

Algorithm order (first that yields a sensible number wins):
1.  Look for a frame with the parameter `exwm-tray`.
2.  Use `exwm-systemtray-width` (pixels) maintained by EXWM.
3.  Otherwise return nil.

When `shaoline-debug` is non-nil every step is logged to *shaoline-logs*."
  (when (and shaoline-with-tray (featurep 'exwm))
    (let ((charw (frame-char-width))
          pixels)
      ;; 1. Try dedicated tray frame
      (when-let ((tray-frame
                  (cl-find-if (lambda (f) (frame-parameter f 'exwm-tray))
                              (frame-list))))
        (setq pixels (frame-pixel-width tray-frame))
        (shaoline--log "tray-dbg: found tray-frame %S, width=%s px" tray-frame pixels))
      ;; 2. Fall back to variable set by exwm-systemtray.el
      (when (and (not pixels)
                 (boundp 'exwm-systemtray-width)
                 (numberp exwm-systemtray-width)
                 (> exwm-systemtray-width 0))
        (setq pixels exwm-systemtray-width)
        (shaoline--log "tray-dbg: using exwm-systemtray-width=%s px" pixels))
      ;; 3. Convert to characters
      (when (and pixels charw (> charw 0))
        (let ((chars (ceiling (/ pixels (float charw)))))
          (shaoline--log "tray-dbg: %s px ≈ %s chars (char-width=%s)"
                         pixels chars charw)
          chars)))))

(defun shaoline--refresh-right-margin ()
  "Recompute `shaoline-right-margin' when `shaoline-with-tray' is enabled.

Adds verbose logging when `shaoline-debug' is non-nil."
  (when shaoline-with-tray
    (let ((old shaoline-right-margin)
          (w   (shaoline--exwm-tray-char-width)))
      (when (and w (> w 0))
        (setq shaoline-right-margin w)
        (shaoline--log "tray-dbg: right-margin changed %s → %s"
                       old shaoline-right-margin)))))

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
  '((((type tty))
     :inherit default
     :foreground "#00aa00"
     :background unspecified)
    (t
     :inherit default
     :height 1.0
     :bold nil
     :family "Digital Display"
     :foreground "#00aa00"
     :background "#002200"))
  "Face for the time segment, adapting to the current theme at load time.
TTY uses just green foreground; GUI uses green on dark background.
For full dynamic adaptation, reload after theme changes."
  :group 'shaoline)

(defface shaoline-date-face
  '((t :inherit shaoline-yin))
  "Face for date display."
  :group 'shaoline)

(defface shaoline-battery-face
  '((t :inherit shaoline-yin))
  "Face for battery information."
  :group 'shaoline)

;; --------------------------------------------------------------------------
;; Battery faces — traffic-light semantics
;; --------------------------------------------------------------------------

(defface shaoline-current-keys-face
  '((t :inherit font-lock-type-face))
  "Face for project name."
  :group 'shaoline)

(defface shaoline-battery-critical-face
  '((t :inherit shaoline-yin :foreground "#f28b82"))
  "Battery critically low face."
  :group 'shaoline)

(defface shaoline-battery-charging-face
  '((t :inherit shaoline-yin :foreground "#81c784"))
  "Battery charging / AC-connected face."
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

(defun shaoline--segment-enabled-p (segment)
  "Return non-nil if SEGMENT (symbol) is present in `shaoline-segments` config."
  (let* ((fetch (lambda (side)
                  (or (cdr (assoc side shaoline-segments))
                      (plist-get shaoline-segments side))))
         (all (append (funcall fetch :left)
                      (funcall fetch :center)
                      (funcall fetch :right))))
    (cl-some (lambda (x) (eq (if (consp x) (car x) x) segment)) all)))

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
         ;; Account for gaps and right margin more precisely
         (left-gap (if (string-empty-p left-str) 0 1))
         (right-gap (if (string-empty-p right-str) 0 1))
         (reserved-space (+ left-w right-w left-gap right-gap shaoline-right-margin))
         (available (max 0 (- width reserved-space)))
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
  ;; Re-calculate tray width if needed
  (shaoline--refresh-right-margin)
  (let* ((target-width (or width (frame-width) 80))
         (left (shaoline--collect-side :left))
         (center (shaoline--collect-side :center))
         (right (shaoline--collect-side :right)))
    (shaoline--compose-line left center right target-width)))

;; ----------------------------------------------------------------------------
;; 八 Cache System — Intelligent Non-Action
;; ----------------------------------------------------------------------------

(defun shaoline--cache-key (&rest elements)
  "Generate cache key from ELEMENTS."
  (mapconcat (lambda (e) (format "%s" e)) elements "|"))

(defun shaoline--cached-call (key ttl-seconds thunk)
  "智能缓存 Intelligent caching with strict TTL (no sliding refresh)."
  (let* ((cache (shaoline--state-get :cache))
         (entry (gethash key cache))
         (now (float-time)))
    (if (and entry (< (- now (cdr entry)) ttl-seconds))
        ;; Cache hit — do NOT extend TTL; expire naturally
        (car entry)
      ;; Cache miss or expired — compute and store
      (let ((result (funcall thunk)))
        (when (and result (not (and (stringp result) (string-empty-p result))))
          (puthash key (cons result now) cache))
        result))))

(defvar shaoline--stable-segment-cache (make-hash-table :test 'equal)
  "Cache for segments that change rarely (mode, project, etc).")

(defun shaoline--stable-cached-call (key dependencies thunk)
  "Cache stable segments until DEPENDENCIES change."
  (let* ((cache-key (format "%s:%s" key (sxhash dependencies)))
         (entry (gethash cache-key shaoline--stable-segment-cache)))
    (or entry
        (let ((result (funcall thunk)))
          (when result
            ;; Clear old entries for this key
            (maphash (lambda (k v)
                       (when (string-prefix-p (format "%s:" key) k)
                         (remhash k shaoline--stable-segment-cache)))
                     shaoline--stable-segment-cache)
            ;; Store new result
            (puthash cache-key result shaoline--stable-segment-cache))
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

• Respects echo-area busy / yield rules.
• In yang (always-visible) mode we *force* re-display whenever the echo
  area has lost our line, even if CONTENT itself has not changed."
  (and content
       (stringp content)
       (not (string-empty-p content))
       ;; 1.  Never interfere while echo-area must be yielded
       (not (shaoline--should-yield-echo-area-p))
       ;; 2.  Decide whether to display
       (let* ((always (shaoline--resolve-setting 'always-visible))
              (echo  (current-message)))
         (or
          ;; 2a. Usual triggers – new or empty echo-area
          (shaoline--content-changed-p content)
          (null echo)
          ;; 2b. Yang re-assertion – echo-area lost our property
          (and always
               (shaoline--echo-area-stable-p)
               (or (null echo)
                   (not (get-text-property 0 'shaoline-origin echo))))))))

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
  "Return non-nil when echo-area сейчас занята пользовательским вводом.

Без перечисления команд и regex; полагаемся на универсальные
системные индикаторы:

  • active minibuffer               → mini-buffer владеет областью
  • cursor-in-echo-area             → курсор мигает в echo-area
  • shaoline--echo-area-input-depth → мы внутри `read-event', запущенного
                                       из echo-area (см. advice)
  • isearch-mode                    → инкрементальный поиск занимает echo-area"
  (or (minibufferp)
      (active-minibuffer-window)
      cursor-in-echo-area
      (> shaoline--echo-area-input-depth 0)
      (and (boundp 'isearch-mode) isearch-mode)))


;; Input-sensitive command detection
(defvar shaoline--input-sensitive-commands
  '(eval-expression shell-command save-buffers-kill-emacs
                    find-file-read-only find-alternate-file
                    delete-file rename-file copy-file
                    compile grep occur
                    query-replace-regexp query-replace
                    read-regexp read-string
                    dired-do-rename dired-do-copy dired-do-delete
                    bookmark-jump bookmark-set
                    kill-buffer kill-this-buffer kill-terminal
                    delete-window delete-other-windows
                    quit-window bury-buffer
                    server-edit server-done
                    magit-commit magit-push magit-pull)
  "Commands that might prompt for user input.")

(defvar shaoline--last-busy-time 0
  "Time when echo-area was last detected as busy.")

(defvar shaoline--echo-area-input-depth 0
  "Depth counter: >0 while `read-event' waits in echo-area.

Incremented by an around-advice on `read-event' whenever
`cursor-in-echo-area' is non-nil, and decremented afterwards.  A
universal, future-proof indicator that Emacs действительно ждёт
ввод в echo-area (y-or-n-p, read-char, query-replace, и т. д.).")

(defvar shaoline--echo-area-stable-delay 0.3
  "Seconds to wait after echo-area becomes free before reclaiming.")

(defun shaoline--echo-area-stable-p ()
  "Check if echo-area has been stable (not busy) for enough time."
  (let ((now (float-time)))
    (if (shaoline--echo-area-busy-p)
        (progn
          (setq shaoline--last-busy-time now)
          nil)
      ;; Стабильна достаточно долго
      (> (- now shaoline--last-busy-time) shaoline--echo-area-stable-delay))))

(defun shaoline--should-yield-echo-area-p ()
  "Comprehensive logic for when Shaoline should yield echo-area."
  (or
   ;; Базовые состояния ввода
   (shaoline--echo-area-busy-p)

   ;; Команды, которые могут запросить ввод
   (memq this-command shaoline--input-sensitive-commands)

   ;; Следующая команда может быть интерактивной
   (memq last-command shaoline--input-sensitive-commands)))

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
