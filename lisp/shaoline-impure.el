;;; shaoline-impure.el --- Impure infrastructure for Shaoline echo-area modeline -*- lexical-binding: t; -*-

;; Version: 2.2.3

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; This file combines the impure infrastructure (advices, hooks, timers, minor mode)
;; from the original shaoline-infra.el and shaoline-events.el for simplicity.
;; All side effects are gathered here, keeping the core pure.

;; Infra requires core (shaoline.el) for variables/faces.
(require 'shaoline)  ;; Ensure core is loaded so shaoline--log etc. are always available

(eval-when-compile
  (defvar shaoline-autohide-modeline)
  (defvar shaoline-exclude-modes))

(require 'cl-lib)                    ; for cl-member etc.

;; ----------------------------------------------------------------------------
;; Helpers for hiding the classic mode-line.

(defun shaoline--set-modeline-format-globally (value &optional backup-restore)
  "Set `mode-line-format' to VALUE for all buffers and windows.
If BACKUP-RESTORE is non-nil, take or restore backup of the default setting."
  (when backup-restore
    (if (eq value :default)
        (when shaoline--default-mode-line-format-backup
          (setq-default mode-line-format shaoline--default-mode-line-format-backup)
          (setq shaoline--default-mode-line-format-backup nil))
      (unless shaoline--default-mode-line-format-backup
        (setq shaoline--default-mode-line-format-backup (default-value 'mode-line-format)))))
  (let ((final (if (eq value :default)
                   (default-value 'mode-line-format)
                 value)))
    (setq-default mode-line-format final)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq-local mode-line-format final))))
  (force-mode-line-update t))

(defun shaoline--autohide-modeline-globally ()
  "Hide the classic mode-line in all current and future buffers,
EXCEPT in modes listed in `shaoline-exclude-modes`.

The current value of `mode-line-format` is stored buffer-locally as `shaoline--saved-mode-line-format`,
allowing exact restoration when `shaoline-mode` is disabled."
  (unless shaoline--default-mode-line-format-backup
    (setq shaoline--default-mode-line-format-backup
          (default-value 'mode-line-format)))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (unless (or (memq major-mode shaoline-exclude-modes)
                  (local-variable-p 'shaoline--saved-mode-line-format))
        (setq-local shaoline--saved-mode-line-format mode-line-format)
        (setq-local mode-line-format nil))))
  (force-mode-line-update t))

(defun shaoline--unhide-modeline-globally ()
  "Restore the classic mode-line in every buffer.

The value saved in `shaoline--saved-mode-line-format` is restored and the marker removed.
If a buffer's mode-line-format was not changed by Shaoline, it is left untouched."
  (when shaoline--default-mode-line-format-backup
    (setq-default mode-line-format shaoline--default-mode-line-format-backup)
    (setq shaoline--default-mode-line-format-backup nil))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (local-variable-p 'shaoline--saved-mode-line-format)
        (setq-local mode-line-format shaoline--saved-mode-line-format)
        (kill-local-variable 'shaoline--saved-mode-line-format))))
  (force-mode-line-update t))

;; ----------------------------------------------------------------------------
;; Display and update.

(defun shaoline--display (str)
  "Show STR in the echo area and tag it so only Shaoline output is affected.
Re-display whenever the echo area is

  • empty,
  • shows a *non-Shaoline* message,
  • or when STR itself changed.

This guarantees Shaoline immediately re-claims the echo area after any
foreign output (keypress echos, «Mark set», timers, etc.)."
  (let ((cur-msg (current-message)))
    (when (or (null cur-msg)                               ;; echo area empty
              (not (get-text-property 0 'shaoline cur-msg)) ;; foreign message
              (not (string-equal str shaoline--last-str)))  ;; text changed
      ;; Remember *unmodified* string for width-comparison / clearing,
      ;; but send the *tagged* one to `message'.
      (setq shaoline--last-str str)
      (let* ((msg-with-prop (propertize str 'shaoline t))
             (message-log-max nil))
        (message "%s" msg-with-prop)
        (run-with-idle-timer shaoline-guard-delay nil #'shaoline--ensure-visible)))))

(defun shaoline--ensure-visible ()
  "Re-draw Shaoline if echo area is blank while `shaoline-always-visible' is active."
  (when (and shaoline-mode
             shaoline-always-visible
             (not (active-minibuffer-window))
             (not (minibufferp))
             (not (bound-and-true-p isearch-mode))
             (let ((cur (current-message)))
               (or (null cur) (string-empty-p cur))))
    (shaoline--update)))

(defun shaoline--predictive-clear ()
  "Predictive clear before commands that may activate minibuffer."
  (when (and (commandp this-command)
             (memq this-command '(execute-extended-command find-file)))
    (shaoline--clear-display)))

(defvar shaoline--update-call-count 0 "Number of times shaoline--update was called across session.")

(defun shaoline--update (&rest _)
  "Recompute and display modeline for the currently selected window.
Skips update and clears during isearch or minibuffer input."
  (cl-incf shaoline--update-call-count)
  (shaoline--log "shaoline--update (buffer=%S major-mode=%S point=%d win=%S)"
                 (current-buffer)
                 major-mode
                 (point)
                 (selected-window))
  (if (or (active-minibuffer-window)
          (minibufferp)
          (bound-and-true-p isearch-mode))
      (shaoline--clear-display)
    (let ((cur-msg (current-message))
          (win (selected-window)))
      (unless (and (not shaoline-always-visible)
                   cur-msg
                   (not (get-text-property 0 'shaoline cur-msg)))
        ;; Выполняем рендер в контексте именно выбранного окна,
        ;; чтобы (current-buffer), (point) и прочие обращались
        ;; к правильным данным, даже внутри таймера.
        (with-selected-window win
          (shaoline--display
           (shaoline-compose-modeline
            (current-buffer)
            win)))))))

(defun shaoline--clear-display ()
  "Clear the echo area if the last output was from Shaoline.
Never clears if already empty or a suppression is in effect."
  (when (and (stringp shaoline--last-str)
             (string= (current-message) shaoline--last-str))
    ;; call only if really non-empty, or else never clear
    (unless (or (not (current-message))
                (string-empty-p (current-message)))
      (message nil)
      (when shaoline-always-visible
        (shaoline--display (shaoline-compose-modeline)))))
  (setq shaoline--last-str ""))

;; ----------------------------------------------------------------------------
;; Message advice and echo-area message capture infrastructure.

(defun shaoline--capture-message-args (args)
  "FILTER-ARGS advice for =message'.
Save the would-be message *before* it reaches the echo area."
  (let* ((fmt (car args))
         (rest (cdr args))
         (str (and fmt (apply #'format fmt rest))))
    (when (and str
               (not (string-empty-p str))
               (not (get-text-property 0 'shaoline str)))
      (shaoline-msg-save (copy-sequence str)))
    args))

(defun shaoline--capture-message-ret (ret)
  "FILTER-RETURN advice for `message'.
Save the *actual* message shown, then force Shaoline redraw."
  (when (and (stringp ret)
             (not (string-empty-p ret))
             (not (get-text-property 0 'shaoline ret)))
    (shaoline-msg-save (copy-sequence ret)))
  ;; ensure Shaoline immediately re-appears
  (shaoline--update)
  ret)

(defun shaoline--around-display-warning (orig type msg &rest rest)
  "Around advice for `display-warning' that captures MSG."
  (shaoline-msg-save (copy-sequence msg))
  (apply orig type msg rest))

(defun shaoline--around-minibuffer-message (orig fmt &rest rest)
  "Around advice for `minibuffer-message' that captures its string."
  (let ((str (apply #'format fmt rest)))
    (shaoline-msg-save (copy-sequence str))
    (apply orig fmt rest)))

(defvar shaoline--debounce-timer nil
  "Debounce timer for Shaoline updates.")

(defun shaoline--debounced-update (&rest _)
  "Debounce wrapper: schedule update after a short delay.
If called repeatedly, only update after 0.12s delay."
  (when (timerp shaoline--debounce-timer)
    (cancel-timer shaoline--debounce-timer))
  (setq shaoline--debounce-timer
        (run-with-timer 0.12 nil #'shaoline--update)))

(defvar shaoline--timer nil
  "Timer for slow periodic updates, if any.")

;;;;
;; Dynamic Segments Declaration
;;
;; To avoid hardcoding, we define a list of segments that require periodic updates (e.g., time, battery, moon-phase).
;; Users can customize this if they add new dynamic segments.

(defcustom shaoline-dynamic-segments
  '(shaoline-segment-digital-clock
    shaoline-segment-time  ; deprecated alias
    shaoline-segment-battery
    shaoline-segment-moon-phase
    shaoline-segment-day-date)
  "List of dynamic segments that require periodic timer updates (e.g., time/battery).
Customize this if you add new time-sensitive segments."
  :type '(repeat symbol)
  :group 'shaoline)

(defun shaoline--has-dynamic-segments ()
  "Return non-nil if any dynamic segment is present in any position (:left, :center, :right)."
  (let ((all-segs (append (cdr (assq :left shaoline-segments))
                          (cdr (assq :center shaoline-segments))
                          (cdr (assq :right shaoline-segments)))))
    (cl-some (lambda (seg)
               (if (consp seg) (memq (car seg) shaoline-dynamic-segments)
                 (memq seg shaoline-dynamic-segments)))
             all-segs)))

(defun shaoline--maybe-start-timer ()
  "Start timer only if dynamic segments are enabled and any are present in the segments list."
  (when (and (null shaoline--timer)
             shaoline-mode
             shaoline-enable-dynamic-segments
             (shaoline--has-dynamic-segments))
    (setq shaoline--timer
          (run-with-timer shaoline-timer-interval
                          shaoline-timer-interval
                          #'shaoline--lazy-update))))

(defun shaoline--maybe-cancel-timer ()
  "Cancel timer if it is not needed (no dynamic segments present)."
  (when (timerp shaoline--timer)
    (cancel-timer shaoline--timer)
    (setq shaoline--timer nil)))

(defun shaoline--lazy-update ()
  "Update Shaoline and disable timer if no dynamic segments are needed anymore."
  (shaoline--log "shaoline--lazy-update")
  (let ((should-keep
         (and shaoline-enable-dynamic-segments
              (shaoline--has-dynamic-segments))))
    (shaoline--update)
    (unless should-keep
      (shaoline--maybe-cancel-timer))))

(defun shaoline--delayed-update ()
  "Schedule a slightly delayed update to prevent blinking or disappearing after minibuffer/isearch."
  (run-with-idle-timer 0.1 nil #'shaoline--update))

(defun shaoline-events-enable ()
  "Enable all Shaoline global hooks, advices and timers for impure infrastructure."
  ;; ------------------------------------------------------------------
  ;; DEBUG
  ;; Показываем в логах факт активации и состояние ключевых флагов.
  (shaoline--log
   "shaoline-events-enable called (attach-hooks=%s attach-advices=%s enable-dynamic=%s enable-hooks=%s shaoline-mode=%s)"
   shaoline-attach-hooks shaoline-attach-advices shaoline-enable-dynamic-segments shaoline-enable-hooks shaoline-mode)
  (shaoline--log "  shaoline-update-hooks=%S" shaoline-update-hooks)
  (shaoline--log "  All hook values now: post-command-hook=%S find-file-hook=%S after-save-hook=%S"
                 (and (boundp 'post-command-hook) post-command-hook)
                 (and (boundp 'find-file-hook) find-file-hook)
                 (and (boundp 'after-save-hook) after-save-hook))
  (shaoline--log "  enable-hooks=%S attach-hooks=%S always-visible=%S" shaoline-enable-hooks shaoline-attach-hooks shaoline-always-visible)

  ;; Message advice (only if always-visible *and* advices enabled).
  (when (and shaoline-attach-advices shaoline-always-visible)
    (shaoline--log "Attaching advices")
    (advice-add #'message            :filter-args   #'shaoline--capture-message-args)
    (advice-add #'message            :filter-return #'shaoline--capture-message-ret)
    (advice-add #'display-warning    :around        #'shaoline--around-display-warning)
    (advice-add #'minibuffer-message :around        #'shaoline--around-minibuffer-message))
  ;; Update hooks (with/without debounce) — only if enabled and hooks allowed.
  (when (and shaoline-enable-hooks shaoline-attach-hooks)
    (shaoline--log "XX DEBUG: shaoline-update-hooks=%S, shaoline-enable-hooks=%S, shaoline-attach-hooks=%S" shaoline-update-hooks shaoline-enable-hooks shaoline-attach-hooks)
    (shaoline--log "Attaching update hooks: %S" shaoline-update-hooks)
    (dolist (hook shaoline-update-hooks)
      (shaoline--log "XX DEBUG: Preparing to add-hook for %S" hook)
      (let* ((fn (if (eq hook 'post-command-hook)
                     #'shaoline--update
                   #'shaoline--debounced-update))
             (wrapper
              (let ((h hook)
                    (f fn))
                (lambda (&rest args)
                  (shaoline--log "HOOK FIRED: %s -> %s (args=%S)" h f args)
                  (apply f args)))))
        (let ((wrapsym (intern (format "shaoline--hook-wrapper-%s" (symbol-name hook)))))
          (shaoline--log "XX DEBUG: fset + add-hook wrapper %S (calls %S)" wrapsym fn)
          (fset wrapsym wrapper)
          (add-hook hook wrapper)
          (shaoline--log "  add-hook %s wrapper=%s (calls=%s)" hook wrapsym fn)
          (shaoline--log "XX DEBUG: post add-hook %S: %S" hook (and (boundp hook) (symbol-value hook)))
          (shaoline--log "  CHECK after add-hook: %s contains wrapper? %s"
                         hook (memq wrapper (and (boundp hook) (symbol-value hook)))))))
    ;; Pre-minibuffer and isearch hooks to clear/restore display
    (add-hook 'minibuffer-setup-hook    #'shaoline--clear-display)    (shaoline--log "  add-hook minibuffer-setup-hook shaoline--clear-display")
    (add-hook 'minibuffer-exit-hook     #'shaoline--delayed-update)   (shaoline--log "  add-hook minibuffer-exit-hook shaoline--delayed-update")
    (add-hook 'isearch-mode-hook        #'shaoline--clear-display)    (shaoline--log "  add-hook isearch-mode-hook shaoline--clear-display")
    (add-hook 'isearch-mode-end-hook    #'shaoline--delayed-update)   (shaoline--log "  add-hook isearch-mode-end-hook shaoline--delayed-update")
    (add-hook 'pre-command-hook         #'shaoline--predictive-clear) (shaoline--log "  add-hook pre-command-hook shaoline--predictive-clear")
    (add-hook 'focus-in-hook            #'shaoline--debounced-update) (shaoline--log "  add-hook focus-in-hook shaoline--debounced-update"))
  ;; Start periodic timer if needed
  (when shaoline-enable-dynamic-segments
    (shaoline--log "shaoline-enable-dynamic-segments=%s, calling shaoline--maybe-start-timer" shaoline-enable-dynamic-segments)
    (shaoline--maybe-start-timer)))

(defun shaoline-events-disable ()
  "Disable all Shaoline global hooks, advices and timers for impure infrastructure."
  (shaoline--log "shaoline-events-disable called")
  ;; Advice always removed (if advices were enabled).
  (when shaoline-attach-advices
    (shaoline--log "Removing advices")
    (advice-remove #'message            #'shaoline--capture-message-args)
    (advice-remove #'message            #'shaoline--capture-message-ret)
    (advice-remove #'display-warning    #'shaoline--around-display-warning)
    (advice-remove #'minibuffer-message #'shaoline--around-minibuffer-message))
  ;; Hooks — only if they were enabled and allowed.
  (when shaoline-attach-hooks
    (shaoline--log "Removing hooks")
    (dolist (hook shaoline-update-hooks)
      (let* ((wrapsym (intern (format "shaoline--hook-wrapper-%s" (symbol-name hook)))))
        (when (fboundp wrapsym)
          (remove-hook hook (symbol-function wrapsym))
          (shaoline--log "  remove-hook %s wrapper=%s" hook wrapsym)
          (fmakunbound wrapsym))))
    (remove-hook 'minibuffer-setup-hook    #'shaoline--clear-display)    (shaoline--log "  remove-hook minibuffer-setup-hook shaoline--clear-display")
    (remove-hook 'minibuffer-exit-hook     #'shaoline--delayed-update)   (shaoline--log "  remove-hook minibuffer-exit-hook shaoline--delayed-update")
    (remove-hook 'isearch-mode-hook        #'shaoline--clear-display)    (shaoline--log "  remove-hook isearch-mode-hook shaoline--clear-display")
    (remove-hook 'isearch-mode-end-hook    #'shaoline--delayed-update)   (shaoline--log "  remove-hook isearch-mode-end-hook shaoline--delayed-update")
    (remove-hook 'pre-command-hook         #'shaoline--predictive-clear) (shaoline--log "  remove-hook pre-command-hook shaoline--predictive-clear")
    (remove-hook 'focus-in-hook            #'shaoline--debounced-update) (shaoline--log "  remove-hook focus-in-hook shaoline--debounced-update"))
  (shaoline--maybe-cancel-timer)
  ;; Cancel debounce timer
  (when (timerp shaoline--debounce-timer)
    (shaoline--log "Cancelling debounce timer")
    (cancel-timer shaoline--debounce-timer)
    (setq shaoline--debounce-timer nil))
  ;; Cancel any remaining timers that may still fire after the mode
  ;; has been disabled (e.g. one-off idle timers created earlier).
  (dolist (fn '(shaoline--update
                shaoline--lazy-update
                shaoline--debounced-update
                shaoline--ensure-visible
                shaoline--log--timer-fn))
    (shaoline--log "Cancelling function timers for %s" fn)
    (cancel-function-timers fn)))

(defun shaoline-purge-infra ()
  "Completely remove all Shaoline hooks, advice, and timers regardless of settings.
For emergency/manual cleanup."
  (let ((shaoline-attach-advices t)
        (shaoline-attach-hooks t))
    (shaoline-events-disable)))

;; ----------------------------------------------------------------------------
;; Activate advice and hooks/timers when shaoline-mode toggles.
(add-hook 'shaoline-mode-hook
          (lambda ()
            (shaoline--log "shaoline-mode-hook fired (shaoline-mode=%s)" shaoline-mode)
            (if shaoline-mode
                (progn (shaoline--log "Enabling infra via shaoline-events-enable")
                       (shaoline-events-enable))
              (progn (shaoline--log "Disabling infra via shaoline-events-disable")
                     (shaoline-events-disable)))))

;; ----------------------------------------------------------------------------
;; Shaoline minor mode engine.

;;;###autoload
(define-minor-mode shaoline-mode
  "Global minor mode that displays a functional minimalist modeline in echo-area."
  :global t
  :lighter ""
  (shaoline--log "shaoline-mode toggled: %s" shaoline-mode)
  (if shaoline-mode
      (progn
        (shaoline--log "shaoline-mode enabled")
        (when shaoline-autohide-modeline
          (shaoline--log "Autohiding classic mode-line")
          (shaoline--autohide-modeline-globally))
        (setq shaoline--resize-mini-windows-backup resize-mini-windows)
        (setq resize-mini-windows nil)
        (shaoline--log "Scheduling shaoline--update via idle-timer")
        (run-with-idle-timer 0.1 nil #'shaoline--update))
    (progn
      (shaoline--log "shaoline-mode disabled")
      (shaoline--clear-display)
      (when shaoline-autohide-modeline
        (shaoline--log "Restoring classic mode-line")
        (shaoline--unhide-modeline-globally))
      (when shaoline--resize-mini-windows-backup
        (setq resize-mini-windows shaoline--resize-mini-windows-backup)
        (setq shaoline--resize-mini-windows-backup nil)))))

(provide 'shaoline-impure)
;;; shaoline-impure.el ends here
