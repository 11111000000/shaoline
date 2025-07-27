;;; shaoline-events.el --- Advice and hook orchestration for Shaoline -*- lexical-binding: t; -*-

;; Version: 2.2.2
;;
;; In the spirit of Dao, this file gathers all worldly effects —
;; hooks, advices, timers — guiding them away from the pure core.
;;
;; Infrastructure and core remain untouched by side effects;
;; only this module orchestrates the flow of change.
;;
;; This module must not require shaoline-infra nor introduce cross-dependencies.
;; Its job: attach/detach all hooks, advices, and timers for Shaoline,
;; then dissolve quietly, leaving stillness behind.

;; Load order: Required by infra and core, but no circular deps.

;;; API:

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
  ;; Message advice (only if always-visible *and* advices enabled).
  (when (and shaoline-attach-advices shaoline-always-visible)
    (advice-add #'message            :filter-args   #'shaoline--capture-message-args)
    (advice-add #'message            :filter-return #'shaoline--capture-message-ret)
    (advice-add #'display-warning    :around        #'shaoline--around-display-warning)
    (advice-add #'minibuffer-message :around        #'shaoline--around-minibuffer-message))
  ;; Update hooks (with/without debounce) — only if enabled and hooks allowed.
  (when (and shaoline-enable-hooks shaoline-attach-hooks)
    (dolist (hook shaoline-update-hooks)
      (add-hook hook (if (eq hook 'post-command-hook)
                         #'shaoline--update
                       #'shaoline--debounced-update)))
    ;; Pre-minibuffer and isearch hooks to clear/restore display
    (add-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
    (add-hook 'minibuffer-exit-hook     #'shaoline--delayed-update)
    (add-hook 'isearch-mode-hook        #'shaoline--clear-display)
    (add-hook 'isearch-mode-end-hook    #'shaoline--delayed-update)
    ;; Predictive clear on pre-command (before minibuffer-activating commands)
    (add-hook 'pre-command-hook         #'shaoline--predictive-clear)
    ;; Focus-in for restoring after frame focus changes
    (add-hook 'focus-in-hook            #'shaoline--debounced-update))
  ;; Start periodic timer if needed
  (when shaoline-enable-dynamic-segments
    (shaoline--maybe-start-timer)))

(defun shaoline-events-disable ()
  "Disable all Shaoline global hooks, advices and timers for impure infrastructure."
  ;; Advice always removed (if advices were enabled).
  (when shaoline-attach-advices
    (advice-remove #'message            #'shaoline--capture-message-args)
    (advice-remove #'message            #'shaoline--capture-message-ret)
    (advice-remove #'display-warning    #'shaoline--around-display-warning)
    (advice-remove #'minibuffer-message #'shaoline--around-minibuffer-message))
  ;; Hooks — only if they were enabled and allowed.
  (when shaoline-attach-hooks
    (dolist (hook shaoline-update-hooks)
      (remove-hook hook (if (eq hook 'post-command-hook)
                            #'shaoline--update
                          #'shaoline--debounced-update)))
    (remove-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
    (remove-hook 'minibuffer-exit-hook     #'shaoline--delayed-update)
    (remove-hook 'isearch-mode-hook        #'shaoline--clear-display)
    (remove-hook 'isearch-mode-end-hook    #'shaoline--delayed-update)
    (remove-hook 'pre-command-hook         #'shaoline--predictive-clear)
    (remove-hook 'focus-in-hook            #'shaoline--debounced-update))
  (shaoline--maybe-cancel-timer)
  ;; Cancel debounce timer
  (when (timerp shaoline--debounce-timer)
    (cancel-timer shaoline--debounce-timer)
    (setq shaoline--debounce-timer nil)))

(defun shaoline-purge-infra ()
  "Completely remove all Shaoline hooks, advice, and timers regardless of settings.
For emergency/manual cleanup."
  (let ((shaoline-attach-advices t)
        (shaoline-attach-hooks t))
    (shaoline-events-disable)))

(provide 'shaoline-events)
;;; shaoline-events.el ends here
