;;; shaoline-infra.el --- Infrastructure for Shaoline echo-area modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; This file hosts *impure* infrastructure that complements the pure core.
;; It is loaded *from* shaoline.el, so we purposely do *not* `require' it
;; back to avoid a circular dependency.

(require 'cl-lib)                    ; for cl-member etc.

;; ----------------------------------------------------------------------------
;; Customization: here dwell options with effects.

(defcustom shaoline-timer-interval 1
  "Interval (seconds) for Shaoline’s optional periodic refresh.
Only used when dynamic segments such as time/battery are present."
  :type 'number
  :group 'shaoline)

(defcustom shaoline-message-timeout 10
  "How many seconds a normal `message' remains before Shaoline redraws.
If non-positive, Shaoline repaints immediately after the `message'."
  :type 'number
  :group 'shaoline)

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
  "Show STR in the echo area. Tag the string so only Shaoline output is affected."
  (let ((cur-msg (current-message)))
    (when (or (not (string-equal str shaoline--last-str))
              (null cur-msg))
      ;; Remember *unmodified* string for width-comparison / clearing,
      ;; but send the *tagged* one to `message'.
      (setq shaoline--last-str str)
      (let* ((msg-with-prop (propertize str 'shaoline t))
             (message-log-max nil))
        (message "%s" msg-with-prop)))))

(defun shaoline--update (&rest _)
  "Recompute and display modeline for the currently selected window.
Skips update and clears during isearch or minibuffer input."
  (shaoline--log "shaoline--update")
  (if (or (active-minibuffer-window)
          (bound-and-true-p isearch-mode))
      (shaoline--clear-display)
    (shaoline--display
     (shaoline-compose-modeline))))

(defun shaoline--clear-display ()
  "Clear the echo area if the last output was from Shaoline.
Never clears if already empty or a suppression is in effect."
  (when (and (stringp shaoline--last-str)
             (string= (current-message) shaoline--last-str))
    ;; call only if really non-empty, or else never clear
    (unless (or (not (current-message))
                (string-empty-p (current-message)))
      (message nil)))
  (setq shaoline--last-str ""))

;; ----------------------------------------------------------------------------
;; Shaoline minor mode engine.

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

(defun shaoline--maybe-start-timer ()
  "Start timer only if dynamic segments are enabled and present in :right or :center."
  (when (and (null shaoline--timer)
             shaoline-mode
             shaoline-enable-dynamic-segments
             (or (cl-member 'shaoline-segment-time (cdr (assq :right shaoline-segments)))
                 (cl-member 'shaoline-segment-time (cdr (assq :center shaoline-segments)))
                 (cl-member 'shaoline-segment-battery (cdr (assq :right shaoline-segments)))
                 (cl-member 'shaoline-segment-battery (cdr (assq :center shaoline-segments)))))
    (setq shaoline--timer
          (run-with-timer shaoline-timer-interval
                          shaoline-timer-interval
                          #'shaoline--lazy-update))))

(defun shaoline--maybe-cancel-timer ()
  "Cancel timer if it is not needed."
  (when (timerp shaoline--timer)
    (cancel-timer shaoline--timer)
    (setq shaoline--timer nil)))

(defun shaoline--lazy-update ()
  "Disable timer if nothing dynamic is needed."
  (shaoline--log "shaoline--lazy-update")
  (let ((should-keep
         (and shaoline-enable-dynamic-segments
              (or
               ;; Check if a dynamic segment (time/battery) is present.
               (cl-member 'shaoline-segment-time (cdr (assq :right shaoline-segments)))
               (cl-member 'shaoline-segment-time (cdr (assq :center shaoline-segments)))
               (cl-member 'shaoline-segment-battery (cdr (assq :right shaoline-segments)))
               (cl-member 'shaoline-segment-battery (cdr (assq :center shaoline-segments)))
               ;; User message is still active
               (shaoline-msg-active-p shaoline-message-timeout)))))
    (shaoline--update)
    (unless should-keep
      (shaoline--maybe-cancel-timer))))

(defun shaoline--message-filter-return (result &rest _args)
  "Intercept `message' for Shaoline: stores last user message & timestamp, manages timer.
Only reacts to user messages, not Shaoline's own."
  (when (and shaoline-mode
             (not (and (stringp result)
                       (get-text-property 0 'shaoline result))))
    (cond
     ;; New, non-empty message
     ((and (stringp result) (not (string-empty-p result)))
      (shaoline-msg-save result)
      (run-at-time 0 nil #'shaoline--update)
      (shaoline--maybe-start-timer)) ; timer is needed

     ;; Empty message: clear
     ((or (null result) (string-empty-p result))
      (shaoline-msg-clear)
      (run-at-time 0 nil #'shaoline--update)
      (shaoline--maybe-cancel-timer))))
  result)

(defun shaoline--cancel-redisplay-timer ()
  "Compatibility stub – idle-redisplay timer is no longer needed."
  nil)

(defun shaoline--schedule-redisplay ()
  "Compatibility stub – idle redisplay timer is no longer used."
  nil)

;; Clean up timer when minor mode toggled.
(add-hook 'shaoline-mode-hook #'shaoline--cancel-redisplay-timer)

;;;###autoload
(define-minor-mode shaoline-mode
  "Global minor mode that displays a functional minimalist modeline in echo-area."
  :global t
  :lighter ""
  (if shaoline-mode
      (progn
        (when shaoline-autohide-modeline
          (shaoline--autohide-modeline-globally))
        ;; Backup and disable vertical resizing of the echo area
        (setq shaoline--resize-mini-windows-backup resize-mini-windows)
        (setq resize-mini-windows nil)
        ;; Update: no debounce for post-command, short debounce for others
        (dolist (hook shaoline-update-hooks)
          (add-hook hook (if (eq hook 'post-command-hook)
                             #'shaoline--update
                           #'shaoline--debounced-update)))
        ;; Use add-function filter (not advice-add); set in shaoline.el
        ;; Hide shaoline when echo-area is used for input, then restore afterwards
        (add-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
        (add-hook 'minibuffer-exit-hook     #'shaoline--update)
        (add-hook 'isearch-mode-hook        #'shaoline--clear-display)
        (add-hook 'isearch-mode-end-hook    #'shaoline--update)
        (shaoline--update)
        (setq shaoline--timer nil) ;; Timer is lazy, not started automatically.
        )
    ;; turn off
    (dolist (hook shaoline-update-hooks)
      (remove-hook hook (if (eq hook 'post-command-hook)
                            #'shaoline--update
                          #'shaoline--debounced-update)))
    ;; Remove shaoline--message-filter via add-function (from shaoline.el)
    (when (fboundp 'shaoline--maybe-remove-message-filter)
      (shaoline--maybe-remove-message-filter))
    (shaoline--clear-display)
    ;; Remove our input hooks
    (remove-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
    (remove-hook 'minibuffer-exit-hook     #'shaoline--update)
    (remove-hook 'isearch-mode-hook        #'shaoline--clear-display)
    (remove-hook 'isearch-mode-end-hook    #'shaoline--update)
    (when shaoline-autohide-modeline
      (shaoline--unhide-modeline-globally))
    ;; Cancel the periodic timer
    (when (timerp shaoline--timer)
      (cancel-timer shaoline--timer)
      (setq shaoline--timer nil))
    ;; Cancel debounce timer
    (when (timerp shaoline--debounce-timer)
      (cancel-timer shaoline--debounce-timer)
      (setq shaoline--debounce-timer nil))
    ;; Restore minibuffer resize behavior
    (when shaoline--resize-mini-windows-backup
      (setq resize-mini-windows shaoline--resize-mini-windows-backup)
      (setq shaoline--resize-mini-windows-backup nil))))

(provide 'shaoline-infra)
;;; shaoline-infra.el ends here
