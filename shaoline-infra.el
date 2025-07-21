;;; shaoline-infra.el --- Infrastructure for Shaoline echo-area modeline -*- lexical-binding: t; -*-

;; Version: 2.1.1

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; This file hosts *impure* infrastructure that complements the pure core.
;; It is loaded *from* shaoline.el, so we purposely do *not* `require' it
;; back to avoid a circular dependency.

;; Infra requires core (shaoline.el) for variables/faces.
(require 'shaoline)

(eval-when-compile
  (defvar shaoline-autohide-modeline)
  (defvar shaoline-exclude-modes))

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

(defun shaoline--predictive-clear ()
  "Predictive clear before commands that may activate minibuffer."
  (when (and (commandp this-command)
             (memq this-command '(execute-extended-command find-file)))
    (shaoline--clear-display)))

(add-hook 'pre-command-hook #'shaoline--predictive-clear)

(defun shaoline--update (&rest _)
  "Recompute and display modeline for the currently selected window.
Skips update and clears during isearch or minibuffer input."
  (shaoline--log "shaoline--update")
  (if (or (active-minibuffer-window)
          (minibufferp)
          (bound-and-true-p isearch-mode))
      (shaoline--clear-display)
    (let ((cur-msg (current-message)))
      (unless (and cur-msg
                   (not (get-text-property 0 'shaoline cur-msg)))
        (shaoline--display
         (shaoline-compose-modeline))))))

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
;; Message advice (moved from shaoline.el for modularity).

(defun shaoline--empty-message-p (fmt args)
  "Return non-nil when calling =message' with FMT/ARGS would show nothing."
  (or (null fmt)
      (and (stringp fmt)
           (string-empty-p (apply #'format fmt args)))
      (and (listp fmt) (equal fmt '("")))))

(defun shaoline--message-filter-capture (orig-fmt &rest args)
  "Around advice for =message=: capture last non-empty message persistently.
Never allow empty message to erase echo area when shaoline-mode is active."
  (let* ((str (apply orig-fmt args))
         (curmsg shaoline-msg--last-user-message))
    (when shaoline-debug
      (shaoline--log "[SHAOLINE] message called: fmt='%s' args='%s' str='%s'" orig-fmt args str))
    (cond
     ;; Case 1: Non-empty, user message.
     ((and (bound-and-true-p shaoline-mode)
           (stringp str) (not (string-empty-p str))
           (not (get-text-property 0 'shaoline str)))
      (setq shaoline-msg--last-user-message str)
      (setq shaoline-msg--last-user-message-ts (float-time))
      (when shaoline-debug (shaoline--log "[SHAOLINE] Saved user message '%s'" str))
      (run-at-time 0 nil #'shaoline--update)
      str)
     ;; Case 2: Empty message, keep current shaoline (never clear!)
     ((and (bound-and-true-p shaoline-mode)
           (or (not str) (string-empty-p str)))
      (when shaoline-debug (shaoline--log "[SHAOLINE] Suppressing echo area clear, restoring message '%s'" curmsg))
      (or (current-message) curmsg ""))
     ;; Case 3: Not in shaoline-mode, normal echo.
     (t str))))

(defun shaoline--advise-message ()
  "Activate advice that captures messages for persistent echo segment."
  (add-function :around (symbol-function 'message) #'shaoline--message-filter-capture))

(defun shaoline--remove-message-advice ()
  "Remove shaoline persistent capture advice from =message'."
  (ignore-errors
    (remove-function (symbol-function 'message) #'shaoline--message-filter-capture)))

;; Activate advice when shaoline-mode toggles.
(add-hook 'shaoline-mode-hook
          (lambda ()
            (if shaoline-mode
                (shaoline--advise-message)
              (shaoline--remove-message-advice))))

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
  "Start timer only if dynamic segments are enabled and present in :right or :center.
No longer starts for messages, as they are persistent."
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
  "Disable timer if nothing dynamic is needed. No message check (persistent)."
  (shaoline--log "shaoline--lazy-update")
  (let ((should-keep
         (and shaoline-enable-dynamic-segments
              (or
               ;; Check if a dynamic segment (time/battery) is present.
               (cl-member 'shaoline-segment-time (cdr (assq :right shaoline-segments)))
               (cl-member 'shaoline-segment-time (cdr (assq :center shaoline-segments)))
               (cl-member 'shaoline-segment-battery (cdr (assq :right shaoline-segments)))
               (cl-member 'shaoline-segment-battery (cdr (assq :center shaoline-segments)))))))
    (shaoline--update)
    (unless should-keep
      (shaoline--maybe-cancel-timer))))

;; Удаляем устаревшие функции: они заменены на shaoline--message-filter-capture в earlier части infra.
;; Таймеры теперь lazy, без этих stubs.

;;;###autoload
(define-minor-mode shaoline-mode
  "Global minor mode that displays a functional minimalist modeline in echo-area."
  :global t
  :lighter ""
  (if shaoline-mode
      (progn
        (require 'shaoline)
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
        ;; filter-return больше не используется (заменено на around advice выше).
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
    ;; filter-return не используется.
    (shaoline--clear-display)
    ;; Remove our input hooks
    (remove-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
    (remove-hook 'minibuffer-exit-hook     #'shaoline--update)
    (remove-hook 'isearch-mode-hook        #'shaoline--clear-display)
    (remove-hook 'isearch-mode-end-hook    #'shaoline--update)
    (remove-hook 'pre-command-hook         #'shaoline--predictive-clear)  ;; Cleanup new hook
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
