;;; shaoline-infra.el --- Infrastructure for Shaoline echo-area modeline -*- lexical-binding: t; -*-

;; Version: 2.2.2

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; This file hosts *impure* infrastructure that complements the pure core.
;; It is loaded *from* shaoline.el, so we purposely do *not* `require' it
;; back to avoid a circular dependency.

;; Infra requires core (shaoline.el) for variables/faces.
(require 'shaoline)
(require 'shaoline-events)

(eval-when-compile
  (defvar shaoline-autohide-modeline)
  (defvar shaoline-exclude-modes))

(require 'cl-lib)                    ; for cl-member etc.

;; ----------------------------------------------------------------------------
;; Customization: timer/message defcustom moved to shaoline.el for proper user customization.

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

(defun shaoline--update (&rest _)
  "Recompute and display modeline for the currently selected window.
Skips update and clears during isearch or minibuffer input."
  (shaoline--log "shaoline--update")
  (if (or (active-minibuffer-window)
          (minibufferp)
          (bound-and-true-p isearch-mode))
      (shaoline--clear-display)
    (let ((cur-msg (current-message)))
      (unless (and (not shaoline-always-visible)
                   cur-msg
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
;; Message advice and echo-area message capture infrastructure is now in shaoline-events.el.
;; The advice(s) and related helpers have been moved for better separation of impure effects.

;; Activate advice and hooks/timers when shaoline-mode toggles.
(add-hook 'shaoline-mode-hook
          (lambda ()
            (if shaoline-mode
                (shaoline-events-enable)
              (shaoline-events-disable))))

;; ----------------------------------------------------------------------------
;; Shaoline minor mode engine.

;; Debounce timer, periodic timer, and associated logic now reside in shaoline-events.el

;; Удаляем устаревшие функции: они заменены на shaoline--message-filter-capture в earlier части infra.
;; Таймеры теперь lazy, без этих stubs.

;;;###autoload
(define-minor-mode shaoline-mode
  "Global minor mode that displays a functional minimalist modeline in echo-area."
  :global t
  :lighter ""
  (if shaoline-mode
      (progn (require 'shaoline)
             (when shaoline-autohide-modeline
               (shaoline--autohide-modeline-globally))
             (setq shaoline--resize-mini-windows-backup resize-mini-windows)
             (setq resize-mini-windows nil)
             (run-with-idle-timer 0.1 nil #'shaoline--update))
    (progn (shaoline--clear-display)
           (when shaoline-autohide-modeline
             (shaoline--unhide-modeline-globally))
           (when shaoline--resize-mini-windows-backup
             (setq resize-mini-windows shaoline--resize-mini-windows-backup)
             (setq shaoline--resize-mini-windows-backup nil)))))

(provide 'shaoline-infra)
;;; shaoline-infra.el ends here
