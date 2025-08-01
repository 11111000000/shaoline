;;; shaoline-mode.el --- Minor mode interface for Shaoline -*- lexical-binding: t; -*-

;; Version: 3.0.0-dao

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The interface layer — where the wisdom of Dao meets practical use.
;; Simple activation, profound effect.

;;; Code:

(require 'shaoline)
(require 'shaoline-effects)
(require 'shaoline-strategy)
(require 'shaoline-segments)          ;; Load the default segment garden so that
;; shaoline-segment-major-mode (and friends)
;; are always available.

;; ----------------------------------------------------------------------------
;; Missing Variable Declarations
;; ----------------------------------------------------------------------------

(defvar shaoline-mode nil
  "Global mode variable for shaoline-mode.")

;; ----------------------------------------------------------------------------
;; Core Update Function — The Heart Beat
;; ----------------------------------------------------------------------------

(defun shaoline-update (&optional force)
  "Update Shaoline display with optional FORCE override."
  (interactive "P")
  (let ((start-time (float-time)))
    (when (or force (shaoline--should-update-p))
      (let ((content (shaoline-compose)))
        (shaoline--display content))
      (shaoline--record-performance start-time))))

;; ----------------------------------------------------------------------------
;; Manual Control Functions — User Agency
;; ----------------------------------------------------------------------------

(defun shaoline-refresh ()
  "Manually refresh Shaoline display."
  (interactive)
  (shaoline-update t))

(defun shaoline-clear ()
  "Clear Shaoline display."
  (interactive)
  (shaoline--clear-echo-area))

(defun shaoline-toggle-strategy ()
  "Cycle between yin, yang, and adaptive strategies."
  (interactive)
  (let ((current (shaoline--state-get :strategy)))
    (pcase current
      ('yin (shaoline-switch-to-yang))
      ('yang (shaoline-switch-to-adaptive))
      (_ (shaoline-switch-to-yin)))))

;; ----------------------------------------------------------------------------
;; Global Minor Mode — The Gateway
;; ----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode shaoline-mode
  "Global minor mode for Shaoline echo-area modeline.

Following the Dao of Wu Wei, Shaoline adapts its behavior based on context:

- yin: Minimal impact, manual updates only
- yang: Active monitoring, always visible
- adaptive: Intelligent switching based on environment

Use \\[shaoline-toggle-strategy] to cycle strategies.
Use \\[shaoline-refresh] to manually update.
Use \\[shaoline-clear] to clear display."
  :global t
  :lighter ""
  :keymap nil
  (if shaoline-mode
      (shaoline--activate)
    (shaoline--deactivate)))

(defun shaoline--activate ()
  "Activate Shaoline with current strategy."
  ;; Initialize state
  (shaoline--state-put :last-content "")
  (shaoline--state-put :cache (make-hash-table :test 'equal))

  ;; Start strategic monitoring
  (shaoline--start-rate-limiting)

  ;; Apply initial strategy
  (if (eq shaoline-mode-strategy 'adaptive)
      (shaoline-switch-to-adaptive)
    (shaoline--apply-strategy shaoline-mode-strategy))

  ;; Hide traditional mode-lines if configured
  (when (shaoline--resolve-setting 'hide-mode-lines)
    (shaoline--hide-mode-lines-globally))

  ;; Initial display
  (run-with-idle-timer 0.1 nil #'shaoline-update))

(defun shaoline--deactivate ()
  "Deactivate Shaoline and restore normal state."
  ;; Clean up all effects
  (shaoline--cleanup-all-effects)

  ;; Stop monitoring
  (shaoline--stop-context-monitoring)
  (shaoline--stop-rate-limiting)

  ;; Restore mode-lines
  (shaoline--restore-mode-lines-globally)

  ;; Clear state
  (shaoline--state-put :strategy nil)
  (shaoline--clear-echo-area))

;; ----------------------------------------------------------------------------
;; Integration Hooks — Gentle Adaptation
;; ----------------------------------------------------------------------------

(defun shaoline--after-theme-change (&rest _)
  "Adapt to theme changes gracefully."
  (run-with-idle-timer 0.5 nil #'shaoline-update))

(add-hook 'after-load-theme-hook #'shaoline--after-theme-change)

;; ----------------------------------------------------------------------------
;; Diagnostic Functions — Self-Awareness (ИСПРАВЛЕНО)
;; ----------------------------------------------------------------------------

(defun shaoline-status ()
  "Show current Shaoline status and configuration."
  (interactive)
  (let* ((strategy (shaoline--state-get :strategy))
         (effects-count (length shaoline--active-effects))
         (content-length (length (shaoline--state-get :last-content))))
    (message "Shaoline: %s strategy, %d effects, %d chars displayed"
             (or strategy "none") effects-count content-length)))

(defun shaoline-debug-info ()
  "Display detailed debug information."
  (interactive)
  (let ((info `(:strategy ,(shaoline--state-get :strategy)
                          :mode ,shaoline-mode
                          :context ,(shaoline--detect-context)
                          :metrics ,shaoline--metrics
                          :active-effects ,shaoline--active-effects)))
    (with-current-buffer (get-buffer-create "*shaoline-debug*")
      (erase-buffer)
      (pp info (current-buffer))
      (goto-char (point-min)))
    (display-buffer "*shaoline-debug*")))

;; ----------------------------------------------------------------------------
;; Cleanup Function — Return to Emptiness
;; ----------------------------------------------------------------------------

(defun shaoline-reset ()
  "Reset Shaoline to initial state."
  (interactive)
  (when shaoline-mode
    (shaoline-mode -1)
    (shaoline-mode 1)))

(defun shaoline-total-cleanup ()
  "Nuclear option: completely remove all Shaoline traces."
  (interactive)
  (shaoline-mode -1)
  (shaoline--cleanup-all-effects)
  (shaoline--stop-context-monitoring)
  (shaoline--stop-rate-limiting)
  (shaoline--restore-mode-lines-globally)
  (message "Shaoline: returned to emptiness"))

;; ----------------------------------------------------------------------------
;; Compatibility Bridge — Bridge Old and New
;; ----------------------------------------------------------------------------

(defun shaoline-cleanup ()
  "Legacy cleanup function for backward compatibility."
  (interactive)
  (shaoline-total-cleanup))

(provide 'shaoline-mode)
;;; shaoline-mode.el ends here
