;;; shaoline-compat-vars.el --- Shared special vars for Shaoline  -*- lexical-binding: nil; -*-

;; Keywords: convenience, internal
;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; Maintainer: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; This file only forward-declares special (dynamic) variables that are
;; referenced across compilation units. It must not introduce any lexical
;; bindings for these symbols to avoid “Defining as dynamic an already lexical var”.

;;; Commentary:
;;; Code:

;; Minor mode variable: defined by define-minor-mode in shaoline-mode.el
(defvar shaoline-mode nil
  "Non-nil when Shaoline mode is enabled.")

;; Non-nil while Shaoline is composing its line; suppress side effects.
(defvar shaoline--composing-p nil
  "Non-nil while Shaoline is composing its line; suppress side effects.")

;; Strategy layer: counter of queued debounced updates.
;; (Used in timers/advice paths; must never be unbound.)
(defvar shaoline--pending-updates 0
  "Internal counter of queued debounced updates.")

;; Effects layer: timestamp until which eval-result message is pinned.
;; (Read inside `message' advice; must never be unbound.)
(defvar shaoline--message-pinned-until 0
  "Float timestamp until which generic messages must not override a pinned one.")

;; Strategy layer: token bucket for update rate limiting.
(defvar shaoline--update-bucket 0
  "Token bucket for update rate limiting.")

;; Echo-area input depth while inside read-event originating in echo area.
(defvar shaoline--echo-area-input-depth 0
  "Internal counter of nested `read-event' calls from the echo area.")

;; Last busy-state logged (to reduce log noise).
(defvar shaoline--last-busy-log-state nil
  "Internal: last observed busy state of the echo area for logging.")

;; Timers used by effects/strategy; forward declarations only.
(defvar shaoline--yang-timer nil
  "Timer used to reassert Yang visibility (periodic gentle restore).")

(defvar shaoline--restore-timer nil
  "Idle timer used for delayed echo-area restore after commands.")

;; Throttle for movement-triggered updates.
(defvar shaoline--last-movement-update 0
  "Timestamp of the last movement-triggered update (float seconds).")

;; ---------------------------------------------------------------------------
;; Hot-path timers/state (may be touched from advice/hooks/timers).
;; Forward declarations here prevent “void-variable” bricks when the user
;; reloads files or has mixed .el/.elc/.eln copies in load-path.
;; ---------------------------------------------------------------------------

;; Strategy layer: debounce and scheduling timers.
(defvar shaoline--debounce-timer nil
  "Timer used by `shaoline--debounced-update' to coalesce updates.")

(defvar shaoline--bucket-timer nil
  "Timer used to refill the strategy token bucket.")

(defvar shaoline--monitor-timer nil
  "Timer used to monitor context changes in adaptive strategy.")

(defvar shaoline--strategy-transition-timer nil
  "Timer used for delayed strategy transitions.")

;; Effects layer: message burst coalescing.
(defvar shaoline--msg-update-timer nil
  "Timer used to coalesce bursts of message/minibuffer-message updates.")

;; Effects layer: prefix-keys capture lifecycle.
(defvar shaoline--current-keys ""
  "Current prefix keys being typed (effects layer state).")

(defvar shaoline--current-keys-time 0
  "Timestamp when `shaoline--current-keys' was last captured.")

(defvar shaoline--clear-keys-timer nil
  "Timer used to clear current prefix keys after a short delay.")

;; Effects layer: timestamp of last Shaoline-originated `message' call.
(defvar shaoline--last-display-time 0
  "Time (float) when Shaoline last called `message'.")

;; Effects layer: significant-state change detection.
(defvar shaoline--last-significant-state nil
  "Last significant state used for change detection in effects layer.")

(defun shaoline--ensure-hot-vars ()
  "Ensure Shaoline hot-path variables are bound to safe defaults.

This function is intentionally conservative and fail-open: it is designed to
run from advice/hooks/timers while Shaoline is active, including during
hot-reload workflows that temporarily `makunbound' defvars."
  ;; Core gates/state ----------------------------------------------------------
  (unless (boundp 'shaoline-mode) (setq shaoline-mode nil))
  (unless (boundp 'shaoline--composing-p) (setq shaoline--composing-p nil))
  ;; Strategy hot vars ---------------------------------------------------------
  (unless (boundp 'shaoline--pending-updates) (setq shaoline--pending-updates 0))
  (unless (boundp 'shaoline--update-bucket) (setq shaoline--update-bucket 0))
  (unless (boundp 'shaoline--debounce-timer) (setq shaoline--debounce-timer nil))
  (unless (boundp 'shaoline--bucket-timer) (setq shaoline--bucket-timer nil))
  (unless (boundp 'shaoline--monitor-timer) (setq shaoline--monitor-timer nil))
  (unless (boundp 'shaoline--strategy-transition-timer) (setq shaoline--strategy-transition-timer nil))
  (unless (boundp 'shaoline--metrics)
    (setq shaoline--metrics
          '(:update-count 0
            :total-time 0.0
            :last-update-time 0.0
            :avg-update-time 0.0)))
  ;; Effects hot vars ----------------------------------------------------------
  (unless (boundp 'shaoline--message-pinned-until) (setq shaoline--message-pinned-until 0))
  (unless (boundp 'shaoline--msg-update-timer) (setq shaoline--msg-update-timer nil))
  (unless (boundp 'shaoline--allow-empty-message) (setq shaoline--allow-empty-message nil))
  (unless (boundp 'shaoline--echo-area-input-depth) (setq shaoline--echo-area-input-depth 0))
  (unless (boundp 'shaoline--last-busy-log-state) (setq shaoline--last-busy-log-state nil))
  (unless (boundp 'shaoline--yang-timer) (setq shaoline--yang-timer nil))
  (unless (boundp 'shaoline--restore-timer) (setq shaoline--restore-timer nil))
  (unless (boundp 'shaoline--last-movement-update) (setq shaoline--last-movement-update 0))
  (unless (boundp 'shaoline--current-keys) (setq shaoline--current-keys ""))
  (unless (boundp 'shaoline--current-keys-time) (setq shaoline--current-keys-time 0))
  (unless (boundp 'shaoline--clear-keys-timer) (setq shaoline--clear-keys-timer nil))
  (unless (boundp 'shaoline--last-display-time) (setq shaoline--last-display-time 0))
  (unless (boundp 'shaoline--last-significant-state) (setq shaoline--last-significant-state nil))
  ;; Registries mutated from hot paths ----------------------------------------
  (unless (boundp 'shaoline--active-effects) (setq shaoline--active-effects nil))
  (unless (boundp 'shaoline--hook-registry) (setq shaoline--hook-registry nil))
  (unless (boundp 'shaoline--advice-registry) (setq shaoline--advice-registry nil))
  (unless (boundp 'shaoline--timer-registry) (setq shaoline--timer-registry (make-hash-table)))
  (unless (boundp 'shaoline--modeline-backup-registry)
    (setq shaoline--modeline-backup-registry (make-hash-table :weakness 'key)))
  (unless (boundp 'shaoline--original-default-modeline) (setq shaoline--original-default-modeline nil))
  (unless (boundp 'shaoline--effect-log) (setq shaoline--effect-log nil)))

(provide 'shaoline-compat-vars)
;; Local Variables:
;; package-lint-main-file: "shaoline.el"
;; End:
;;; shaoline-compat-vars.el ends here
