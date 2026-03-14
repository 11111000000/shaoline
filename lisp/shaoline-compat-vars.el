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

(provide 'shaoline-compat-vars)
;; Local Variables:
;; package-lint-main-file: "shaoline.el"
;; End:
;;; shaoline-compat-vars.el ends here
