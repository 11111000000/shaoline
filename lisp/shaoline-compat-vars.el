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

(provide 'shaoline-compat-vars)
;; Local Variables:
;; package-lint-main-file: "shaoline.el"
;; End:
;;; shaoline-compat-vars.el ends here
