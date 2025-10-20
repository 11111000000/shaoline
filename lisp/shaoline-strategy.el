;;; shaoline-strategy.el --- Strategic adaptation for Shaoline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Strategic layer implementing adaptive behavior.
;; Like water, Shaoline flows around obstacles and adapts to its container.

;;; Code:

;; Forward declaration to silence compiler when this file is compiled
;; before shaoline-mode.el. define-minor-mode will provide the real binding.
(defvar shaoline-mode nil
  "Non-nil if Shaoline minor mode is enabled.")

(require 'shaoline)
(require 'shaoline-effects)

(declare-function shaoline-update "shaoline-mode")

;; ----------------------------------------------------------------------------
;; Context Detection — Reading the Environment
;; ----------------------------------------------------------------------------

(defun shaoline--detect-context ()
  "Detect current environmental context for adaptive decisions."
  (list :buffer-size (buffer-size)
        :buffer-remote-p (file-remote-p default-directory)
        :display-type (if (display-graphic-p) 'gui 'tty)
        :major-mode major-mode
        :buffer-modified-p (buffer-modified-p)
        :active-modes (mapcar #'car (remove nil minor-mode-alist))
        :system-load (when (fboundp 'load-average) (load-average))))

(defvar shaoline--last-context nil
  "Last detected context for change detection.")

(defun shaoline--context-changed-p ()
  "Detect if context has changed significantly."
  (let ((new-context (shaoline--detect-context)))
    (prog1 (not (equal new-context shaoline--last-context))
      (setq shaoline--last-context new-context))))

;; ----------------------------------------------------------------------------
;; Performance Metrics — Self-Awareness
;; ----------------------------------------------------------------------------

(defvar shaoline--metrics
  '(:update-count 0
                  :total-time 0.0
                  :last-update-time 0.0
                  :avg-update-time 0.0)
  "Performance metrics for self-monitoring.")

(defun shaoline--record-performance (start-time)
  "Record performance metrics from START-TIME."
  (let* ((duration (- (float-time) start-time))
         (count (1+ (plist-get shaoline--metrics :update-count)))
         (total (+ duration (plist-get shaoline--metrics :total-time)))
         (avg (/ total count)))
    (setq shaoline--metrics
          (plist-put
           (plist-put
            (plist-put shaoline--metrics :update-count count)
            :total-time total)
           :avg-update-time avg))))

;; ----------------------------------------------------------------------------
;; Adaptive Debouncing — Intelligent Response Timing
;; ----------------------------------------------------------------------------

(defvar shaoline--debounce-timer nil)
(defvar shaoline--pending-updates 0)

(defun shaoline--adaptive-debounce-delay ()
  "Calculate optimal debounce delay based on context and performance."
  (let* ((base-delay 0.1)
         (size-factor (min 2.0 (/ (buffer-size) 50000.0)))
         (load-factor (if-let ((load (car (load-average))))
                          (min 3.0 (/ load 1.0))
                        1.0))
         (perf-factor (min 2.0 (/ (plist-get shaoline--metrics :avg-update-time)
                                  0.01))))
    (* base-delay size-factor load-factor perf-factor)))

(defun shaoline--debounced-update ()
  "Intelligent debounced update with adaptive timing."
  (cl-incf shaoline--pending-updates)
  (when shaoline--debounce-timer
    (cancel-timer shaoline--debounce-timer))
  (setq shaoline--debounce-timer
        (run-with-timer
         (shaoline--adaptive-debounce-delay)
         nil
         (lambda ()
           (setq shaoline--pending-updates 0
                 shaoline--debounce-timer nil)
           (shaoline-update)))))

;; ----------------------------------------------------------------------------
;; Load Detection — System Awareness
;; ----------------------------------------------------------------------------

(defun shaoline--system-under-stress-p ()
  "Detect if system is under stress and should reduce activity."
  (or (> shaoline--pending-updates 5)
      (> (plist-get shaoline--metrics :avg-update-time) 0.05)
      (when-let ((load (car (load-average))))
        (> load 3.0))))

;; ----------------------------------------------------------------------------
;; Strategy Transitions — Smooth Mode Changes
;; ----------------------------------------------------------------------------

(defvar shaoline--strategy-transition-timer nil)

(defun shaoline--transition-to-strategy (new-strategy &optional delay)
  "Transition to NEW-STRATEGY after optional DELAY."
  (when shaoline--strategy-transition-timer
    (cancel-timer shaoline--strategy-transition-timer))
  (if delay
      (let ((ns new-strategy))
        (setq shaoline--strategy-transition-timer
              (run-with-timer delay nil
                              (lambda () (shaoline--apply-strategy ns)))))
    (shaoline--apply-strategy new-strategy)))

(defun shaoline--adaptive-strategy-selection ()
  "Select optimal strategy based on current context and performance."
  (cond
   ;; High stress → Yin (minimal)
   ((shaoline--system-under-stress-p) 'yin)

   ;; Remote files → Yin (no hooks)
   ((file-remote-p default-directory) 'yin)

   ;; Large files → Yin (performance)
   ((> (buffer-size) 500000) 'yin)

   ;; Special modes that need space → Yin
   ((memq major-mode '(shell-mode term-mode eshell-mode)) 'yin)

   ;; Normal interactive editing → Yang
   ((and (buffer-file-name) (display-graphic-p)) 'yang)

   ;; Default fallback → adaptive behavior
   (t 'yang)))

;; ----------------------------------------------------------------------------
;; Context Monitoring — Continuous Adaptation
;; ----------------------------------------------------------------------------

(defvar shaoline--monitor-timer nil)

(defun shaoline--start-context-monitoring ()
  "Start monitoring context for adaptive strategy change."
  (unless shaoline--monitor-timer
    (setq shaoline--monitor-timer
          (run-with-timer 2.0 2.0 #'shaoline--check-strategy-adaptation))))

(defun shaoline--stop-context-monitoring ()
  "Stop context monitoring."
  (when shaoline--monitor-timer
    (cancel-timer shaoline--monitor-timer)
    (setq shaoline--monitor-timer nil)))

(defun shaoline--check-strategy-adaptation ()
  "Check if strategy should be adapted based on context change."
  (when (and (eq shaoline-mode-strategy 'adaptive)
             (shaoline--context-changed-p))
    (let* ((current (shaoline--state-get :strategy))
           (optimal (shaoline--adaptive-strategy-selection)))
      (unless (eq current optimal)
        (shaoline--transition-to-strategy optimal 0.5)))))

;; ----------------------------------------------------------------------------
;; Traffic Shaping — Preventing Update Storms
;; ----------------------------------------------------------------------------

(defvar shaoline--update-bucket 0
  "Token bucket for update rate limiting.")

(defvar shaoline--bucket-timer nil)

(defun shaoline--refill-bucket ()
  "Refill update bucket with tokens."
  (setq shaoline--update-bucket
        (min 10 (1+ shaoline--update-bucket))))

(defun shaoline--start-rate-limiting ()
  "Start token bucket rate limiting."
  (unless shaoline--bucket-timer
    (setq shaoline--bucket-timer
          (run-with-timer 0.2 0.2 #'shaoline--refill-bucket))))

(defun shaoline--stop-rate-limiting ()
  "Stop rate limiting."
  (when shaoline--bucket-timer
    (cancel-timer shaoline--bucket-timer)
    (setq shaoline--bucket-timer nil)))

(defun shaoline--consume-update-token (&optional mode)
  "Consume update token based on MODE (:light, :normal)."
  (pcase mode
    ;; Light mode: always allow if any tokens available, minimal cost
    ('light (when (> shaoline--update-bucket 0)
              (cl-decf shaoline--update-bucket 0.2) ; Minimal token cost
              t))
    ;; Normal mode: standard token consumption
    (_ (when (> shaoline--update-bucket 0)
         (cl-decf shaoline--update-bucket)
         t))))

;; ----------------------------------------------------------------------------
;; Intelligent Update Decision — The Central Wisdom
;; ----------------------------------------------------------------------------

(defun shaoline--should-update-p ()
  "Central intelligence — Wu Wei decision making, with diagnostic logging."
  (let* ((busy (shaoline--echo-area-busy-p))
         (mode (cond
                ((shaoline--resolve-setting 'always-visible) 'light)
                ((shaoline--significant-change-p) 'light)
                (t 'normal)))
         (ok (and shaoline-mode (not busy) (shaoline--consume-update-token mode))))
    (shaoline--log "should-update? %s (busy=%s mode=%s buffer=%s)"
                   ok busy mode (buffer-name))
    ok))

;; ----------------------------------------------------------------------------
;; Strategy API — External Interface
;; ----------------------------------------------------------------------------

(defun shaoline-switch-to-yin ()
  "Switch to Yin strategy (minimal, manual)."
  (interactive)
  (shaoline--transition-to-strategy 'yin))

(defun shaoline-switch-to-yang ()
  "Switch to Yang strategy (active, automatic)."
  (interactive)
  (shaoline--transition-to-strategy 'yang))

(defun shaoline-switch-to-adaptive ()
  "Switch to adaptive strategy."
  (interactive)
  (setq shaoline-mode-strategy 'adaptive)
  (shaoline--start-context-monitoring)
  (shaoline--transition-to-strategy
   (shaoline--adaptive-strategy-selection)))

(defun shaoline-performance-report ()
  "Display performance metrics."
  (interactive)
  (message "Shaoline: %d updates, avg %.3fs, strategy: %s"
           (plist-get shaoline--metrics :update-count)
           (plist-get shaoline--metrics :avg-update-time)
           (shaoline--state-get :strategy)))

(provide 'shaoline-strategy)
;; Local Variables:
;; package-lint-main-file: "shaoline.el"
;; End:
;;; shaoline-strategy.el ends here
