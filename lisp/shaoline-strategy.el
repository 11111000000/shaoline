;;; shaoline-strategy.el --- Strategic adaptation for Shaoline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Strategic layer implementing adaptive behavior.
;; Like water, Shaoline flows around obstacles and adapts to its container.

;;; Code:

(require 'shaoline-compat-vars)

;; shaoline-mode is declared in shaoline-compat-vars.el

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
  (shaoline--ensure-strategy-vars)
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

(defun shaoline--ensure-strategy-vars ()
  "Ensure strategy hot-path variables are bound to safe defaults.

Needed because these variables are read/incremented from timers/advice
paths; if they are ever unbound (mixed builds, old .elc/.eln, or during
live reload), Emacs can get stuck in repeated void-variable errors."
  (unless (boundp 'shaoline--pending-updates)
    (setq shaoline--pending-updates 0))
  (unless (boundp 'shaoline--update-bucket)
    (setq shaoline--update-bucket 0))
  ;; Performance metrics may be read from update hot-path (mode layer).
  (unless (boundp 'shaoline--metrics)
    (setq shaoline--metrics
          '(:update-count 0
            :total-time 0.0
            :last-update-time 0.0
            :avg-update-time 0.0)))
  ;; Timers that may be read/cancelled from hot paths.
  (unless (boundp 'shaoline--debounce-timer)
    (setq shaoline--debounce-timer nil))
  (unless (boundp 'shaoline--bucket-timer)
    (setq shaoline--bucket-timer nil))
  (unless (boundp 'shaoline--monitor-timer)
    (setq shaoline--monitor-timer nil))
  (unless (boundp 'shaoline--strategy-transition-timer)
    (setq shaoline--strategy-transition-timer nil)))

(defun shaoline--adaptive-debounce-delay ()
  "Calculate optimal debounce delay based on context and performance."
  (shaoline--ensure-strategy-vars)
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
  (shaoline--ensure-strategy-vars)
  (cl-incf shaoline--pending-updates)
  (when shaoline--debounce-timer
    (cancel-timer shaoline--debounce-timer))
  (setq shaoline--debounce-timer
        (run-with-timer
         (shaoline--adaptive-debounce-delay)
         nil
         (lambda ()
           (shaoline--ensure-strategy-vars)
           (setq shaoline--pending-updates 0
                 shaoline--debounce-timer nil)
           (shaoline-update)))))

;; ----------------------------------------------------------------------------
;; Load Detection — System Awareness
;; ----------------------------------------------------------------------------

(defun shaoline--system-under-stress-p ()
  "Return non-nil only for sustained Shaoline-local pressure."
  (shaoline--ensure-strategy-vars)
  (or (> shaoline--pending-updates 5)
      (> (plist-get shaoline--metrics :avg-update-time) 0.05)
      (and (fboundp 'load-average)
           (when-let ((load (car (load-average))))
             (> load 3.0)))))

;; ----------------------------------------------------------------------------
;; Strategy Transitions — Smooth Mode Changes
;; ----------------------------------------------------------------------------

(defvar shaoline--strategy-transition-timer nil)

(defun shaoline--transition-to-strategy (new-strategy &optional delay)
  "Transition to NEW-STRATEGY after optional DELAY."
  (shaoline--ensure-strategy-vars)
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
  (shaoline--ensure-strategy-vars)
  (unless shaoline--monitor-timer
    (setq shaoline--monitor-timer
          (run-with-timer 2.0 2.0 #'shaoline--check-strategy-adaptation))))

(defun shaoline--stop-context-monitoring ()
  "Stop context monitoring."
  (shaoline--ensure-strategy-vars)
  (when shaoline--monitor-timer
    (cancel-timer shaoline--monitor-timer)
    (setq shaoline--monitor-timer nil)))

(defun shaoline--check-strategy-adaptation ()
  "Refresh adaptive context without reinstalling effects."
  (when (eq (or (and (boundp 'shaoline-mode-strategy) shaoline-mode-strategy)
                'yang)
            'adaptive)
    (shaoline--context-changed-p)))


;; ----------------------------------------------------------------------------
;; Traffic Shaping — Preventing Update Storms
;; ----------------------------------------------------------------------------

(defvar shaoline--update-bucket 0
  "Token bucket for update rate limiting.")

(defvar shaoline--bucket-timer nil)

(defun shaoline--refill-bucket ()
  "Refill update bucket with tokens."
  (shaoline--ensure-strategy-vars)
  (setq shaoline--update-bucket
        (min 10 (1+ shaoline--update-bucket))))

(defun shaoline--start-rate-limiting ()
  "Start token bucket rate limiting."
  (shaoline--ensure-strategy-vars)
  (unless shaoline--bucket-timer
    (setq shaoline--bucket-timer
          (run-with-timer 0.25 0.25 #'shaoline--refill-bucket))))

(defun shaoline--stop-rate-limiting ()
  "Stop rate limiting."
  (shaoline--ensure-strategy-vars)
  (when shaoline--bucket-timer
    (cancel-timer shaoline--bucket-timer)
    (setq shaoline--bucket-timer nil)))

(defun shaoline--consume-update-token (&optional mode)
  "Consume update token based on MODE (:light, :normal)."
  (shaoline--ensure-strategy-vars)
  (pcase mode
    ;; Light mode: always allow if any tokens available, minimal cost
    ('light (when (> shaoline--update-bucket 0)
              (cl-decf shaoline--update-bucket 0.6) ; Increased cost to reduce burst rate
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
  "Switch to adaptive strategy without replacing the active effects."
  (interactive)
  (setq shaoline-mode-strategy 'adaptive)
  (shaoline--start-context-monitoring)
  (shaoline--state-put :strategy 'adaptive)
  (shaoline--apply-strategy 'adaptive))

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
