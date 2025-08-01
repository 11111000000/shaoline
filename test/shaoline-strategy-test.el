;;; shaoline-strategy-test.el --- Strategy system tests for Shaoline 3.0 -*- lexical-binding: t; -*-

;; Version: 3.0.0-dao

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'shaoline)
(require 'shaoline-strategy)

;; ----------------------------------------------------------------------------
;; Test: Context detection
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-context-detection ()
  "Test context detection provides expected data."
  (let ((context (shaoline--detect-context)))
    (should (plist-get context :buffer-size))
    (should (plist-get context :display-type))
    (should (plist-get context :major-mode))
    (should (member (plist-get context :display-type) '(gui tty)))))

(ert-deftest shaoline-context-change-detection ()
  "Test context change detection."
  (setq shaoline--last-context nil)

  ;; First call should detect change
  (should (shaoline--context-changed-p))

  ;; Second call immediately should not detect change
  (should-not (shaoline--context-changed-p))

  ;; Changing major mode should detect change
  (let ((major-mode 'text-mode))
    (should (shaoline--context-changed-p))))

;; ----------------------------------------------------------------------------
;; Test: Performance metrics
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-performance-metrics ()
  "Test performance metrics recording."
  (setq shaoline--metrics '(:update-count 0 :total-time 0.0 :avg-update-time 0.0))

  (let ((start-time (- (float-time) 0.1)))  ;; Simulate 0.1s operation
    (shaoline--record-performance start-time)

    (should (= (plist-get shaoline--metrics :update-count) 1))
    (should (> (plist-get shaoline--metrics :total-time) 0.05))
    (should (> (plist-get shaoline--metrics :avg-update-time) 0.05))))

;; ----------------------------------------------------------------------------
;; Test: Adaptive debouncing
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-adaptive-debouncing ()
  "Test adaptive debounce delay calculation."
  ;; Mock various system conditions for testing
  (cl-letf (((symbol-function 'buffer-size) (lambda () 1000))
            ((symbol-function 'load-average) (lambda () '(1.0))))
    (setq shaoline--metrics (plist-put shaoline--metrics :avg-update-time 0.01))

    (let ((delay (shaoline--adaptive-debounce-delay)))
      (should (numberp delay))
      (should (> delay 0)))))

(ert-deftest shaoline-debounced-update ()
  "Test debounced update mechanism."
  (setq shaoline--pending-updates 0
        shaoline--debounce-timer nil)

  (let ((update-called nil))
    ;; Mock update function
    (cl-letf (((symbol-function 'shaoline-update) (lambda () (setq update-called t))))

      ;; Call debounced update
      (shaoline--debounced-update)
      (should (= shaoline--pending-updates 1))
      (should shaoline--debounce-timer)

      ;; Another call should cancel previous timer
      (let ((old-timer shaoline--debounce-timer))
        (shaoline--debounced-update)
        (should (= shaoline--pending-updates 2))))))

;; ----------------------------------------------------------------------------
;; Test: System stress detection
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-stress-detection ()
  "Test system stress detection."
  ;; No stress conditions
  (setq shaoline--pending-updates 0)
  (setq shaoline--metrics '(:avg-update-time 0.01))
  (cl-letf (((symbol-function 'load-average) (lambda () '(0.5))))
    (should-not (shaoline--system-under-stress-p)))

  ;; High pending updates = stress
  (setq shaoline--pending-updates 10)
  (should (shaoline--system-under-stress-p))

  ;; High average update time = stress
  (setq shaoline--pending-updates 0
        shaoline--metrics '(:avg-update-time 0.1))
  (should (shaoline--system-under-stress-p))

  ;; High system load = stress
  (setq shaoline--metrics '(:avg-update-time 0.01))
  (cl-letf (((symbol-function 'load-average) (lambda () '(5.0))))
    (should (shaoline--system-under-stress-p))))

;; ----------------------------------------------------------------------------
;; Test: Strategy selection
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-adaptive-strategy-selection ()
  "Test adaptive strategy selection logic."
  ;; Mock normal conditions -> should select yang
  (cl-letf (((symbol-function 'shaoline--system-under-stress-p) (lambda () nil))
            ((symbol-function 'file-remote-p) (lambda (dir) nil))
            ((symbol-function 'buffer-size) (lambda () 10000))
            (major-mode 'emacs-lisp-mode)
            ((symbol-function 'buffer-file-name) (lambda () "/test/file.el"))
            ((symbol-function 'display-graphic-p) (lambda () t)))
    (should (eq (shaoline--adaptive-strategy-selection) 'yang)))

  ;; Mock stress conditions -> should select yin
  (cl-letf (((symbol-function 'shaoline--system-under-stress-p) (lambda () t)))
    (should (eq (shaoline--adaptive-strategy-selection) 'yin)))

  ;; Mock remote file -> should select yin
  (cl-letf (((symbol-function 'shaoline--system-under-stress-p) (lambda () nil))
            ((symbol-function 'file-remote-p) (lambda (dir) t)))
    (should (eq (shaoline--adaptive-strategy-selection) 'yin)))

  ;; Mock large file -> should select yin
  (cl-letf (((symbol-function 'shaoline--system-under-stress-p) (lambda () nil))
            ((symbol-function 'file-remote-p) (lambda (dir) nil))
            ((symbol-function 'buffer-size) (lambda () 1000000)))
    (should (eq (shaoline--adaptive-strategy-selection) 'yin)))

  ;; Mock shell mode -> should select yin
  (cl-letf (((symbol-function 'shaoline--system-under-stress-p) (lambda () nil))
            ((symbol-function 'file-remote-p) (lambda (dir) nil))
            ((symbol-function 'buffer-size) (lambda () 10000))
            (major-mode 'shell-mode))
    (should (eq (shaoline--adaptive-strategy-selection) 'yin))))

;; ----------------------------------------------------------------------------
;; Test: Strategy transitions
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-strategy-transitions ()
  "Test strategy transition mechanism."
  (setq shaoline--strategy-transition-timer nil)

  (let ((applied-strategy nil))
    ;; Mock strategy application
    (cl-letf (((symbol-function 'shaoline--apply-strategy)
               (lambda (strategy) (setq applied-strategy strategy))))

      ;; Immediate transition
      (shaoline--transition-to-strategy 'test-strategy)
      (should (eq applied-strategy 'test-strategy))

      ;; Delayed transition
      (setq applied-strategy nil)
      (shaoline--transition-to-strategy 'delayed-strategy 0.1)
      (should (null applied-strategy))  ;; Should not apply immediately
      (should shaoline--strategy-transition-timer))))

;; ----------------------------------------------------------------------------
;; Test: Token bucket rate limiting
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-rate-limiting ()
  "Test token bucket rate limiting."
  (setq shaoline--update-bucket 5)

  ;; Should consume token when available
  (should (shaoline--consume-update-token))
  (should (= shaoline--update-bucket 4))

  ;; Empty bucket should not allow consumption
  (setq shaoline--update-bucket 0)
  (should-not (shaoline--consume-update-token))
  (should (= shaoline--update-bucket 0))

  ;; Refill should add tokens
  (shaoline--refill-bucket)
  (should (= shaoline--update-bucket 1)))

;; ----------------------------------------------------------------------------
;; Test: Update decision logic
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-update-decision ()
  "Test central update decision logic."
  (setq shaoline-mode t
        shaoline--update-bucket 5)

  ;; Mock functions to control decision
  (cl-letf (((symbol-function 'minibufferp) (lambda () nil))
            ((symbol-function 'active-minibuffer-window) (lambda () nil))
            ((symbol-function 'shaoline--context-changed-p) (lambda () t)))

    (should (shaoline--should-update-p)))

  ;; Mode disabled should prevent update
  (setq shaoline-mode nil)
  (should-not (shaoline--should-update-p))

  ;; No tokens should prevent update
  (setq shaoline-mode t
        shaoline--update-bucket 0)
  (should-not (shaoline--should-update-p))

  ;; Minibuffer should prevent update
  (setq shaoline--update-bucket 5)
  (cl-letf (((symbol-function 'minibufferp) (lambda () t)))
    (should-not (shaoline--should-update-p))))

;; ----------------------------------------------------------------------------
;; Test: Strategy API
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-strategy-api ()
  "Test strategy switching API functions."
  (let ((applied-strategy nil))
    ;; Mock strategy application
    (cl-letf (((symbol-function 'shaoline--transition-to-strategy)
               (lambda (strategy &optional delay) (setq applied-strategy strategy))))

      ;; Test yin switch
      (shaoline-switch-to-yin)
      (should (eq applied-strategy 'yin))

      ;; Test yang switch
      (shaoline-switch-to-yang)
      (should (eq applied-strategy 'yang)))))

;; ----------------------------------------------------------------------------
;; Test: Context monitoring
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-context-monitoring ()
  "Test context monitoring start/stop."
  (setq shaoline--monitor-timer nil)

  ;; Start monitoring
  (shaoline--start-context-monitoring)
  (should shaoline--monitor-timer)

  ;; Stop monitoring
  (shaoline--stop-context-monitoring)
  (should-not shaoline--monitor-timer))

(provide 'shaoline-strategy-test)
;;; shaoline-strategy-test.el ends here
