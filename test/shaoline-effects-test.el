;;; shaoline-effects-test.el --- Effect system tests for Shaoline 3.0 -*- lexical-binding: t; -*-

;; Version: 3.0.0-dao

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'shaoline)
(require 'shaoline-effects)
(require 'cl-lib)
;; cl-format is not autoloaded; older Emacs may not have it in cl-lib
(unless (fboundp 'cl-format)
  (ignore-errors (require 'cl-extra)))

;; ----------------------------------------------------------------------------
;; Test: Effect logging
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-effect-logging ()
  "Test that effects are properly logged."
  (setq shaoline--effect-log nil)
  (shaoline--log-effect 'test-effect)
  (should (= (length shaoline--effect-log) 1))
  (should (eq (car (car shaoline--effect-log)) 'test-effect)))

;; ----------------------------------------------------------------------------
;; Test: Display effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-display-effect ()
  "Test display effect with tagging."
  (shaoline--state-put :last-content "")

  ;; Mock should-display-p to return true
  (cl-letf (((symbol-function 'shaoline--should-display-p) (lambda (content) t)))
    (shaoline--display "test content")
    (should (string= (shaoline--state-get :last-content) "test content"))
    (should (member 'display shaoline--active-effects))))

(ert-deftest shaoline-clear-effect ()
  "Test clear echo area effect."
  (shaoline--state-put :last-content "some content")

  ;; Mock current-message to return tagged content
  (cl-letf (((symbol-function 'current-message)
             (lambda () (propertize "test" 'shaoline-origin t))))
    (shaoline--clear-echo-area)
    (should (string= (shaoline--state-get :last-content) ""))
    (should (member 'clear shaoline--active-effects))))

;; ----------------------------------------------------------------------------
;; Test: Timer effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-timer-effects ()
  "Test timer start and stop effects."
  (clrhash shaoline--timer-registry)

  ;; Start a timer
  (shaoline--start-timer 'test-timer 1.0 nil (lambda () (message "test")))
  (should (gethash 'test-timer shaoline--timer-registry))
  (should (member '(timer . test-timer) shaoline--active-effects))

  ;; Stop the timer
  (shaoline--stop-timer 'test-timer)
  (should-not (gethash 'test-timer shaoline--timer-registry)))

(ert-deftest shaoline-timer-cleanup ()
  "Test cleanup of all timers."
  (clrhash shaoline--timer-registry)

  ;; Start multiple timers
  (shaoline--start-timer 'timer1 1.0 nil (lambda () (message "test1")))
  (shaoline--start-timer 'timer2 1.0 nil (lambda () (message "test2")))

  (should (= (hash-table-count shaoline--timer-registry) 2))

  ;; Cleanup all
  (shaoline--cleanup-all-timers)
  (should (= (hash-table-count shaoline--timer-registry) 0)))

;; ----------------------------------------------------------------------------
;; Test: Hook effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-hook-effects ()
  "Test hook attach and detach effects."
  (setq shaoline--hook-registry nil)

  ;; Test function
  (defun test-hook-function () (message "test"))

  ;; Attach hook
  (shaoline--attach-hook 'test-hook 'test-hook-function)
  (should (member '(test-hook . test-hook-function) shaoline--hook-registry))
  (should (member '(hook . test-hook) shaoline--active-effects))

  ;; Detach hook
  (shaoline--detach-hook 'test-hook 'test-hook-function)
  (should-not (assoc 'test-hook shaoline--hook-registry)))

(ert-deftest shaoline-hook-cleanup ()
  "Test cleanup of all hooks."
  (setq shaoline--hook-registry nil)

  ;; Add some hooks to registry
  (push '(hook1 . func1) shaoline--hook-registry)
  (push '(hook2 . func2) shaoline--hook-registry)

  (should (= (length shaoline--hook-registry) 2))

  ;; Mock remove-hook
  (cl-letf (((symbol-function 'remove-hook) (lambda (hook func) nil)))
    (shaoline--cleanup-all-hooks)
    (should (= (length shaoline--hook-registry) 0))))

;; ----------------------------------------------------------------------------
;; Test: Advice effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-advice-effects ()
  "Test advice attach and detach effects."
  (setq shaoline--advice-registry nil)

  ;; Test advice function
  (defun test-advice (orig-fn &rest args) (apply orig-fn args))

  ;; Attach advice
  (shaoline--attach-advice 'message :around 'test-advice)
  (should (member '(message :around test-advice) shaoline--advice-registry))
  (should (member '(advice . message) shaoline--active-effects))

  ;; Detach advice
  (shaoline--detach-advice 'message 'test-advice)
  (should-not (cl-find-if (lambda (entry)
                            (and (eq (car entry) 'message)
                                 (eq (caddr entry) 'test-advice)))
                          shaoline--advice-registry)))

;; ----------------------------------------------------------------------------
;; Test: Mode line effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-mode-line-effects ()
  "Test mode line hide and restore effects."
  (with-temp-buffer
    (let ((original-mode-line mode-line-format))
      ;; Hide mode line
      (shaoline--hide-mode-line)
      (should (null mode-line-format))
      (should (local-variable-p 'shaoline--original-mode-line))

      ;; Restore mode line
      (shaoline--restore-mode-line)
      (should mode-line-format)
      (should-not (local-variable-p 'shaoline--original-mode-line)))))

;; ----------------------------------------------------------------------------
;; Test: Message capture advice
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-message-capture ()
  "Test message capture advice."
  (shaoline-msg-clear)

  ;; Mock original message function
  (shaoline-msg-clear)
  (shaoline--advice-capture-message (lambda (fmt &rest args)
                                      (format fmt args))
                                    "Test %s" "message")
  (should (string= (shaoline-msg-current) "Test message"))
  )

;; ----------------------------------------------------------------------------
;; Test: Guard timer
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-guard-visibility ()
  "Test guard timer behavior."
  (let ((update-called nil))
    ;; Set up state to trigger guard update
    (setq shaoline-mode t)
    (shaoline--state-put :last-content "test content")
    (setq shaoline--last-display-time 0) ; Force stale timestamp

    ;; Mock functions
    (cl-letf (((symbol-function 'shaoline--echo-area-busy-p) (lambda () nil))
              ((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'shaoline-update) (lambda () (setq update-called t))))

      (shaoline--guard-visibility)
      (should update-called))))

;; ----------------------------------------------------------------------------
;; Test: Effect orchestration
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-strategy-application ()
  "Test strategy application orchestrates effects correctly."
  (setq shaoline--active-effects nil)

  ;; Mock resolve-setting for yang strategy
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting)
               (pcase setting
                 ('use-hooks t)
                 ('use-advice t)
                 ('use-timers t)
                 (_ nil)))))

    (shaoline--apply-strategy 'yang)
    (should (eq (shaoline--state-get :strategy) 'yang))))

;; ----------------------------------------------------------------------------
;; Test: Cleanup all effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-cleanup-all-effects ()
  "Test that cleanup properly clears all effects."
  ;; Set up some state
  (setq shaoline--active-effects '(display timer hook))
  (shaoline--state-put :last-content "some content")

  ;; Mock cleanup functions
  (cl-letf (((symbol-function 'shaoline--cleanup-all-timers) (lambda () nil))
            ((symbol-function 'shaoline--cleanup-all-hooks) (lambda () nil))
            ((symbol-function 'shaoline--cleanup-all-advice) (lambda () nil))
            ((symbol-function 'shaoline--clear-echo-area) (lambda () nil)))

    (shaoline--cleanup-all-effects)
    (should (null shaoline--active-effects))))

(provide 'shaoline-effects-test)
;;; shaoline-effects-test.el ends here
