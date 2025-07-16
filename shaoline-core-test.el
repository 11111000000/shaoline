;;; shaoline-core-test.el --- Pure core tests for Shaoline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

(require 'ert)
(require 'shaoline)

;; ----------------------------------------------------------------------------
;; Test: Modeline always returns a string

(ert-deftest shaoline-compose-modeline-output-is-string ()
  "Shaoline modeline should always return a string, with no Emacs I/O."
  (let ((result (shaoline-compose-modeline)))
    (should (stringp result))))

;; ----------------------------------------------------------------------------
;; Test: Segment errors do not break modeline

(ert-deftest shaoline-compose-modeline-segment-errors-never-break ()
  "Segment errors do not break modeline output."
  (puthash 'crazy-segment
           (lambda (_buffer) (error "OOPS"))
           shaoline--segment-table)
  (let* ((shaoline-segments '((:left crazy-segment)))
         (s (shaoline-compose-modeline)))
    (should (string-match-p "\\[SEGMENT ERROR:" s))))

;; ----------------------------------------------------------------------------
;; Test: Minimal Emacs setup support

(ert-deftest shaoline-minimal-config-no-dependencies ()
  "Shaoline works in minimal Emacs setups without optional dependencies."
  (let ((old-features features)
        (shaoline-segments '((:left shaoline-segment-icon-and-buffer)
                             (:right shaoline-segment-battery shaoline-segment-time)))
        (result nil))
    ;; Simulate no optional features loaded
    (dolist (feat '(all-the-icons projectile battery))
      (setq features (delq feat features)))
    (with-temp-buffer
      (rename-buffer "test-buffer")
      (setq result (shaoline-compose-modeline (current-buffer))))
    (should (stringp result))
    ;; Should be no icons (Unicode private use)
    (should-not (string-match-p "[\uE000-\uF8FF]" result))
    ;; Fallback to buffer name
    (should (string-match-p "test-buffer" result))
    ;; Battery fallback
    (should (string-match-p "N/A" result))
    ;; Time segment present
    (should (string-match-p "%H:%M" result))
    ;; Restore features
    (setq features old-features)))

;; ----------------------------------------------------------------------------
;; Test: Debounce prevents rapid update flicker

(ert-deftest shaoline-debounce-no-flicker ()
  "Debounce prevents multiple rapid shaoline updates."
  (let ((shaoline--debounce-timer nil)
        (update-count 0))
    (advice-add 'shaoline--update :before (lambda (&rest _) (cl-incf update-count)))
    (shaoline--debounced-update)
    (shaoline--debounced-update)
    (sit-for 0.2)
    (should (= update-count 1))
    (advice-remove 'shaoline--update (lambda (&rest _) (cl-incf update-count)))))

;; ----------------------------------------------------------------------------
;; Add more property-based or ERT tests below if needed.

(provide 'shaoline-core-test)
