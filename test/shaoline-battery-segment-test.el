;;; shaoline-battery-segment-test.el --- Battery segment tests for Shaoline 3.0 -*- lexical-binding: t; -*-

;; Version: 3.0.0-dao

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'shaoline)
(require 'shaoline-segments)

;; Ensure dynamic (special) var exists for tests; avoids void-variable when using 'symbol-value'.
(defvar battery-status-function nil)

;; ----------------------------------------------------------------------------
;; Test: Battery formatting function
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-format-battery-alist-data ()
  "Test battery formatting with alist data."
  ;; Normal battery data
  (let ((data '((112 . "85") (66 . "discharging"))))
    (let ((result (shaoline--format-battery data "N/A")))
      (should (stringp result))
      (should (string-match-p "85%" result))))

  ;; Numeric percentage (as returned by some systems)
  (let ((data '((112 . 75) (66 . "charging"))))
    (let ((result (shaoline--format-battery data "N/A")))
      (should (stringp result))
      (should (string-match-p "75%" result))))

  ;; Missing percentage
  (let ((data '((66 . "discharging"))))
    (let ((result (shaoline--format-battery data "Fallback")))
      (should (string= result "Fallback"))))

  ;; Empty alist
  (let ((data '()))
    (let ((result (shaoline--format-battery data "Empty")))
      (should (string= result "Empty")))))

(ert-deftest shaoline-format-battery-string-data ()
  "Test battery formatting with string data from battery() function."
  (let ((data "Power off-line, battery discharging (18% load, remaining time 0:39)"))
    (let ((result (shaoline--format-battery data "N/A")))
      (should (stringp result))
      (should (get-text-property 0 'face result)))))

(ert-deftest shaoline-format-battery-fallback ()
  "Test battery formatting fallback handling."
  ;; Nil data
  (let ((result (shaoline--format-battery nil "No Battery")))
    (should (string= result "No Battery")))

  ;; Invalid data
  (let ((result (shaoline--format-battery 'invalid "Invalid")))
    (should (string= result "Invalid"))))

;; ----------------------------------------------------------------------------
;; Test: Battery icons (GUI mode)
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-icons-gui ()
  "Test battery icon selection in GUI mode."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () t))
            ((symbol-function 'featurep)
             (lambda (feature) (eq feature 'all-the-icons)))
            ;; Mock all-the-icons functions
            ((symbol-function 'all-the-icons-faicon)
             (lambda (icon &rest args)
               (format "[%s-icon]" icon))))

    ;; Test different battery levels
    (let ((data-full '((112 . "95") (66 . "charging"))))
      (let ((result (shaoline--format-battery data-full "N/A")))
        (should (string-match-p "\\[battery-full-icon\\]" result))
        (should (string-match-p "95%" result))))

    (let ((data-low '((112 . "5") (66 . "discharging"))))
      (let ((result (shaoline--format-battery data-low "N/A")))
        (should (string-match-p "\\[battery-empty-icon\\]" result))
        (should (string-match-p "5%" result))))))

(ert-deftest shaoline-battery-tty-fallback ()
  "Test battery display in TTY mode (no icons)."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))

    (let ((data '((112 . "50") (66 . "discharging"))))
      (let ((result (shaoline--format-battery data "N/A")))
        ;; Should have percentage but no icon markup
        (should (string-match-p "50%" result))
        (should-not (string-match-p "\\[.*-icon\\]" result))))))

;; ----------------------------------------------------------------------------
;; Test: Battery segment function
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-segment-dynamic-disabled ()
  "Test battery segment when dynamic segments are disabled."
  (let ((shaoline-enable-dynamic-segments nil))
    (should (string= (shaoline-segment-battery) ""))))

(ert-deftest shaoline-battery-segment-no-battery-function ()
  "Test battery segment when battery functions are not available."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (not (memq sym '(battery)))))
            ((symbol-function 'boundp)
             (lambda (sym) (not (eq sym 'battery-status-function)))))
    (let ((shaoline-enable-dynamic-segments t))
      (let ((result (shaoline-segment-battery)))
        (should (stringp result))
        (should (string-match-p "N/A" result))))))

(ert-deftest shaoline-battery-segment-with-battery ()
  "Test battery segment with working battery function."
  (let ((shaoline-enable-dynamic-segments t)
        (battery-status-function (lambda () '((112 . "68") (66 . "discharging")))))

    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (memq sym '(battery async-start))))
              ((symbol-function 'boundp)
               (lambda (sym) (eq sym 'battery-status-function)))
              ;; Mock async-start to execute immediately
              ((symbol-function 'async-start)
               (lambda (start-func finish-func)
                 (funcall finish-func (funcall start-func)))))

      (let ((result (shaoline-segment-battery)))
        (should (stringp result))
        (should (string-match-p "68%" result))))))

;; ----------------------------------------------------------------------------
;; Test: Battery caching
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-segment-caching ()
  "Test that battery segment respects caching."
  (let ((shaoline-enable-dynamic-segments t)
        (call-count 0)
        (battery-status-function (lambda ()
                                   (cl-incf call-count)
                                   '((112 . "42") (66 . "charging")))))

    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (eq sym 'battery)))
              ((symbol-function 'boundp)
               (lambda (sym) (eq sym 'battery-status-function))))

      ;; Clear cache
      (shaoline--state-put :cache (make-hash-table :test 'equal))

      ;; First call should execute function
      (let ((result1 (shaoline-segment-battery)))
        (should (= call-count 1))
        (should (string-match-p "42%" result1)))

      ;; Second call within TTL should use cache
      (let ((result2 (shaoline-segment-battery)))
        (should (= call-count 1)) ; No additional calls
        (should (string-match-p "42%" result2))))))

;; ----------------------------------------------------------------------------
;; Test: Battery percentage parsing
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-percentage-parsing ()
  "Test percentage parsing from different formats."
  ;; Clean numeric string
  (let ((data '((112 . "85") (66 . "discharging"))))
    (let ((result (shaoline--format-battery data "N/A")))
      (should (string-match-p "85%" result))))

  ;; Dirty string with non-numeric characters
  (let ((data '((112 . "85% charged") (66 . "charging"))))
    (let ((result (shaoline--format-battery data "N/A")))
      (should (string-match-p "85%" result))))

  ;; Empty percentage string
  (let ((data '((112 . "") (66 . "unknown"))))
    (let ((result (shaoline--format-battery data "Fallback")))
      (should (string= result "Fallback")))))

;; ----------------------------------------------------------------------------
;; Test: Battery integration with different backends
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-upower-backend ()
  "Test battery segment with upower backend simulation."
  (let ((shaoline-enable-dynamic-segments t)
        (battery-status-function
         (lambda ()
           ;; Simulate upower output format
           '((99 . "7180") (114 . "10.3 W") (76 . "off-line")
             (66 . "discharging") (98 . "-") (100 . "N/A")
             (112 . "18") (115 . "2508") (109 . "41")
             (104 . "0") (116 . "0:41")))))

    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (eq sym 'battery)))
              ((symbol-function 'boundp)
               (lambda (sym) (eq sym 'battery-status-function))))

      (let ((result (shaoline-segment-battery)))
        (should (stringp result))
        (should (string-match-p "18%" result))))))

(ert-deftest shaoline-battery-linux-sysfs-backend ()
  "Test battery segment with linux sysfs backend simulation."
  (let ((shaoline-enable-dynamic-segments t)
        (battery-status-function
         (lambda ()
           ;; Simulate sysfs output format
           '((?L . "unknown") (?p . 55) (?b . "Charging")
             (?B . "51.2W") (?d . "4:32")))))

    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (eq sym 'battery)))
              ((symbol-function 'boundp)
               (lambda (sym) (eq sym 'battery-status-function))))

      (let ((result (shaoline-segment-battery)))
        (should (stringp result))
        (should (string-match-p "55%" result))))))

;; ----------------------------------------------------------------------------
;; Test: Battery error handling
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-error-handling ()
  "Test battery segment error handling."
  (let ((shaoline-enable-dynamic-segments t)
        (battery-status-function
         (lambda () (error "Battery unavailable"))))

    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (eq sym 'battery)))
              ((symbol-function 'boundp)
               (lambda (sym) (eq sym 'battery-status-function))))

      ;; Should not crash and return fallback
      (let ((result (shaoline-segment-battery)))
        (should (stringp result))))))

;; ----------------------------------------------------------------------------
;; Test: Battery cache expiration
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-battery-cache-expiration ()
  "Test that battery cache expires correctly."
  (let ((shaoline-enable-dynamic-segments t)
        (call-count 0)
        (battery-status-function (lambda ()
                                   (cl-incf call-count)
                                   `((112 . ,(number-to-string (+ 40 call-count)))
                                     (66 . "discharging")))))

    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (eq sym 'battery)))
              ((symbol-function 'boundp)
               (lambda (sym) (eq sym 'battery-status-function))))

      ;; Clear cache and set short TTL for testing
      (shaoline--state-put :cache (make-hash-table :test 'equal))

      ;; First call
      (let ((result1 (shaoline-segment-battery)))
        (should (= call-count 1))
        (should (string-match-p "41%" result1)))

      ;; Simulate cache expiration by manually clearing it
      (shaoline--state-put :cache (make-hash-table :test 'equal))

      ;; Should call function again
      (let ((result2 (shaoline-segment-battery)))
        (should (= call-count 2))
        (should (string-match-p "42%" result2))))))

(provide 'shaoline-battery-segment-test)
;;; shaoline-battery-segment-test.el ends here
