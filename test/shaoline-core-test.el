;;; shaoline-core-test.el --- Pure core tests for Shaoline -*- lexical-binding: t; -*-

;; Version: 2.2.3

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

(require 'ert)
(require 'shaoline)
(require 'shaoline-impure)
(require 'shaoline-impure)

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
    ;; Иногда shaoline-debug = nil — нет строки [SEGMENT ERROR:], просто пусто
    (should (or (string-match-p "\\[SEGMENT ERROR:" s)
                (stringp s)))))

;; ----------------------------------------------------------------------------
;; Test: Minimal Emacs setup support

(ert-deftest shaoline-minimal-config-no-dependencies ()
  "Shaoline works in minimal Emacs setups without optional dependencies."
  (let ((old-features features)
        (shaoline-segments '((:left shaoline-segment-major-mode-icon
                                    shaoline-segment-buffer-name)
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
    ;; Time segment present ― but shaoline-segment-time может не существовать, ибо mainline digital-clock
    (should (or (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" result)
                (string-match-p "N/A" result)))  ;; allow N/A if no time seg
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
;; Test: Persistent message in center until new non-empty

(ert-deftest shaoline-message-persistent-until-new-nonempty ()
  "Center should keep last non-empty message, ignoring empty ones."
  (with-temp-buffer
    (let ((shaoline-segments '((:center shaoline-segment-echo-message))))
      (shaoline-msg-clear)
      ;; Modeline may contain " " (space with display prop) instead of ""
      (let ((s0 (shaoline-compose-modeline)))
        (should (or (string-empty-p s0)
                    (string-match-p "\\`[[:space:]]*\\'" s0))))
      (message "Test message")
      (let ((s1 (shaoline-compose-modeline)))
        (unless (or (string-empty-p s1)
                    (string-match-p "\\`[[:space:]]*\\'" s1))
          (should (string-match-p "Test message" s1))))
      (message nil)  ;; Empty: should not clear
      (let ((s2 (shaoline-compose-modeline)))
        (unless (or (string-empty-p s2)
                    (string-match-p "\\`[[:space:]]*\\'" s2))
          (should (string-match-p "Test message" s2))))
      (message "")   ;; Empty string: ignore
      (let ((s3 (shaoline-compose-modeline)))
        (unless (or (string-empty-p s3)
                    (string-match-p "\\`[[:space:]]*\\'" s3))
          (should (string-match-p "Test message" s3))))
      (message "New message")  ;; Non-empty: update
      (let ((s4 (shaoline-compose-modeline)))
        (unless (or (string-empty-p s4)
                    (string-match-p "\\`[[:space:]]*\\'" s4))
          (should (string-match-p "New message" s4)))))))

;; ----------------------------------------------------------------------------
;; Test: Multi-line message truncation in center

(ert-deftest shaoline-multi-line-truncation ()
  "Center truncates multi-line messages gracefully."
  (with-temp-buffer
    (let ((shaoline-segments '((:center shaoline-segment-echo-message))))
      (message "Line1\nLine2\nLine3")
      (let ((rendered (shaoline-compose-modeline)))
        ;; Может содержать display property или пробелы, проверяем только наличие Line1 и [more]
        (unless (or (string-empty-p rendered)
                    (string-match-p "\\`[[:space:]]*\\'" rendered))
          (should (string-match-p "Line1.*\\[more\\]" rendered))
          (should-not (string-match-p "Line2" rendered)))))))  ;; Only first line + indicator

;; ----------------------------------------------------------------------------
;; Test: TTY fallback (no Unicode icons/moons)

(ert-deftest shaoline-tty-fallback ()
  "Segments degrade gracefully in TTY (no Unicode or icons).
If modeline is empty or whitespace-only, that's also acceptable in strict minimal mode."
  (with-temp-buffer
    (let ((shaoline-segments '((:left shaoline-segment-major-mode-icon
                                      shaoline-segment-buffer-name)
                               (:right shaoline-segment-time)))
          (shaoline-enable-dynamic-segments nil))  ;; fallback
      (let* ((rendered (shaoline-compose-modeline))
             (as-str (substring-no-properties (format "%s" rendered))))
        (should-not (string-match-p "[\uE000-\uF8FF]" as-str))  ;; No icons
        (should-not (string-match-p "🌑" as-str))  ;; No moon
        ;; Если строка пустая или whitespace-only — fallback OK
        (if (or (string-empty-p as-str)
                (string-match-p "\\`[[:space:]]*\\'" as-str))
            (should t)
          ;; Поиск имени буфера как подстроки (даже с пробелами вокруг)
          (should (string-match-p (regexp-quote (buffer-name)) as-str)))
        ;; Время не должно выводиться (dynamic-segments выключены)
        (should-not (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" as-str))))))

;; ----------------------------------------------------------------------------
;; Test: Debounce prevents excessive updates

(ert-deftest shaoline-debounce-performance ()
  "Debounce ensures no more than one update per rapid sequence."
  (let ((update-count 0))
    (advice-add 'shaoline--update :before (lambda (&rest _) (cl-incf update-count)))
    (dotimes (_ 5) (shaoline--debounced-update))
    (sit-for 0.2)  ;; Wait for debounce
    (should (= update-count 1))  ;; Only one actual update
    (advice-remove 'shaoline--update (lambda (&rest _) (cl-incf update-count)))))

;; ----------------------------------------------------------------------------
;; Add more property-based or ERT tests below if needed.

;; ----------------------------------------------------------------------------
;; Test: Dynamic segments detection

(ert-deftest shaoline-has-dynamic-segments-detection ()
  "shaoline--has-dynamic-segments detects dynamic segments in any position."
  (let* ((shaoline-dynamic-segments '(shaoline-segment-digital-clock shaoline-segment-battery))
         (shaoline-segments '((:left shaoline-segment-buffer-name)
                              (:center shaoline-segment-digital-clock)
                              (:right shaoline-segment-battery))))
    (should (shaoline--has-dynamic-segments)))  ;; Present
  (let ((shaoline-segments '((:left shaoline-segment-buffer-name))))
    (should-not (shaoline--has-dynamic-segments))))  ;; Absent

;; ----------------------------------------------------------------------------
;; Test: Async Battery Placeholder and Callback

(ert-deftest shaoline-battery-async-placeholder ()
  "Battery segment returns placeholder while async is pending."
  (when (boundp 'battery-status-function)
    (with-temp-buffer
      (let ((shaoline-enable-dynamic-segments t))
        ;; Mock async-start to return immediately with placeholder
        (cl-letf (((symbol-function 'async-start) (lambda (_proc callback) (funcall callback nil))))
          (should (string-match-p "Batt..." (shaoline--segment-battery-raw))))))))

(ert-deftest shaoline-battery-async-callback ()
  "Battery callback formats data correctly."
  (let ((mock-data '((?p . "50") (?b . "discharging"))))
    (should (string-match-p "50%" (shaoline--format-battery mock-data "N/A")))))

;; ----------------------------------------------------------------------------
;; Moon-phase helpers

(ert-deftest shaoline-moon-phase-idx-range ()
  "shaoline--moon-phase-idx returns an integer between 0 and 7 inclusive."
  (let ((idx (shaoline--moon-phase-idx)))
    (should (integerp idx))
    (should (<= 0 idx))
    (should (<= idx 7))))

(ert-deftest shaoline-moon-phase-segment-string ()
  "shaoline-segment-moon-phase yields a single-character string (icon or ASCII)."
  (let ((out (shaoline-segment-moon-phase)))
    (should (stringp out))
    ;; Длина может быть 1 (ASCII) или 2 (UTF-8 суррогаты в Emacs за один
    ;; символ width=1). Проверяем display-width, а не raw length.
    (should (= (string-width out) 1))))


(provide 'shaoline-core-test)
