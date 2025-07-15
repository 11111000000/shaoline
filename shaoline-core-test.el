;;; shaoline-core-test.el --- Tests for Shaoline pure core functions -*- lexical-binding: t; -*-

(require 'ert)
(require 'shaoline) ;; or relative require if used as submodule

(ert-deftest shaoline-compose-modeline-output-is-string ()
  "shaoline-compose-modeline должен возвращать строку без вызова Emacs I/O."
  (let ((result (shaoline-compose-modeline)))
    (should (stringp result))))

(ert-deftest shaoline-compose-modeline-segment-errors-never-break ()
  "Ошибки сегмента не должны сбивать весь вывод."
  (puthash 'crazy-segment
           (lambda (_buffer) (error "OOPS"))
           shaoline--segment-table)
  (let* ((shaoline-segments '((:left crazy-segment)))
         (s (shaoline-compose-modeline)))
    (should (string-match-p "\\[SEGMENT ERROR:" s))))

(ert-deftest shaoline-minimal-config-no-dependencies ()
  "Test that Shaoline works in minimal Emacs without optional dependencies."
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
    (should-not (string-match-p "[\uE000-\uF8FF]" result))  ;; No icons (Unicode private use area for all-the-icons)
    (should (string-match-p "test-buffer" result))  ;; Fallback to buffer name
    (should (string-match-p "N/A" result))  ;; Battery fallback
    (should (string-match-p "%H:%M" result))  ;; Time present, moon may or may not be (assuming calendar loads)
    ;; Restore features
    (setq features old-features)))

;;; Add more property-based or ERT tests here as needed

(provide 'shaoline-core-test)
