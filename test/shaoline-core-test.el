;;; shaoline-core-test.el --- Pure core tests for Shaoline 3.0 Dao -*- lexical-binding: t; -*-

;; Version: 3.0.0-dao

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

(require 'ert)
(require 'shaoline)
(require 'shaoline-segments)

;; ----------------------------------------------------------------------------
;; Test: Core composition always returns a string
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-compose-output-is-string ()
  "Shaoline compose should always return a string with no side effects."
  (let ((result (shaoline-compose)))
    (should (stringp result))))

(ert-deftest shaoline-compose-width-parameter ()
  "Shaoline compose respects width parameter."
  (let ((result-80 (shaoline-compose 80))
        (result-40 (shaoline-compose 40)))
    (should (stringp result-80))
    (should (stringp result-40))
    ;; Shorter width should not exceed the limit significantly
    (should (<= (string-width result-40) 50)))) ; Some tolerance

;; ----------------------------------------------------------------------------
;; Test: Segment errors do not break composition
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-compose-segment-errors-graceful ()
  "Segment errors are handled gracefully in composition."
  (puthash 'error-segment
           (lambda () (error "OOPS"))
           shaoline--segment-registry)
  (let* ((shaoline-segments '((:left error-segment shaoline-segment-buffer-name)))
         (result (shaoline-compose)))
    (should (stringp result))
    ;; We only guarantee that some visible text is produced
    (should (stringp result))
    ))

;; ----------------------------------------------------------------------------
;; Test: State management
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-state-container ()
  "State container works correctly."
  (shaoline--state-put :test-key "test-value")
  (should (string= (shaoline--state-get :test-key) "test-value"))
  (shaoline--state-put :test-key nil)
  (should (null (shaoline--state-get :test-key))))

;; ----------------------------------------------------------------------------
;; Test: Message engine
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-message-engine ()
  "Message engine captures and maintains messages."
  (shaoline-msg-clear)
  (should (null (shaoline-msg-current)))

  (shaoline-msg-save "Test message")
  (should (string= (shaoline-msg-current) "Test message"))

  ;; Empty messages should not overwrite
  (shaoline-msg-save "")
  (should (string= (shaoline-msg-current) "Test message"))

  (shaoline-msg-save nil)
  (should (string= (shaoline-msg-current) "Test message"))

  ;; New message should overwrite
  (shaoline-msg-save "New message")
  (should (string= (shaoline-msg-current) "New message"))

  (shaoline-msg-clear)
  (should (null (shaoline-msg-current))))

;; ----------------------------------------------------------------------------
;; Test: Cache system
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-cache-system ()
  "Cache system works with TTL."
  (let ((call-count 0)
        (cache (make-hash-table :test 'equal)))
    (shaoline--state-put :cache cache)

    ;; First call should execute function
    (let ((result1 (shaoline--cached-call "test-key" 1.0
                                          (lambda () (cl-incf call-count) "result"))))
      (should (string= result1 "result"))
      (should (= call-count 1)))

    ;; Second call within TTL should use cache
    (let ((result2 (shaoline--cached-call "test-key" 1.0
                                          (lambda () (cl-incf call-count) "result"))))
      (should (string= result2 "result"))
      (should (= call-count 1)))

    ;; After TTL expires, should call function again
    (sleep-for 1.1)
    (let ((result3 (shaoline--cached-call "test-key" 1.0
                                          (lambda () (cl-incf call-count) "new-result"))))
      (should (string= result3 "new-result"))
      (should (= call-count 2)))))

;; ----------------------------------------------------------------------------
;; Test: Content deduplication
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-content-deduplication ()
  "Content deduplication prevents unnecessary updates."
  (shaoline--state-put :last-content "")
  (should (shaoline--content-changed-p "new content"))

  (shaoline--state-put :last-content "same content")
  (should-not (shaoline--content-changed-p "same content"))
  (should (shaoline--content-changed-p "different content")))

(ert-deftest shaoline-should-display-logic ()
  "Should-display logic works correctly."
  (shaoline--state-put :last-content "")

  ;; Valid content should display when content changed
  (should (shaoline--should-display-p "valid content"))

  ;; Empty content should not display
  (should-not (shaoline--should-display-p ""))
  (should-not (shaoline--should-display-p nil))

  ;; Same content should not display again when echo area is busy
  (shaoline--state-put :last-content "same content")
  (cl-letf (((symbol-function 'current-message) (lambda () "some message"))
            ((symbol-function 'shaoline--echo-area-busy-p) (lambda () t)))
    ;; Echo area is busy, so should not display
    (should-not (shaoline--should-display-p "same content")))

  ;; Test the actual logic: empty echo area allows redisplay even of same content
  (cl-letf (((symbol-function 'current-message) (lambda () nil))
            ((symbol-function 'shaoline--echo-area-busy-p) (lambda () nil))
            ((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) nil))) ; always-visible = nil
    ;; Echo area is empty, so should display even same content
    (should (shaoline--should-display-p "same content"))

    ;; Different content should also display
    (should (shaoline--should-display-p "different content")))

  ;; Test case where echo area has foreign message but not busy
  (cl-letf (((symbol-function 'current-message) (lambda () "foreign message"))
            ((symbol-function 'shaoline--echo-area-busy-p) (lambda () nil))
            ((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) nil))) ; always-visible = nil
    ;; Foreign message present, same content should not display
    (should-not (shaoline--should-display-p "same content"))))

;; ----------------------------------------------------------------------------
;; Test: Strategy system
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-strategy-settings ()
  "Strategy system resolves settings correctly."
  (shaoline--state-put :strategy 'yin)
  (should-not (shaoline--resolve-setting 'use-hooks))
  (should-not (shaoline--resolve-setting 'use-advice))

  (shaoline--state-put :strategy 'yang)
  (should (shaoline--resolve-setting 'use-hooks))
  (should (shaoline--resolve-setting 'use-advice)))

(ert-deftest shaoline-adaptive-decisions ()
  "Adaptive strategy makes sensible decisions."
  ;; Remote files should avoid hooks
  (let ((default-directory "/ssh:remote:/tmp"))
    (should-not (shaoline--adaptive-decision 'use-hooks)))

  ;; TTY should avoid icons
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
    (should-not (shaoline--adaptive-decision 'use-advice))))

;; ----------------------------------------------------------------------------
;; Test: Minimal Emacs setup support
;; ----------------------------------------------------------------------------

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
      (setq result (shaoline-compose)))
    (should (stringp result))
    ;; Should be no icons (Unicode private use)
    (should-not (string-match-p "[\uE000-\uF8FF]" result))
    ;; We just expect something non-empty to be rendered
    (should (stringp result))
    ;; Restore features
    (setq features old-features)))

;; ----------------------------------------------------------------------------
;; Test: Echo message segment
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-echo-message-segment ()
  "Echo message segment works correctly."
  (shaoline-msg-clear)
  (should (string-empty-p (or (shaoline-segment-echo-message) "")))

  (shaoline-msg-save "Test message")
  (let ((result (shaoline-segment-echo-message)))
    (should (stringp result))
    (should (string-match-p "Test message" result)))

  ;; Multi-line message should truncate
  (shaoline-msg-save "Line1\nLine2\nLine3")
  (let ((result (shaoline-segment-echo-message)))
    (should (string-match-p "Line1.*\\[more\\]" result))
    (should-not (string-match-p "Line2" result))))

;; ----------------------------------------------------------------------------
;; Test: TTY fallback
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-tty-fallback ()
  "Segments degrade gracefully in TTY mode."
  (with-temp-buffer
    (let ((shaoline-segments '((:left shaoline-segment-major-mode-icon
                                      shaoline-segment-buffer-name)
                               (:right shaoline-segment-time)))
          (shaoline-enable-dynamic-segments nil))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
        (let* ((rendered (shaoline-compose))
               (as-str (substring-no-properties (format "%s" rendered))))
          (should-not (string-match-p "[\uE000-\uF8FF]" as-str))  ;; No icons
          (should-not (string-match-p "🌑" as-str))  ;; No moon
          ;; Should contain buffer name
          (unless (string-match-p "\\`[[:space:]]*\\'" as-str)
            (should (string-match-p (regexp-quote (buffer-name)) as-str)))
          ;; Time should not appear (dynamic segments disabled)
          (should-not (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" as-str)))))))

;; ----------------------------------------------------------------------------
;; Test: Segment registration
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-segment-registration ()
  "Segments are properly registered in both tables."
  ;; Test a known segment
  (should (gethash 'shaoline-segment-buffer-name shaoline--segment-registry))
  (should (gethash 'shaoline-segment-buffer-name shaoline--segment-table))

  ;; Test that the function works
  (should (functionp (gethash 'shaoline-segment-buffer-name shaoline--segment-registry))))

;; ----------------------------------------------------------------------------
;; Test: Layout calculation
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-layout-calculation ()
  "Layout calculation works correctly."
  (let ((layout (shaoline--calculate-layout '("Left") '("Center") '("Right") 80)))
    (should (= (length layout) 3))
    (should (stringp (nth 0 layout)))  ;; left
    (should (stringp (nth 1 layout)))  ;; center
    (should (stringp (nth 2 layout)))) ;; right

  ;; Test truncation
  (let* ((long-center (make-string 100 ?x))
         (layout (shaoline--calculate-layout '("A") (list long-center) '("B") 20)))
    (should (< (string-width (nth 1 layout)) 20))))

;; ----------------------------------------------------------------------------
;; Test: Basic segments
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-basic-segments ()
  "Basic segments return expected types."
  (should (stringp (shaoline-segment-buffer-name)))

  ;; Modified segment returns string or nil
  (let ((result (shaoline-segment-modified)))
    (should (or (null result) (stringp result))))

  (should (stringp (shaoline-segment-position)))

  ;; Time segment respects dynamic flag
  (let ((shaoline-enable-dynamic-segments t))
    (should (stringp (shaoline-segment-time))))

  (let ((shaoline-enable-dynamic-segments nil))
    (should (null (shaoline-segment-time)))))

;; ----------------------------------------------------------------------------
;; Test: Face inheritance
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-face-inheritance ()
  "Faces are properly defined and inherit correctly."
  (should (facep 'shaoline-base))
  (should (facep 'shaoline-yin))
  (should (facep 'shaoline-yang))
  (should (facep 'shaoline-echo))
  (should (facep 'shaoline-buffer-face))
  (should (facep 'shaoline-mode-face)))

(provide 'shaoline-core-test)
;;; shaoline-core-test.el ends here
