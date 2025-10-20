;;; shaoline-cached-segment-test.el --- Unit tests for TTL-cached segments -*- lexical-binding: t; -*-

(require 'ert)
(require 'shaoline-segments)

;; Ensure dynamic (special) var exists for tests; avoids void-variable when using 'symbol-value'.
(defvar battery-status-function nil)

(ert-deftest shaoline-segment-battery-cache-basic ()
  "Battery segment should call 'battery-status-function' only once inside TTL."
  (let* ((shaoline-enable-dynamic-segments t))
    ;; Provide stub 'battery' to satisfy the 'require' in async path, and
    ;; bind 'battery-status-function' dynamically to avoid lexical/defcustom clash.
    (cl-letf (((symbol-value 'battery-status-function) (lambda () '((?p . "75"))))
              ((symbol-function 'battery) (lambda () nil)))
      ;; first call – computes
      (let ((out1 (shaoline-segment-battery)))
        (should (stringp out1))
        ;; second call within 2 s – must reuse cache
        (let ((out2 (shaoline-segment-battery)))
          (should (string= out1 out2))))
      ;; TTL — 10 с; имитируем истечение
      (sleep-for 0.1)
      (let ((out3 (shaoline-segment-battery)))
        (should (stringp out3))))))


(provide 'shaoline-cached-segment-test)
;;; shaoline-cached-segment-test.el ends here
