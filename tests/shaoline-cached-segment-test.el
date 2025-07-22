;;; shaoline-cached-segment-test.el --- Unit tests for TTL-cached segments -*- lexical-binding: t; -*-

(require 'ert)
(require 'shaoline-segments)

(ert-deftest shaoline-segment-battery-cache-basic ()
  "Test that battery segment caches within TTL."
  (let* ((shaoline-battery-ttl 2)
         (orig (symbol-function 'shaoline--segment-battery-raw))
         (call-count 0))
    (cl-letf (((symbol-function 'shaoline--segment-battery-raw)
               (lambda () (cl-incf call-count) "BAT")))
      (setq call-count 0)
      (let ((out1 (shaoline-segment-battery)))
        (should (equal out1 "BAT"))
        (let ((out2 (shaoline-segment-battery)))
          (should (equal out2 "BAT"))
          (should (= call-count 1)))))
    ;; After a delay, cache must expire:
    (sleep-for 2.1)
    (setq call-count 0)
    (shaoline-segment-battery)
    (should (= call-count 1))))

;;; shaoline-cached-segment-test.el ends here
