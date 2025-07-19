;;; custom-segments.el --- Example custom segments for Shaoline -*- lexical-binding: t; -*-

;; This is a template for user-defined segments. Copy and customize as needed.

(require 'shaoline-macros)  ;; For shaoline-define-segment

(shaoline-define-segment my-custom-segment (buffer)
  "Example: Returns buffer size."
  (format "Size: %d bytes" (buffer-size buffer)))

;; Add to configuration:
;; (add-to-list 'shaoline-segments '(:right my-custom-segment))

(provide 'custom-segments)
;;; custom-segments.el ends here
