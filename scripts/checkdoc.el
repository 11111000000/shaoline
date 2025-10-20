;;; checkdoc.el --- Run checkdoc over lisp/ in batch  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defun shaoline-checkdoc-batch ()
  "Run checkdoc on all lisp/*.el; exit non-zero if any issues are found.
Capture and print all diagnostics emitted via `message' during checks."
  (require 'checkdoc)
  (require 'cl-lib)
  (let* ((dir   (expand-file-name "lisp" default-directory))
         (files (directory-files dir t "\\.el\\'"))
         ;; Ensure checkdoc creates diagnostic buffers when it wants to
         (checkdoc-create-error-buffer t)
         (status 0))
    (dolist (f files)
      (let (logs)
        (with-current-buffer (find-file-noselect f)
          ;; Capture all `message' output produced by checkdoc
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) logs)
                       ;; Also forward to real stderr/stdout for visibility, if desired:
                       ;; (apply #'princ (concat (apply #'format fmt args) "\n"))
                       )))
            (let ((found (checkdoc-current-buffer)))
              (when found
                (setq status 1)))))
        (when logs
          (princ (format ">> %s\n" f))
          (dolist (line (nreverse logs))
            (princ (format "  %s\n" line)))))
      (when (get-file-buffer f)
        (kill-buffer (get-file-buffer f))))
    (kill-emacs status)))

;; Allow: emacs -Q --batch -l scripts/checkdoc.el -f shaoline-checkdoc-batch
(provide 'shaoline-checkdoc)
;;; checkdoc.el ends here
