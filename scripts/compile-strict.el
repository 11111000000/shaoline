;;; compile-strict.el --- Byte-compile with warnings as errors  -*- lexical-binding: t; -*-

(defun shaoline-byte-compile-strict ()
  "Byte-compile all lisp/*.el with warnings as errors.
Exits with non-zero status when any warning/error is signaled."
  (let* ((byte-compile-error-on-warn t)
         (load-prefer-newer t)
         (dir   (expand-file-name "lisp" default-directory))
         (files (directory-files dir t "\\.el\\'"))
         (status 0))
    (dolist (f files)
      (condition-case err
          (progn
            (byte-compile-file f)
            (princ (format "Compiled OK: %s\n" f)))
        (error
         (princ (format "Compile FAILED: %s\n  %S\n" f err))
         (setq status 1))))
    (kill-emacs status)))

;; Allow: emacs -Q --batch -l scripts/compile-strict.el -f shaoline-byte-compile-strict
(provide 'shaoline-compile-strict)
;;; compile-strict.el ends here
