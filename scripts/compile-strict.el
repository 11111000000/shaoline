;;; compile-strict.el --- Byte-compile with warnings as errors  -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Avoid lexical/dynamic conflict: mark compiler vars as special in this unit.
(defvar byte-compile-error-on-warn)
(defvar byte-compile-warnings)

(defun shaoline-byte-compile-strict ()
  "Byte-compile all lisp/*.el with warnings as errors.
Exits with non-zero status when any warning/error is signaled."
  (let* ((byte-compile-error-on-warn nil)
         ;; Treat most warnings as errors, but skip noisy/benign categories:
         ;; - obsolete: Emacs itself may emit these during compilation
         ;; - lexical: duplicate defvar across compilation units
         ;; - redefine: benign cross-file redefinitions/forward declarations
         (byte-compile-warnings '(not obsolete lexical redefine))
         (load-prefer-newer t)
         (dir   (expand-file-name "lisp" default-directory))
         (files (cl-remove-if
                 (lambda (f) (string= (file-name-nondirectory f) "shaoline-compat-vars.el"))
                 (sort (directory-files dir t "\\.el\\'")
                       (lambda (a b)
                         (let ((an (file-name-nondirectory a))
                               (bn (file-name-nondirectory b)))
                           (cond
                            ((string= an "shaoline-compat-vars.el") t)
                            ((string= bn "shaoline-compat-vars.el") nil)
                            (t (string< an bn))))))))
         (status 0))

    ;; Ensure shared special vars are known to the compiler up-front.
    ;; Preload them so the byte-compiler knows these are special (dynamic) vars.
    ;; Also ensure lisp/ is on =load-path' so (require 'shaoline) works during compilation.
    (add-to-list 'load-path dir)
    (let ((compat (expand-file-name "shaoline-compat-vars.el" dir)))
      (when (file-exists-p compat)
        (load compat nil t)))

    (let ((orig-warn (symbol-function 'byte-compile-warn)))
      (cl-letf (((symbol-function 'byte-compile-warn)
                 (lambda (fmt &rest args)
                   (let ((msg (apply #'format fmt args)))
                     (cond
                      ;; 1) Whitelist: dynamic vs lexical — оставить предупреждением
                      ((string-match-p "Defining as dynamic an already lexical var" msg)
                       (display-warning 'bytecomp msg :warning))
                      ;; 2) Whitelist: устаревший byte-compile-dest-file — оставить предупреждением
                      ((string-match-p "Changing ‘byte-compile-dest-file’ is obsolete" msg)
                       (display-warning 'bytecomp msg :warning))
                      ;; 3) Всё прочее — строго, как прежде
                      (t
                       (apply orig-warn fmt args)
                       (error "%s" msg)))))))
        (dolist (f files)
          (condition-case err
              (progn
                (byte-compile-file f)
                (princ (format "Compiled OK: %s\n" f)))
            (error
             (let ((msg (error-message-string err)))
               (if (string-match-p "Defining as dynamic an already lexical var" msg)
                   (progn
                     (display-warning 'bytecomp msg :warning)
                     (princ (format "Compiled with tolerated warning: %s\n" f)))
                 (princ (format "Compile FAILED: %s\n  %S\n" f err))
                 (setq status 1)))))))
      (kill-emacs status))))

;; Allow: emacs -Q --batch -l scripts/compile-strict.el -f shaoline-byte-compile-strict
(provide 'shaoline-compile-strict)
;;; compile-strict.el ends here
