;;; format-tabs.el --- Convert leading spaces to tabs in lisp/*.el  -*- lexical-binding: t; -*-

(defun shaoline-format-tabs ()
  "Convert leading spaces to tabs for all files under lisp/*.el.
Only leading indentation is tabified; inner spaces are preserved."
  (let* ((dir   (expand-file-name "lisp" default-directory))
         (files (directory-files dir t "\\.el\\'"))
         (status 0))
    (dolist (f files)
      (condition-case err
          (with-current-buffer (find-file-noselect f)
            ;; Tabify only leading indentation
            (let ((tabify-regexp "^[ \t]+"))
              (tabify (point-min) (point-max)))
            (save-buffer)
            (kill-buffer (current-buffer))
            (princ (format "Tabified leading indentation: %s\n" f)))
        (error
         (setq status 1)
         (princ (format "Tabify failed: %s\n  %S\n" f err)))))
    (kill-emacs status)))

;; Allow: emacs -Q --batch -l scripts/format-tabs.el -f shaoline-format-tabs
(provide 'shaoline-format-tabs)
;;; format-tabs.el ends here
