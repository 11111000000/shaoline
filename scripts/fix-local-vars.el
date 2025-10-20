;;; fix-local-vars.el --- Check & report Local Variables blocks -*- lexical-binding: t; -*-

(defun shaoline-check-local-vars ()
  "Report files with improperly terminated Local Variables blocks in lisp/."
  (let* ((dir (expand-file-name "lisp" default-directory))
         (files (directory-files dir t "\\.el\\'"))
         (bad 0))
    (dolist (f files)
      (with-current-buffer (find-file-noselect f)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*;;[ \t]*Local Variables:" nil t)
          (let ((ok (save-excursion
                      (re-search-forward "^[ \t]*;;[ \t]*End:" nil t))))
            (unless ok
              (setq bad (1+ bad))
              (princ (format "Local Variables not terminated: %s\n" f)))))
        (kill-buffer (current-buffer))))
    (kill-emacs (if (> bad 0) 1 0))))

(provide 'shaoline-fix-local-vars)
;;; fix-local-vars.el ends here
