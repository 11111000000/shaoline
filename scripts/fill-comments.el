;;; fill-comments.el --- Reflow ;; comment paragraphs to 70 cols  -*- lexical-binding: t; -*-

(defun shaoline-fill-comments ()
  "Fill only comment paragraphs in lisp/*.el to 70 columns.
Skips autoloads and -pkg files. Processes whole comment blocks to avoid loops."
  (let* ((dir   (expand-file-name "lisp" default-directory))
         (files (directory-files dir t "\\.el\\'"))
         (status 0))
    (dolist (f files)
      (condition-case err
          (unless (string-match-p "\\(autoloads\\.el\\|-pkg\\.el\\)\\'" (file-name-nondirectory f))
            (with-current-buffer (find-file-noselect f)
              (emacs-lisp-mode)
              (setq-local fill-column 70)
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*;;" nil t)
                (beginning-of-line)
                ;; Skip Local Variables block entirely (do not reflow it)
                (if (looking-at "^[ \t]*;;[ \t]*Local Variables:")
                    (progn
                      (forward-line 1)
                      (while (and (not (eobp))
                                  (not (looking-at "^[ \t]*;;[ \t]*End:")))
                        (forward-line 1))
                      ;; Skip the End: line too
                      (when (looking-at "^[ \t]*;;[ \t]*End:")
                        (forward-line 1)))
                  (let ((para-beg (point)))
                    ;; Extend to the end of this contiguous comment block
                    (while (and (not (eobp))
                                (looking-at "^[ \t]*;;"))
                      (forward-line 1))
                    (let ((para-end (point)))
                      (save-restriction
                        (narrow-to-region para-beg para-end)
                        (goto-char (point-min))
                        (fill-region (point-min) (point-max)))
                      ;; Continue after the processed block
                      (goto-char para-end)))))
              (save-buffer)
              (kill-buffer (current-buffer))
              (princ (format "Comments filled: %s\n" f))))
        (error
         (setq status 1)
         (princ (format "Fill comments failed: %s\n  %S\n" f err)))))
    (kill-emacs status)))

(provide 'shaoline-fill-comments)
;;; fill-comments.el ends here
