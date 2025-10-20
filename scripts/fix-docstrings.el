;;; fix-docstrings.el --- Heuristic docstring fixes  -*- lexical-binding: t; -*-

(defun shaoline--docstring-at-point-p ()
  "Return non-nil if point is at a docstring string."
  (and (nth 3 (syntax-ppss))                      ;; inside string
       (save-excursion
         (ignore-errors
           (backward-up-list 1)
           (forward-char 1)
           (let ((sym (symbol-at-point)))
             (and sym
                  (memq sym
                        '(defun defmacro defsubst define-minor-mode
                                define-globalized-minor-mode defvar defconst defcustom))))))))

(defun shaoline-fix-docstrings ()
  "Fix common checkdoc issues in docstrings across lisp/*.el.
Currently replaces 'symbol' with =symbol' inside real docstrings.
Skips autoloads and -pkg files."
  (let* ((dir   (expand-file-name "lisp" default-directory))
         (files (directory-files dir t "\\.el\\'"))
         (status 0))
    (dolist (f files)
      (condition-case err
          (unless (string-match-p "\\(autoloads\\.el\\|-pkg\\.el\\)\\'" (file-name-nondirectory f))
            (with-current-buffer (find-file-noselect f)
              (emacs-lisp-mode)
              (goto-char (point-min))
              (while (re-search-forward "\"\\([^\"\\]\\|\\\\.\\)*\"" nil t)
                (when (shaoline--docstring-at-point-p)
                  ;; Work within string content (exclude quotes)
                  (let* ((beg (1+ (match-beginning 0)))
                         (end (1- (match-end 0))))
                    (save-excursion
                      (save-restriction
                        (narrow-to-region beg end)
                        (goto-char (point-min))
                        ;; 'foo' -> =foo'
                        (while (re-search-forward "'\\([A-Za-z0-9-]+\\)'" nil t)
                          (replace-match "=\\1'" t)))))))
              (save-buffer)
              (kill-buffer (current-buffer))
              (princ (format "Docstrings fixed (heuristic): %s\n" f))))
        (error
         (setq status 1)
         (princ (format "Docstring fix failed: %s\n  %S\n" f err)))))
    (kill-emacs status)))

(provide 'shaoline-fix-docstrings)
;;; fix-docstrings.el ends here
