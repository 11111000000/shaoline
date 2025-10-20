;;; fix-footers.el --- Normalize package footers and Local Variables -*- lexical-binding: t; -*-

(defun shaoline--fix-footer-in-current-buffer (feature-name &optional add-local-vars)
  "Ensure the last lines are:
  (provide 'FEATURE-NAME)
  ;;; FILE.el ends here
If ADD-LOCAL-VARS is non-nil, insert a proper Local Variables block
right before the footer with package-lint-main-file set to \"shaoline.el\".
Also remove any broken or duplicate footer/local variables fragments."
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (base (file-name-base file))
         (expected-provide (format "(provide '%s)" feature-name))
         (expected-ends (format ";;; %s.el ends here" base)))
    ;; Remove any trailing whitespace
    (goto-char (point-max))
    (delete-trailing-whitespace)

    ;; Remove any existing Local Variables block near end
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^[ \t]*;;[ \t]*Local Variables:" nil t)
        (let ((lv-beg (line-beginning-position)))
          (when (re-search-forward "^[ \t]*;;[ \t]*End:" nil t)
            (forward-line 0)
            (let ((lv-end (line-end-position)))
              (delete-region lv-beg lv-end)
              (delete-blank-lines))))))

    ;; Remove any existing provide/footer lines near the end to avoid duplicates
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n\t ")
      (forward-line 1)
      (let ((end (point-max)))
        (goto-char (point-max))
        (when (re-search-backward "^(provide '[^)]+)" nil t)
          (let ((p (line-beginning-position)))
            (goto-char (point-max))
            (delete-region p end)))))

    ;; Optionally insert a correct Local Variables block
    (when add-local-vars
      (goto-char (point-max))
      (insert "\n;; Local Variables:\n")
      (insert ";; package-lint-main-file: \"shaoline.el\"\n")
      (insert ";; End:\n"))

    ;; Insert canonical footer
    (goto-char (point-max))
    (insert (format "%s\n%s\n" expected-provide expected-ends))))

(defun shaoline-fix-footers ()
  "Normalize footer and Local Variables blocks for Shaoline submodules."
  (let* ((dir (expand-file-name "lisp" default-directory))
         (targets
          ;; feature-name and whether to add Local Variables block
          ;; We add Local Variables block only to multi-file helpers.
          '(("shaoline-effects.el" . (shaoline-effects . t))
            ("shaoline-segments.el" . (shaoline-segments . t))
            ("shaoline-strategy.el" . (shaoline-strategy . t))
            ("shaoline-mode.el" . (shaoline-mode . nil))
            ("shaoline.el" . (shaoline . nil))))
         (status 0))
    (dolist (entry targets)
      (let* ((fname (car entry))
             (spec (cdr entry))
             (feature (car spec))
             (addlv (cdr spec))
             (path (expand-file-name fname dir)))
        (when (file-exists-p path)
          (condition-case err
              (with-current-buffer (find-file-noselect path)
                (shaoline--fix-footer-in-current-buffer feature addlv)
                (save-buffer)
                (kill-buffer (current-buffer))
                (princ (format "Footer fixed: %s\n" path)))
            (error
             (setq status 1)
             (princ (format "Footer fix failed: %s\n  %S\n" path err)))))))
    (kill-emacs status)))

(provide 'shaoline-fix-footers)
;;; fix-footers.el ends here
