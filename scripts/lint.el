;;; scripts/lint.el --- Run package-lint in batch mode for Shaoline  -*- lexical-binding: t; -*-

(defun shaoline-package-lint-batch ()
  "Install package-lint (in isolated dir) and lint all files under lisp/."
  (let* ((default-directory (or (getenv "GITHUB_WORKSPACE") default-directory))
         (package-user-dir (expand-file-name ".elpa-lint" default-directory)))
    (require 'package)
    (require 'cl-lib)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (unless (package-installed-p 'package-lint)
      (package-refresh-contents)
      (package-install 'package-lint))
    (require 'package-lint)
    (let* ((dir (expand-file-name "lisp" default-directory))
           (files (cl-remove-if
                   (lambda (f)
                     (let ((bn (file-name-nondirectory f)))
                       (or (string-match-p "\\(autoloads\\.el\\|-pkg\\.el\\)\\'" bn)
                           (string= bn "shaoline-compat-vars.el"))))
                   (directory-files dir t "\\.el\\'")))
           (status 0))
      (dolist (f files)
        (with-current-buffer (find-file-noselect f)
          (let ((res (package-lint-buffer)))
            (when res
              (princ (format "%s: %S\n" f res))
              (setq status 1))))
        (when (get-file-buffer f)
          (kill-buffer (get-file-buffer f))))
      (kill-emacs status))))

;; Allow running directly via: emacs -Q --batch -l scripts/lint.el -f shaoline-package-lint-batch
(provide 'shaoline-lint)
;;; lint.el ends here
