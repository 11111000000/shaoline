;;; checkdoc-elisp.el --- Run elisp-lint (incl. checkdoc) in batch  -*- lexical-binding: t; -*-

(defun shaoline-checkdoc-elisp-lint ()
  "Install elisp-lint in isolated dir and run it over lisp/*.el with detailed output."
  (let* ((default-directory (or (getenv "GITHUB_WORKSPACE") default-directory))
         (package-user-dir (expand-file-name ".elpa-lint" default-directory)))
    (require 'package)
    ;; Use MELPA for elisp-lint
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (unless (package-installed-p 'elisp-lint)
      (package-refresh-contents)
      (package-install 'elisp-lint))
    (require 'elisp-lint)
    (let* ((dir   (expand-file-name "lisp" default-directory))
           (files (directory-files dir t "\\.el\\'"))
           ;; exit status we set only if our wrapper catches an error/returns non-zero
           (status 0))
      (condition-case err
          (let ((command-line-args-left files))
            (cond
             ;; Newer elisp-lint entrypoint that exits non-zero on failures
             ((fboundp 'elisp-lint-batch-and-exit)
              (elisp-lint-batch-and-exit))
             ;; Some versions provide this name
             ((fboundp 'elisp-lint-files-batch-and-exit)
              (elisp-lint-files-batch-and-exit))
             ;; Older versions accept argv from command-line-args-left, or no args at all
             ((fboundp 'elisp-lint-files-batch)
              (elisp-lint-files-batch))
             (t
              (error "No suitable elisp-lint batch entrypoint found"))))
        (error
         (setq status 1)
         (princ (format "elisp-lint failed: %S\n" err))))
      (kill-emacs status))))

;; Allow: emacs -Q --batch -l scripts/checkdoc-elisp.el -f shaoline-checkdoc-elisp-lint
(provide 'shaoline-checkdoc-elisp)
;;; checkdoc-elisp.el ends here
