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
    (require 'cl-lib)
    (let* ((dir   (expand-file-name "lisp" default-directory))
           ;; Исключаем любые autoloads (*.el, *-autoloads.el, lisp-autoloads.el) и -pkg.el
           (files (cl-remove-if
                   (lambda (f)
                     (string-match-p
                      "\\(\\(?:-\\|\\)autoloads\\.el\\'\\)\\|\\(-pkg\\.el\\'\\)"
                      (file-name-nondirectory f)))
                   (directory-files dir t "\\.el\\'")))
           ;; Запускаем только checkdoc (при желании добавьте check-declare)
           (elisp-lint-validators '(checkdoc))
           (elisp-lint-ignored-validators nil)
           (elisp-lint-file-ignored-regexps '(".*autoloads\\.el\\'" ".*-pkg\\.el\\'"))
           ;; exit status we set only if our wrapper catches an error/returns non-zero
           (status 0))
      (condition-case err
          ;; Ограничим валидаторы до checkdoc через CLI-флаги: отключим шумные проверки.
          (let ((command-line-args-left
                 (append '("--no-indent"
                           "--no-indent-character"
                           "--no-fill-column"
                           "--no-byte-compile"
                           "--no-package-lint"
                           "--no-check-declare")  ;; при желании уберите этот флаг
                         files)))
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
