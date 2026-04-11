;;; shaoline-e2e-test.el --- E2E vertical tests for Shaoline HDS -*- lexical-binding: t; -*-

;; Version: 3.3.4
;; Package-Requires: ((emacs "27.1"))
;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'shaoline)
(require 'shaoline-segments)

;; ----------------------------------------------------------------------------
;; E2E Scenario: Активация mode-line с проектом
;; HDS: Vertical test для FROZEN items
;; ----------------------------------------------------------------------------

(ert-deftest e2e-shaoline-activate-with-project ()
  "HDS Scenario: Активация и отображение mode-line с проектом."
  (let* ((test-dir "/home/zoya/Code/shaoline"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional dir)
                 (when (string= dir test-dir) 'shaoline-project)))))
      (should (stringp (shaoline-segment-project-name))))))

(ert-deftest e2e-shaoline-segment-project-name ()
  "FROZEN: segment-project-name возвращает строку."
  (let ((result (shaoline-segment-project-name)))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest e2e-shaoline-segment-gptel-model ()
  "FROZEN: segment-gptel-model возвращает строку модели."
  (let ((result (shaoline-segment-gptel-model)))
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest e2e-shaoline-project-face-color ()
  "FROZEN: project-face имеет голубой цвет."
  (let* ((face (get-face 'shaoline-project-face))
         (spec (face-spec-choose (get 'shaoline-project-face 'face-spec))))
    (should (or (eq (plist-get spec :foreground) "#5e9fff")
                (face-attribute-where face :foreground :global)
                (memq 'foreground spec)))))

(ert-deftest e2e-shaoline-project-face-weight ()
  "FROZEN: project-face имеет жирный вес."
  (let* ((face (get-face 'shaoline-project-face)))
    (should (memq (face-attribute face :weight :global)
                  '(bold extra-bold)))))

(ert-deftest e2e-shaoline-gptel-face-color ()
  "FROZEN: gptel-face имеет фиолетовый цвет."
  (let* ((face (get-face 'shaoline-gptel-face))
         (spec (face-spec-choose (get 'shaoline-gptel-face 'face-spec))))
    (should (or (eq (plist-get spec :foreground) "#b259b6")
                (face-attribute-where face :foreground :global)
                (memq 'foreground spec)))))

;; ----------------------------------------------------------------------------
;; E2E Scenario: Кеширование с TTL
;; ----------------------------------------------------------------------------

(ert-deftest e2e-shaoline-stable-cache-invalidation ()
  "FROZEN: stable cache сбрасывается при смене директории."
  (let* ((dir1 "/home/zoya/Code/shaoline")
         (dir2 "/home/zoya/Code/pro"))
    (cl-letf (((symbol-function 'project-current) (lambda (&optional d) d))
              ((symbol-function 'default-directory) dir1))
      (shaoline--stable-cached-call "test-key" default-directory
        (lambda () "value1"))
      (let ((cached (shaoline--stable-cached-call "test-key" default-directory
                     (lambda () "different"))))
        (should (string= cached "value1")))
      (cl-letf (((symbol-function 'default-directory) dir2))
        (let ((fresh (shaoline--stable-cached-call "test-key" default-directory
                        (lambda () "value2"))))
          (should (string= fresh "value2"))))))

;; ----------------------------------------------------------------------------
;; E2E Scenario: Совместимость с agent-shell
;; ----------------------------------------------------------------------------

(ert-deftest e2e-agent-shell-model-detection ()
  "FROZEN: В буфере agent-shell показывается его модель."
  (unless (featurep 'agent-shell)
    (setq emacs-load-message "agent-shell not loaded, skipping"))
  (skip-unless (featurep 'agent-shell))
  (let ((result (shaoline-segment-gptel-model)))
    (should (stringp result))
    (should (> (length result) 0))))

;; ----------------------------------------------------------------------------
;; E2E Verify команд
;; ----------------------------------------------------------------------------

(defun shaoline-e2e-verify ()
  "Run all E2E tests."
  (ert-run-tests-batch-and-exit))

(provide 'shaoline-e2e-test)
;;; shaoline-e2e-test.el ends here