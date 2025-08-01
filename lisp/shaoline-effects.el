;;; shaoline-effects.el --- Effect system for Shaoline -*- lexical-binding: t; -*-

;; Version: 3.0.0

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; All impure effects isolated here — the single place where Shaoline
;; touches the outside world. Like a temple gate: all must pass through,
;; but the inner sanctuary remains pure.

;;; Code:

(require 'shaoline)

;; ----------------------------------------------------------------------------
;; Effect Registry — All World Changes Flow Here
;; ----------------------------------------------------------------------------

(defvar shaoline--active-effects nil
  "List of currently active effects.")

(defmacro shaoline-defeffect (name args docstring &rest body)
  "Define an effect NAME that changes the world."
  (declare (indent defun))
  `(defun ,name ,args
     ,docstring
     (shaoline--log-effect ',name)
     ,@body))

;; ----------------------------------------------------------------------------
;; Display Effects — Echo Area Manifestation
;; ----------------------------------------------------------------------------

(shaoline-defeffect shaoline--display (content)
  "Display CONTENT in echo area with Shaoline tagging."
  (shaoline--log "shaoline--display called in buffer: %s, content: %s" (buffer-name) content)
  (when (shaoline--should-display-p content)
    (shaoline--state-put :last-content content)
    (let* ((tagged (propertize content 'shaoline-origin t))
           (message-log-max nil))
      (shaoline--log "shaoline--display actually displaying: %s" tagged)
      (message "%s" tagged)
      (push 'display shaoline--active-effects))))

(shaoline-defeffect shaoline--clear-echo-area ()
  "Clear echo area if it contains Shaoline content."
  (when (and (current-message)
             (get-text-property 0 'shaoline-origin (current-message)))
    (message nil)
    (shaoline--state-put :last-content "")
    (push 'clear shaoline--active-effects)))

;; ----------------------------------------------------------------------------
;; Timer Effects — Temporal Manifestations
;; ----------------------------------------------------------------------------

(defvar shaoline--timer-registry (make-hash-table)
  "Registry of active timers.")

(shaoline-defeffect shaoline--start-timer (name interval repeating-p function)
  "Start timer NAME with INTERVAL calling FUNCTION."
  (shaoline--stop-timer name) ; Cancel existing
  (let ((timer (run-with-timer interval
                               (when repeating-p interval)
                               function)))
    (puthash name timer shaoline--timer-registry)
    (push `(timer . ,name) shaoline--active-effects)))

(shaoline-defeffect shaoline--stop-timer (name)
  "Stop timer NAME if running."
  (when-let ((timer (gethash name shaoline--timer-registry)))
    (cancel-timer timer)
    (remhash name shaoline--timer-registry)))

(defun shaoline--cleanup-all-timers ()
  "Clean up all Shaoline timers."
  (maphash (lambda (name timer)
             (cancel-timer timer))
           shaoline--timer-registry)
  (clrhash shaoline--timer-registry))

;; ----------------------------------------------------------------------------
;; Hook Effects — Event Flow Manipulation
;; ----------------------------------------------------------------------------

(defvar shaoline--hook-registry nil
  "Registry of attached hooks.")

(shaoline-defeffect shaoline--attach-hook (hook-name function)
  "Attach FUNCTION to HOOK-NAME."
  (add-hook hook-name function)
  (push (cons hook-name function) shaoline--hook-registry)
  (push `(hook . ,hook-name) shaoline--active-effects))

(shaoline-defeffect shaoline--detach-hook (hook-name function)
  "Detach FUNCTION from HOOK-NAME."
  (remove-hook hook-name function)
  (setq shaoline--hook-registry
        (assq-delete-all hook-name shaoline--hook-registry)))

(defun shaoline--cleanup-all-hooks ()
  "Clean up all Shaoline hooks."
  (dolist (entry shaoline--hook-registry)
    (remove-hook (car entry) (cdr entry)))
  (setq shaoline--hook-registry nil))

;; ----------------------------------------------------------------------------
;; Advice Effects — Function Flow Interception
;; ----------------------------------------------------------------------------

(defvar shaoline--advice-registry nil
  "Registry of attached advice.")

(shaoline-defeffect shaoline--attach-advice (function how advice-fn)
  "Attach ADVICE-FN to FUNCTION with HOW method."
  (advice-add function how advice-fn)
  (push (list function how advice-fn) shaoline--advice-registry)
  (push `(advice . ,function) shaoline--active-effects))

(shaoline-defeffect shaoline--detach-advice (function advice-fn)
  "Detach ADVICE-FN from FUNCTION."
  (advice-remove function advice-fn)
  (setq shaoline--advice-registry
        (cl-remove-if (lambda (entry)
                        (and (eq (car entry) function)
                             (eq (caddr entry) advice-fn)))
                      shaoline--advice-registry)))

(defun shaoline--cleanup-all-advice ()
  "Clean up all Shaoline advice."
  (dolist (entry shaoline--advice-registry)
    (advice-remove (car entry) (caddr entry)))
  (setq shaoline--advice-registry nil))

;; ----------------------------------------------------------------------------
;; Mode Line Effects — Traditional Display Manipulation
;; ----------------------------------------------------------------------------

;; Mode Line Effects — Traditional Display Manipulation
;; ----------------------------------------------------------------------------

(defvar shaoline--modeline-backup-registry (make-hash-table :weakness 'key)
  "Registry mapping buffers to their original mode-line-format.
Uses weak references so buffers can be garbage collected normally.")

(shaoline-defeffect shaoline--hide-mode-line ()
  "Hide traditional mode-line in current buffer."
  (unless (gethash (current-buffer) shaoline--modeline-backup-registry)
    ;; Save original mode-line-format
    (puthash (current-buffer) mode-line-format shaoline--modeline-backup-registry)
    (setq mode-line-format nil)
    (force-mode-line-update)
    (push `(modeline . ,(current-buffer)) shaoline--active-effects)))

(shaoline-defeffect shaoline--restore-mode-line ()
  "Restore traditional mode-line in current buffer."
  (when-let ((original (gethash (current-buffer) shaoline--modeline-backup-registry)))
    (setq mode-line-format original)
    (remhash (current-buffer) shaoline--modeline-backup-registry)
    (force-mode-line-update)))

(defun shaoline--hide-mode-lines-globally ()
  "Hide mode-lines in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (shaoline--hide-mode-line))))

(defun shaoline--restore-mode-lines-globally ()
  "Restore mode-lines in all buffers."
  (dolist (buffer (buffer-list))
    (when (gethash buffer shaoline--modeline-backup-registry)
      (with-current-buffer buffer
        (shaoline--restore-mode-line)))))

(defun shaoline--cleanup-all-modelines ()
  "Clean up all modeline effects."
  (maphash (lambda (buffer original)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq mode-line-format original)
                 (force-mode-line-update))))
           shaoline--modeline-backup-registry)
  (clrhash shaoline--modeline-backup-registry))

;; ----------------------------------------------------------------------------
;; Effect Orchestration — Coordinated World Changes
;; ----------------------------------------------------------------------------

(defun shaoline--apply-strategy (strategy)
  "Apply STRATEGY by orchestrating appropriate effects."
  (shaoline--cleanup-all-effects) ; Clean slate

  (when (shaoline--resolve-setting 'use-hooks)
    (shaoline--attach-hook 'post-command-hook #'shaoline-update)
    (shaoline--attach-hook 'after-save-hook #'shaoline-update)
    (shaoline--attach-hook 'window-configuration-change-hook #'shaoline-update)

    (shaoline--attach-hook 'pre-command-hook #'shaoline--capture-prefix-keys))

  (when (shaoline--resolve-setting 'use-advice)
    (shaoline--attach-advice #'message :around #'shaoline--advice-capture-message))

  (when (shaoline--resolve-setting 'use-timers)
    (shaoline--start-timer 'update 1.0 t #'shaoline-update)

    (shaoline--start-timer 'guard 0.15 t #'shaoline--guard-visibility))

  (when (shaoline--resolve-setting 'hide-modelines)
    (shaoline--hide-mode-lines-globally))

  (shaoline--state-put :strategy strategy))

(defun shaoline--cleanup-all-effects ()
  "Clean up all active effects — return to stillness."
  (shaoline--cleanup-all-timers)
  (shaoline--cleanup-all-hooks)
  (shaoline--cleanup-all-advice)
  (shaoline--cleanup-all-modelines)
  (shaoline--clear-echo-area)
  (setq shaoline--active-effects nil))

;; ----------------------------------------------------------------------------
;; Message Capture Advice
;; ----------------------------------------------------------------------------

(defun shaoline--advice-capture-message (orig-fn format-string &rest args)
  "Capture message content before display, excluding Shaoline's own messages."
  (let ((content (when format-string
                   (apply #'format format-string args))))
    (unless (and content
                 (stringp content)
                 (get-text-property 0 'shaoline-origin content))
      (when content
        (shaoline-msg-save content)
        (when (and (shaoline--resolve-setting 'always-visible)
                   (not (minibufferp))
                   (not (active-minibuffer-window)))
          (run-with-timer 0.05 nil #'shaoline-update))))
    (apply orig-fn format-string args)))

;; ----------------------------------------------------------------------------
;; Prefix Key Capture — Yang Mode Enhancement
;; ----------------------------------------------------------------------------

(defun shaoline--capture-prefix-keys ()
  "Capture prefix keys for display in yang mode."
  (when (and (vectorp (this-command-keys))
             (> (length (this-command-keys)) 0))
    (let* ((keys (this-command-keys))
           (key-desc (key-description keys)))
      (when (string-match-p "^C-\\|^M-\\|^s-\\|^H-" key-desc)
        ;; This is a prefix key, save it as a message
        (shaoline-msg-save (format "Key: %s" key-desc))))))

;; ----------------------------------------------------------------------------
;; Guard Timer — Gentle Persistence
;; ----------------------------------------------------------------------------

(defun shaoline--guard-visibility ()
  "Gentle guardian ensuring Shaoline remains visible when appropriate."
  (when (and (shaoline--resolve-setting 'always-visible)
             (not (shaoline--echo-area-busy-p)))
    (shaoline-update)))


;; ----------------------------------------------------------------------------
;; Effect Logging — Observing Changes
;; ----------------------------------------------------------------------------

(defvar shaoline--effect-log nil
  "Log of all effects applied.")

(defun shaoline--log-effect (effect)
  "Log EFFECT application."
  (push (list effect (current-time)) shaoline--effect-log)
  (when (> (length shaoline--effect-log) 100)
    (setq shaoline--effect-log (butlast shaoline--effect-log 50))))

(provide 'shaoline-effects)
;;; shaoline-effects.el ends here
