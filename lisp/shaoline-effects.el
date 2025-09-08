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

;; Failsafe: ensure the counter variable exists even
;; if shaoline.el hasn’t been (re)loaded yet.
(defvar shaoline--echo-area-input-depth 0)

;; Dynamic gate-keeper: when non-nil, Shaoline temporarily *allows*
;; `(message nil)' / "" to pass through untouched.  Use it via
;;
;;   (let ((shaoline--allow-empty-message t)) (message nil))
;;
;; so that legitimate, intentional clears do not get blocked.
(defvar shaoline--allow-empty-message nil)

;; ----------------------------------------------------------------------------
;; Effect Registry and Variables — All World Changes Flow Here
;; ----------------------------------------------------------------------------

(defvar shaoline--active-effects nil
  "List of currently active effects.")

(defvar shaoline--modeline-backup-registry (make-hash-table :weakness 'key)
  "Registry mapping buffers to their original mode-line-format.
Uses weak references so buffers can be garbage collected normally.")

(defvar shaoline--original-default-modeline nil
  "Backup of the original default mode-line-format.")

;; Buffer-local backup used by per-buffer hide/restore helpers
;; (unit tests look for this variable).
(defvar-local shaoline--original-mode-line nil
  "Original mode-line-format saved before Shaoline hides it.")

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
  "Display CONTENT in echo area with Shaoline tagging, избегая лишних перерисовок."
  (shaoline--log "shaoline--display called in buffer: %s, content: %s" (buffer-name) content)
  (when (and (shaoline--should-display-p content)
             (not (equal content (current-message))))   ; уже показываем? не трогаем.
    (shaoline--state-put :last-content content)
    (setq shaoline--last-display-time (float-time))
    (let* ((tagged (propertize content 'shaoline-origin t))
           (message-log-max nil))
      (shaoline--log "shaoline--display actually displaying: %s" tagged)
      (message "%s" tagged)
      (push 'display shaoline--active-effects))))

(shaoline-defeffect shaoline--clear-echo-area ()
  "Clear echo area if it currently displays Shaoline content."
  (when (and (current-message)
             (get-text-property 0 'shaoline-origin (current-message)))
    (let ((shaoline--allow-empty-message t)) ; conscious, whitelisted clear
      (message nil))
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

;; ────────────────────────────────────────────────────────────
;;  新 advice: расширяем систему advice
;; ────────────────────────────────────────────────────────────

(defun shaoline--advice-read-event (orig &rest args)
  "Around-advice on `read-event'.

Если `cursor-in-echo-area' установлена, увеличиваем
`shaoline--echo-area-input-depth' перед чтением события и
уменьшаем после, тем самым отмечая период реального ввода в
echo-area."
  (if cursor-in-echo-area
      (progn
        (cl-incf shaoline--echo-area-input-depth)
        (unwind-protect
            (apply orig args)
          (cl-decf shaoline--echo-area-input-depth)))
    (apply orig args)))

;; Существующее advice, блокирующее (message nil)
;; ────────────────────────────────────────────────────────────

(defun shaoline--advice-preserve-empty-message (orig &rest args)
  "Guard echo-area against flicker.

Intercepts `(message nil)' and `(message \"\")'.  Such empty
messages are *blocked* when:

  • The echo-area currently shows Shaoline content (property
    `shaoline-origin'), and
  • `shaoline--allow-empty-message' is *not* bound non-nil.

Otherwise the original `message' is executed unchanged."
  (let* ((empty? (or (equal args '(nil))
                     (and (= (length args) 1)
                          (stringp (car args))
                          (string-empty-p (car args)))))
         (shaoline-visible?
          (let ((cur (current-message)))
            (and cur (get-text-property 0 'shaoline-origin cur)))))
    (if (and empty? shaoline-visible? (not shaoline--allow-empty-message))
        (current-message)          ; keep our line – no blink
      (apply orig args))))

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

(shaoline-defeffect shaoline--hide-mode-line ()
  "Hide traditional mode-line in current buffer."
  (unless (gethash (current-buffer) shaoline--modeline-backup-registry)
    ;; Save original mode-line-format (registry + buffer-local var)
    (puthash (current-buffer) mode-line-format shaoline--modeline-backup-registry)
    (setq-local shaoline--original-mode-line mode-line-format)
    (setq mode-line-format nil)
    (force-mode-line-update)
    (push `(modeline . ,(current-buffer)) shaoline--active-effects)))

(shaoline-defeffect shaoline--restore-mode-line ()
  "Restore traditional mode-line in current buffer."
  (when-let ((original (gethash (current-buffer) shaoline--modeline-backup-registry)))
    (setq mode-line-format original)
    (remhash (current-buffer) shaoline--modeline-backup-registry)
    ;; Remove buffer-local backup variable
    (when (local-variable-p 'shaoline--original-mode-line)
      (kill-local-variable 'shaoline--original-mode-line))
    (force-mode-line-update)))

(defun shaoline--hide-mode-lines-globally ()
  "Hide mode-lines in all buffers and set default for new buffers."
  ;; Save the original default mode-line-format if not already saved
  (unless shaoline--original-default-modeline
    (setq shaoline--original-default-modeline (default-value 'mode-line-format)))

  ;; Set the default mode-line-format to nil for all new buffers
  (setq-default mode-line-format nil)

  ;; Hide mode-lines in existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (shaoline--hide-mode-line))))

(defun shaoline--restore-mode-lines-globally ()
  "Restore mode-lines in all buffers and restore default for new buffers."
  ;; First restore the default mode-line-format for new buffers
  (when shaoline--original-default-modeline
    (setq-default mode-line-format shaoline--original-default-modeline)
    (setq shaoline--original-default-modeline nil))

  ;; Restore mode-lines in existing buffers that we modified
  (dolist (buffer (buffer-list))
    (when (gethash buffer shaoline--modeline-backup-registry)
      (with-current-buffer buffer
        (shaoline--restore-mode-line))))

  ;; For buffers that don't have individual backups but got the nil default,
  ;; reset them to use the restored default
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (null mode-line-format)
                 (not (gethash buffer shaoline--modeline-backup-registry)))
        (kill-local-variable 'mode-line-format)))))

(defun shaoline--cleanup-all-modelines ()
  "Clean up all modeline effects."
  ;; Restore default mode-line-format
  (when shaoline--original-default-modeline
    (setq-default mode-line-format shaoline--original-default-modeline)
    (setq shaoline--original-default-modeline nil))

  ;; Restore individual buffer mode-lines
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

  ;; ------------------------------------------------------------------
  ;; Yang re-assertion: keep Shaoline permanently visible.
  ;; A very early post-command hook plus a light timer are installed
  ;; only when STRATEGY is 'yang.
  ;; ------------------------------------------------------------------
  (when (eq strategy 'yang)
    (add-hook 'post-command-hook #'shaoline--reassert-yang-visibility -100)
    (unless (boundp 'shaoline--yang-timer)
      (defvar shaoline--yang-timer nil))
    (setq shaoline--yang-timer
          (run-with-timer 0.11 0.13 #'shaoline--maybe-reassert-yang-after-timer)))
  (unless (eq strategy 'yang)
    (remove-hook 'post-command-hook #'shaoline--reassert-yang-visibility)
    (when (bound-and-true-p shaoline--yang-timer)
      (cancel-timer shaoline--yang-timer)
      (setq shaoline--yang-timer nil)))

  ;; ------------------------------------------------------------------
  ;; Install the *inner-most* advice that blocks spurious (message nil)
  ;; BEFORE any other Shaoline or third-party advice appears.
  ;; depth 100 ⇒ executed last ⇒ closest to the original `message'.
  ;; ------------------------------------------------------------------
  (unless (advice-member-p #'shaoline--advice-preserve-empty-message #'message)
    (advice-add #'message :around
                #'shaoline--advice-preserve-empty-message
                '((depth . 100))))

  (when (shaoline--resolve-setting 'use-hooks)
    (shaoline--attach-hook 'post-command-hook #'shaoline--smart-post-command-update)
    (shaoline--attach-hook 'after-save-hook #'shaoline--debounced-update)
    (shaoline--attach-hook 'window-configuration-change-hook #'shaoline--debounced-update)

    ;; Yang mode: gentle echo area reclaim
    (when (shaoline--resolve-setting 'always-visible)
      (shaoline--attach-hook 'post-command-hook #'shaoline--post-command-restore)
      (shaoline--attach-hook 'window-selection-change-functions
                             (lambda (&rest _)
                               (unless (shaoline--should-yield-echo-area-p)
                                 (run-with-timer 0.1 nil #'shaoline--display-cached))))
      (shaoline--attach-hook 'focus-in-hook
                             (lambda ()
                               (unless (shaoline--should-yield-echo-area-p)
                                 (run-with-timer 0.1 nil #'shaoline--display-cached)))))

    ;; Attach prefix-keys capture hooks only if the segment is enabled
    (when (shaoline--segment-enabled-p 'shaoline-segment-current-keys)
      (shaoline--attach-hook 'post-command-hook #'shaoline--capture-prefix-keys-post)
      (shaoline--attach-hook 'pre-command-hook #'shaoline--capture-prefix-keys-pre)))

  (when (shaoline--resolve-setting 'use-advice)
    (shaoline--attach-advice #'message :around #'shaoline--advice-capture-message)
    (shaoline--attach-advice #'minibuffer-message :around #'shaoline--advice-capture-minibuffer-message)
    (shaoline--attach-advice #'read-event :around #'shaoline--advice-read-event)
    ;; Eval results — capture and pin
    (shaoline--attach-advice #'eval-last-sexp :around #'shaoline--advice-capture-eval-last-sexp)
    (shaoline--attach-advice #'eval-expression :around #'shaoline--advice-capture-eval-expression)
    (when (fboundp 'pp-eval-expression)
      (shaoline--attach-advice #'pp-eval-expression :around #'shaoline--advice-capture-eval-expression)))

  ;; `shaoline--advice-preserve-empty-message' is already installed above with proper depth.

  (when (shaoline--resolve-setting 'use-timers)
    (shaoline--start-timer 'update 1.0 t #'shaoline-update)
    ;; More conservative guard timing
    (let ((guard-interval (if (shaoline--resolve-setting 'always-visible)
                              0.5          ; Реже — меньше конфликтов
                            0.7)))
      (shaoline--start-timer 'guard guard-interval t #'shaoline--guard-visibility)))

  (when (shaoline--resolve-setting 'hide-modelines)
    (shaoline--hide-mode-lines-globally))

  (shaoline--state-put :strategy strategy))

(defun shaoline--cleanup-all-effects ()
  "Clean up all active effects — return to stillness."
  (shaoline--cleanup-all-timers)
  (shaoline--cleanup-all-hooks)
  (shaoline--cleanup-all-advice)
  (shaoline--cleanup-all-modelines)
  ;; Cancel pending deferred restore, if any
  (when (timerp shaoline--restore-timer)
    (cancel-timer shaoline--restore-timer)
    (setq shaoline--restore-timer nil))
  ;; Do *not* clear while always-visible – prevents one-frame blink.
  (unless (shaoline--resolve-setting 'always-visible)
    (shaoline--clear-echo-area))
  (setq shaoline--active-effects nil))

;; ----------------------------------------------------------------------------
;; Message Capture Advice
;; ----------------------------------------------------------------------------

(defvar shaoline--message-pinned-until 0
  "Timestamp (float-time) until which generic messages must not override a pinned one.

Used to keep the last eval result visible briefly even if other
packages call `message' immediately afterwards.")

(defun shaoline--advice-capture-message (orig-fun format-string &rest args)
  "Around-advice on `message' that stores user messages for Shaoline,
but gracefully ignores echo-area clears such as (message nil)."
  (let* ((result (apply orig-fun format-string args))
         (cmsg (current-message))
         ;; Only try to format when the first arg is really a string.
         (clean-text (when (stringp format-string)
                       (apply #'format format-string args))))
    ;; Save non-empty messages that are NOT produced by Shaoline itself.
    (when (and (stringp clean-text)
               (not (string-empty-p clean-text))
               (not (and cmsg (get-text-property 0 'shaoline-origin cmsg))))
      ;; Respect pinned eval result: do not override for a short TTL,
      ;; unless it's the same text.
      (if (and (< (float-time) shaoline--message-pinned-until)
               (not (equal clean-text (shaoline-msg-current))))
          nil
        (shaoline-msg-save clean-text)
        (when (fboundp 'shaoline--debounced-update)
          (shaoline--debounced-update))))
    result))

(defun shaoline--advice-capture-minibuffer-message (orig format-string &rest args)
  "Around-advice on `minibuffer-message' to capture echo-only messages (e.g., eval results)."
  (let ((res (apply orig format-string args))
        (cmsg (current-message)))
    (when (stringp format-string)
      (let ((text (apply #'format format-string args)))
        (when (and text
                   (not (string-empty-p text))
                   ;; Ignore our own echo output
                   (not (and cmsg (get-text-property 0 'shaoline-origin cmsg))))
          ;; Respect pinned eval message
          (if (and (< (float-time) shaoline--message-pinned-until)
                   (not (equal text (shaoline-msg-current))))
              nil
            (shaoline-msg-save text)
            (when (fboundp 'shaoline--debounced-update)
              (shaoline--debounced-update))))))
    res))

(defun shaoline--save-eval-result (value)
  "Format and pin VALUE (result of eval) for Shaoline's message segment."
  (let* ((str (condition-case nil
                  (pp-to-string value)
                (error (format "%S" value))))
         (one (string-trim (replace-regexp-in-string "\n\\s-*" " ⏎ " str)))
         (text (format "=> %s" one))
         (propto (propertize text 'shaoline-kind 'eval)))
    (when (and (stringp propto) (not (string-empty-p propto)))
      (shaoline-msg-save propto)
      ;; Pin for a short while so generic messages don't override instantly
      (setq shaoline--message-pinned-until (+ (float-time) 1.2))
      ;; Eval — обновляем немедленно, чтобы линия точно успела попасть в центр
      (if (fboundp 'shaoline-update)
          (shaoline-update t)
        (when (fboundp 'shaoline--debounced-update)
          (shaoline--debounced-update))))))

(defun shaoline--advice-capture-eval-last-sexp (orig &rest args)
  "Capture result of `eval-last-sexp' and pin it briefly."
  (let ((val (apply orig args)))
    (shaoline--save-eval-result val)
    val))

(defun shaoline--advice-capture-eval-expression (orig &rest args)
  "Capture result of `eval-expression'/`pp-eval-expression' and pin it briefly."
  (let ((val (apply orig args)))
    (shaoline--save-eval-result val)
    val))

;; ----------------------------------------------------------------------------
;; Persistent visibility ------------------------------------------------------

(defvar shaoline--yang-timer nil
  "Internal timer supporting continuous echo-area presence in yang mode.")

(defun shaoline--reassert-yang-visibility ()
  "Forcefully re-display last Shaoline line if echo area lost it."
  (when (and shaoline-mode
             (shaoline--resolve-setting 'always-visible)
             (not (shaoline--should-yield-echo-area-p))
             (shaoline--echo-area-stable-p))
    (let* ((content (shaoline--state-get :last-content))
           (cur (current-message)))
      (when (and content
                 (not (string-empty-p content))
                 (or (null cur)
                     (not (get-text-property 0 'shaoline-origin cur))))
        (let ((tagged (propertize content 'shaoline-origin t))
              (message-log-max nil))
          (message "%s" tagged))))))

(defun shaoline--maybe-reassert-yang-after-timer ()
  "Backup timer to restore Shaoline if some other code cleared it."
  (when (and shaoline-mode
             (shaoline--resolve-setting 'always-visible))
    (shaoline--reassert-yang-visibility)))

(defun shaoline--preserve-visibility-pre ()
  "Восстанавливать Shaoline только когда безопасно.

Уважает пользовательское взаимодействие и не вмешивается
во время ввода или ожидания команд."
  nil)

(defvar shaoline--restore-timer nil
  "Idle timer for deferred visibility restore (avoid stacking).")

(defun shaoline--post-command-restore ()
  "Delayed restoration after command completion (single pending timer)."
  (shaoline--log "post-command-restore: yield=%s pending=%S"
                 (shaoline--should-yield-echo-area-p)
                 (and (timerp shaoline--restore-timer) shaoline--restore-timer))
  (unless (shaoline--should-yield-echo-area-p)
    (unless (timerp shaoline--restore-timer)
      (setq shaoline--restore-timer
            (run-with-idle-timer
             0.15 nil
             (lambda ()
               (setq shaoline--restore-timer nil)
               (shaoline--smart-restore-visibility)))))))

(defun shaoline--smart-restore-visibility ()
  "Restore visibility only when truly safe."
  (when (and shaoline-mode
             (shaoline--resolve-setting 'always-visible)
             (not (shaoline--echo-area-busy-p))
             (shaoline--echo-area-stable-p))
    (let ((our-content (shaoline--state-get :last-content)))
      (when (and our-content (not (string-empty-p our-content)))
        (shaoline--display-cached)))))

;; ----------------------------------------------------------------------------
;; Prefix Key Capture — Yang Mode Enhancement
;; ----------------------------------------------------------------------------

(defvar shaoline--current-keys ""
  "Current prefix keys being typed.")

(defvar shaoline--current-keys-time 0
  "Time when current keys were captured.")

(defvar shaoline--clear-keys-timer nil
  "Timer for clearing current keys.")

(defun shaoline--capture-prefix-keys-pre ()
  "Pre-command hook for capturing prefix keys."
  ;; Capture any prefix arguments or partial key sequences
  (when (and (vectorp (this-command-keys-vector))
             (> (length (this-command-keys-vector)) 0))
    (let* ((keys (this-command-keys-vector))
           (key-desc (key-description keys))
           (command this-command))
      (shaoline--log "shaoline--capture-prefix-keys-pre: keys=%s, desc=%s, command=%s"
                     keys key-desc command)

      ;; Store keys if they look like prefix sequences
      (when (or
             ;; Explicit prefix keys like C-x, C-c, etc.
             (string-match-p "^\\(C-[xc]\\|M-[a-z]\\|C-u\\)" key-desc)
             ;; Universal argument commands
             (and (symbolp command)
                  (commandp command)
                  (string-match-p "\\(universal-argument\\|digit-argument\\|negative-argument\\)"
                                  (symbol-name command))))
        (setq shaoline--current-keys key-desc
              shaoline--current-keys-time (float-time))
        (shaoline--log "shaoline--capture-prefix-keys-pre: captured keys=%s" key-desc)))))

(defun shaoline--capture-prefix-keys-post ()
  "Post-command hook for managing prefix keys lifecycle."
  (let ((command this-command))
    (shaoline--log "shaoline--capture-prefix-keys-post: command=%s, current-keys=%s"
                   command shaoline--current-keys)

    ;; Update keys with current sequence if it's a prefix command
    (when (and command (symbolp command) (commandp command))
      (let* ((keys (this-command-keys-vector))
             (desc (when keys (key-description keys))))
        (when (and desc
                   ;; Command is a prefix command
                   (or (get command 'prefix)
                       (keymapp (key-binding keys))
                       ;; Or sequence looks like a prefix
                       (string-match-p "^\\(C-[xc]\\|M-[a-z]\\|C-h\\)" desc)))
          (setq shaoline--current-keys desc
                shaoline--current-keys-time (float-time))
          (shaoline--log "shaoline--capture-prefix-keys-post: updated keys=%s" desc))))

    ;; Clear keys after completed (non-prefix) commands
    (when (and shaoline--current-keys
               (not (minibufferp))
               (not (and (boundp 'prefix-arg) prefix-arg))
               (not (and (symbolp command)
                         (commandp command)
                         (or (string-match-p "\\(universal-argument\\|digit-argument\\|negative-argument\\)"
                                             (symbol-name command))
                             (get command 'prefix)
                             (keymapp (key-binding (this-command-keys-vector)))))))
      ;; Cancel existing timer
      (when shaoline--clear-keys-timer
        (cancel-timer shaoline--clear-keys-timer)
        (setq shaoline--clear-keys-timer nil))
      ;; Set new timer to clear keys
      (setq shaoline--clear-keys-timer
            (run-with-timer 1.0 nil #'shaoline--clear-current-keys)))))

(defun shaoline--get-current-keys ()
  "Get current prefix keys if recent enough."
  (let ((now (float-time)))
    (cond
     ;; Check for active prefix-arg first (C-u combinations)
     ((and (boundp 'prefix-arg) prefix-arg)
      (let ((prefix-desc (cond
                          ((numberp prefix-arg) (format "C-u %d" prefix-arg))
                          ((eq prefix-arg '-) "C-u -")
                          ((consp prefix-arg) (format "C-u %d" (car prefix-arg)))
                          (t "C-u"))))
        (shaoline--log "shaoline--get-current-keys: prefix-arg active=%s" prefix-desc)
        prefix-desc))

     ;; Check for multi-key sequences in progress
     ((let* ((keys (this-command-keys-vector))
             (desc (when (and keys (vectorp keys) (> (length keys) 0))
                     (key-description keys))))
        (when (and desc
                   ;; Look for incomplete sequences (common prefixes)
                   (string-match-p "^\\(C-[xc]\\|M-[a-z]\\|C-h\\|C-z\\)" desc)
                   ;; But not if it's a complete command
                   (not (commandp (key-binding keys))))
          (shaoline--log "shaoline--get-current-keys: incomplete sequence=%s" desc)
          desc)))

     ;; Return cached keys if they're recent
     ((and shaoline--current-keys
           (not (string-empty-p shaoline--current-keys))
           (< (- now shaoline--current-keys-time) 3.0))
      (shaoline--log "shaoline--get-current-keys: returning cached=%s" shaoline--current-keys)
      shaoline--current-keys)

     ;; Clear stale keys
     (t
      (when shaoline--current-keys
        (setq shaoline--current-keys ""
              shaoline--current-keys-time 0))
      nil))))

(defun shaoline--clear-current-keys ()
  "Clear current prefix keys."
  (setq shaoline--current-keys ""
        shaoline--current-keys-time 0
        shaoline--clear-keys-timer nil)
  (shaoline--log "shaoline--clear-current-keys: keys cleared"))

;; ----------------------------------------------------------------------------
;; Optimized Update Logic — Reduce Unnecessary Updates
;; ----------------------------------------------------------------------------

(defvar shaoline--last-significant-state nil
  "Last significant state that would require a display update.")

(defun shaoline--get-current-significant-state ()
  "Get current significant state for change detection."
  (list
   :buffer-name (buffer-name)
   :position (cons (line-number-at-pos) (current-column)) ; Always track position
   :modified-p (buffer-modified-p)
   :current-keys shaoline--current-keys
   :major-mode major-mode
   :message (shaoline-msg-current)
   :window-width (window-width)))

(defun shaoline--significant-change-p ()
  "Check if there's been a significant change requiring update."
  (let ((current-state (shaoline--get-current-significant-state)))
    (prog1 (not (equal current-state shaoline--last-significant-state))
      (setq shaoline--last-significant-state current-state))))

(defvar shaoline--commands-requiring-update
  '(switch-to-buffer find-file save-buffer
                     windmove-up windmove-down windmove-left windmove-right
                     other-window beginning-of-buffer end-of-buffer)
  "Commands that should always trigger an update.
Reduced list focusing on major state changes.")

(defvar shaoline--movement-commands
  '(next-line previous-line forward-char backward-char
              beginning-of-line end-of-line scroll-up-command scroll-down-command
              dired-next-line dired-previous-line)
  "Movement commands that should update regularly.")

;; ---------------------------------------------------------------------------
;; Update–decision helper — основной фильтр мерцания
;; ---------------------------------------------------------------------------

(defun shaoline--should-update-after-command-p (command)
  "Return non-nil when Shaoline should run an *update* after COMMAND.

Логика:
1. НЕ обновляем если echo-area занята или нестабильна
2. Команды из `shaoline--commands-requiring-update` — всегда.
3. «Движение» (`shaoline--movement-commands`) — не чаще, чем раз в
   `shaoline-update-debounce` секунд.
4. В остальных случаях полагаемся на `shaoline--significant-change-p`."
  (and
   ;; Основное правило: НЕ обновляем если echo-area должна быть свободной
   (not (shaoline--should-yield-echo-area-p))
   (cond
    ;; Явно важные события
    ((memq command shaoline--commands-requiring-update) t)
    ;; Частые перемещения курсора — тротлим
    ((memq command shaoline--movement-commands)
     (> (- (float-time) shaoline--last-movement-update)
        shaoline-update-debounce))
    ;; Всё остальное — по изменению значимого состояния
    (t (shaoline--significant-change-p)))))

(defvar shaoline--last-movement-update 0
  "Time of last movement-triggered update.")



(defun shaoline--smart-post-command-update ()
  "Smart post-command update that reduces unnecessary updates."
  ;; Логируем для отладки
  (shaoline--log "post-command: %s, busy: %s, yield: %s"
                 this-command
                 (shaoline--echo-area-busy-p)
                 (shaoline--should-yield-echo-area-p))

  (when (shaoline--should-update-after-command-p this-command)
    ;; Record command and time for rate limiting
    (shaoline--state-put :last-command this-command)
    (shaoline--state-put :last-update-time (float-time))

    ;; Update movement timestamp if needed
    (when (memq this-command shaoline--movement-commands)
      (setq shaoline--last-movement-update (float-time)))

    ;; repaint right here – no extra 0.1 s gap
    (shaoline-update)))

;; ----------------------------------------------------------------------------
;; Guard Timer — Gentle Persistence
;; ----------------------------------------------------------------------------

(defun shaoline--guard-visibility ()
  "Gentle guardian that respects user interaction.
Вызывается таймером; возвращает Shaoline-строку только когда это безопасно."
  (when (and shaoline-mode
             (not (shaoline--echo-area-busy-p))
             (shaoline--echo-area-stable-p))
    (let* ((current-msg (current-message))
           (since-last (- (float-time) shaoline--last-display-time))
           (our-content (shaoline--state-get :last-content))
           (should-restore
            (and our-content
                 (not (string-empty-p our-content))
                 (> since-last 0.3) ; Увеличенная задержка для стабильности
                 (or (null current-msg)
                     (and (not (get-text-property 0 'shaoline-origin current-msg))
                          ;; Не восстанавливаем поверх системных сообщений
                          (not (string-match-p "\\(?:Saving\\|Loading\\|Auto-saving\\|Mark set\\)"
                                               current-msg)))))))
      (when should-restore
        (shaoline--log "Guard gently reclaiming echo area after %.2fs: current=%S our=%S"
                       since-last current-msg our-content)
        (shaoline--smart-restore-visibility)))))

(defun shaoline--display-cached ()
  "Display last cached content immediately without recomputation."
  (let ((content (shaoline--state-get :last-content)))
    (when (and content (not (string-empty-p content)))
      (let* ((tagged (propertize content 'shaoline-origin t))
             (message-log-max nil))
        (message "%s" tagged)))))


;; ----------------------------------------------------------------------------
;; Effect Logging — Observing Changes
;; ----------------------------------------------------------------------------

(defvar shaoline--last-display-time 0
  "Time (float) когда Shaoline в последний раз вызвала `message'.")

(defvar shaoline--effect-log nil
  "Log of all effects applied.")

(defun shaoline--log-effect (effect)
  "Log EFFECT application."
  (push (list effect (current-time)) shaoline--effect-log)
  (when (> (length shaoline--effect-log) 100)
    (setq shaoline--effect-log (butlast shaoline--effect-log 50))))

(provide 'shaoline-effects)
;;; shaoline-effects.el ends here
