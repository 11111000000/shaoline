;;; shaoline-effects.el --- Effect system for Shaoline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; All impure effects isolated here — the single place where Shaoline
;; touches the outside world.  Like a temple gate: all must pass through,
;; but the inner sanctuary remains pure.

;;; Code:

(require 'shaoline-compat-vars)

;; shaoline-mode is declared in shaoline-compat-vars.el

(require 'shaoline)
(require 'cl-lib)
(require 'subr-x)

;; Byte-compiler hints and forward declarations
(defvar shaoline--last-display-time 0
  "Time (float) когда Shaoline в последний раз вызвала `message'.")

(defvar shaoline--last-displayed-content ""
  "Substring-no-properties of the last shaoline line drawn to the echo-area.
Reassert-yang-visibility and shaoline--display compare the freshly-composed
shaoline line against this snapshot instead of `current-message'.  Comparing
against the live echo-area is unreliable because corfu, flyspell, eldoc,
agent-shell-active-message and other modules routinely clear and rewrite
the echo-area between cursor-move and our re-assertion.  In that race,
naive `current-message' comparison saw `cur=nil' (or non-ours), concluded
the echo-area had lost our line, and redrew — producing a single-frame
flicker.  Comparing against this internal snapshot instead removes the
race: reassert only fires when `shaoline-compose' itself has produced
something new, regardless of what other modules are doing to the
echo-area at the same instant.")

(defvar shaoline--last-displayed-content-time 0
  "Float time at which `shaoline--last-displayed-content' was set.
Used by `shaoline-reassert-throttle-seconds' to delay re-draws of
the same content (cheap idempotency) but not first-draws after a long
idle period (must not appear frozen).")
;; shaoline--yang-timer, shaoline--restore-timer and shaoline--last-movement-update
;; are declared in shaoline-compat-vars.el

(declare-function shaoline-update "shaoline-mode")
(declare-function shaoline--debounced-update "shaoline-strategy")

(defvar shaoline--msg-update-timer nil
  "Timer used to coalesce bursts of message/minibuffer-message updates.")

(defun shaoline--ensure-shared-vars ()
  "Ensure shared Shaoline state variables are bound to safe defaults.

This is a fail-open guard: even if the user has mixed/old .elc/.eln or
multiple Shaoline copies in `load-path', or is live-reloading files while
`shaoline-mode' is active, advice/hooks must not brick Emacs with repeated
void-variable errors."
  ;; Centralized rebinding for hot-reload / makunbound workflows.
  (when (fboundp 'shaoline--ensure-hot-vars)
    (shaoline--ensure-hot-vars))
  (unless (boundp 'shaoline--composing-p)
    (setq shaoline--composing-p nil))
  (unless (boundp 'shaoline--pending-updates)
    (setq shaoline--pending-updates 0))
  (unless (boundp 'shaoline--message-pinned-until)
    (setq shaoline--message-pinned-until 0))
  (unless (boundp 'shaoline--update-bucket)
    (setq shaoline--update-bucket 0))
  ;; Hot-path vars used from hooks/advice/timers (must never be void).
  (unless (boundp 'shaoline--echo-area-input-depth)
    (setq shaoline--echo-area-input-depth 0))
  (unless (boundp 'shaoline--last-busy-log-state)
    (setq shaoline--last-busy-log-state nil))
  (unless (boundp 'shaoline--last-movement-update)
    (setq shaoline--last-movement-update 0))
  (unless (boundp 'shaoline--msg-update-timer)
    (setq shaoline--msg-update-timer nil))
  (unless (boundp 'shaoline--current-keys)
    (setq shaoline--current-keys ""))
  (unless (boundp 'shaoline--current-keys-time)
    (setq shaoline--current-keys-time 0))
  (unless (boundp 'shaoline--clear-keys-timer)
    (setq shaoline--clear-keys-timer nil))
  (unless (boundp 'shaoline--last-display-time)
    (setq shaoline--last-display-time 0))
  (unless (boundp 'shaoline--last-significant-state)
    (setq shaoline--last-significant-state nil))
  ;; Hot-path: read in message advice; must never be void.
  (unless (boundp 'shaoline--allow-empty-message)
    (setq shaoline--allow-empty-message nil))
  ;; Registries are mutated (push/setq) from hooks/advice/timers; must never be void.
  (unless (boundp 'shaoline--active-effects)
    (setq shaoline--active-effects nil))
  (unless (boundp 'shaoline--hook-registry)
    (setq shaoline--hook-registry nil))
  (unless (boundp 'shaoline--advice-registry)
    (setq shaoline--advice-registry nil))
  (unless (boundp 'shaoline--timer-registry)
    (setq shaoline--timer-registry (make-hash-table)))
  (unless (boundp 'shaoline--modeline-backup-registry)
    (setq shaoline--modeline-backup-registry (make-hash-table :weakness 'key)))
  (unless (boundp 'shaoline--original-default-modeline)
    (setq shaoline--original-default-modeline nil))
  ;; Effect log is pushed to from many paths; must never be void.
  (unless (boundp 'shaoline--effect-log)
    (setq shaoline--effect-log nil))
  ;; Tuning knobs used from hot paths; must never be void.
  (unless (boundp 'shaoline-yang-reassert-min-interval)
    (setq shaoline-yang-reassert-min-interval 1.0))
  (unless (boundp 'shaoline-message-update-delay)
    (setq shaoline-message-update-delay 0.5)))

(defun shaoline--schedule-msg-update ()
  "Schedule a single debounced update for recent message bursts."
  (shaoline--ensure-shared-vars)
  (unless shaoline--msg-update-timer
    (setq shaoline--msg-update-timer
          (run-with-timer
           (or (and (boundp 'shaoline-message-update-delay)
                    (numberp shaoline-message-update-delay)
                    shaoline-message-update-delay)
               0.5)
           nil
           (lambda ()
             (setq shaoline--msg-update-timer nil)
             (when (fboundp 'shaoline--debounced-update)
               ;; Strategy uses `shaoline--pending-updates' (cl-incf).  If the user
               ;; somehow ended up with an unbound variable, bind it fail-open.
               (shaoline--ensure-shared-vars)
               (shaoline--debounced-update)))))))

;; shaoline--echo-area-input-depth is declared in shaoline-compat-vars.el

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
  "Registry mapping buffers to their original `mode-line-format'.
Uses weak references so buffers can be garbage collected normally.")

(defvar shaoline--original-default-modeline nil
  "Backup of the original default `mode-line-format'.")

;; Buffer-local backup used by per-buffer hide/restore helpers
;; (unit tests look for this variable).
(defvar-local shaoline--original-mode-line nil
  "Original `mode-line-format' saved before Shaoline hides it.")

(defmacro shaoline-defeffect (name args docstring &rest body)
  "Define an effect NAME that change the world with ARGS and DOCSTRING.
And optional BODY."
  (declare (indent defun))
  `(defun ,name ,args
     ,docstring
     (shaoline--log-effect ',name)
     ,@body))

;; ----------------------------------------------------------------------------
;; Display Effects — Echo Area Manifestation
;; ----------------------------------------------------------------------------

(defun shaoline--display (content)
  "Display CONTENT in echo area with Shaoline tagging, avoiding redundant redraws."
  (shaoline--log "shaoline--display called in buffer: %s, content: %s" (buffer-name) content)
  (let* ((content-np (and (stringp content) (substring-no-properties content)))
         (last-draw shaoline--last-displayed-content)
         ;; Two skip checks (any one is enough):
         ;;  1. `already-drawn': we drew this exact visual content on a
         ;;     previous call (race-free snapshot).  Handles the case
         ;;     where corfu/flyspell/eldoc cleared the echo-area between
         ;;     calls — we trust our own snapshot, not the live
         ;;     `current-message'.
         ;;  2. `ours-in-echo': the live echo-area still has our message
         ;;     and matches.  Handles the case where nothing changed and
         ;;     the snapshot is also a match.
         ;; Either condition skips.  We also accept the legacy
         ;; `same-visual' (== ours-in-echo) for symmetry with
         ;; `shaoline--reassert-yang-visibility'.
         (already-drawn (and (stringp content-np)
                             (string= content-np last-draw)))
         (cur (current-message))
          (ours-in-echo (and cur
                             (get-text-property 0 'shaoline-origin cur)
                             (string= (substring-no-properties cur)
                                      content-np)))
          (already-visible (and already-drawn ours-in-echo))
          (skip already-visible))

    (when (and (not (bound-and-true-p shaoline--composing-p))
               (shaoline--should-display-p content)
               (not skip))
      (shaoline--state-put :last-content content)
      (setq shaoline--last-display-time (float-time)
            shaoline--last-displayed-content (or content-np "")
            shaoline--last-displayed-content-time (float-time))
       (let* ((tagged (propertize content 'shaoline-origin t))
              (message-log-max nil)
              (resize-mini-windows nil)
              (max-mini-window-height 1)
              (message-truncate-lines t))
         (with-current-buffer (window-buffer (minibuffer-window (selected-frame)))
           (setq-local message-truncate-lines t))

         (shaoline--log "shaoline--display actually displaying: %s" tagged)
         (message "%s" tagged)
         (push 'display shaoline--active-effects)))))

(shaoline-defeffect shaoline--clear-echo-area ()
  "Clear echo area if it currently displays Shaoline content.
Also resets `shaoline--last-displayed-content' so the next
`shaoline--display' / `shaoline--reassert-yang-visibility' is not
treated as a no-op."
  (when (and (current-message)
             (get-text-property 0 'shaoline-origin (current-message)))
    (let ((shaoline--allow-empty-message t)) ; conscious, whitelisted clear
      (message nil))
    (shaoline--state-put :last-content "")
    (setq shaoline--last-displayed-content ""
          shaoline--last-displayed-content-time 0)
    (push 'clear shaoline--active-effects)))

(defun shaoline--clear-message-guard ()
  "Hook for `clear-message-function' (Emacs 30+).
Return `dont-clear-message' to keep the echo area when its current
content is shaoline's, so external `(message nil)' calls (e.g. from
agent-shell's active-message cleanup) do not erase the modeline.
Return nil (or anything non-special) to let Emacs clear normally."
  (let ((cur (current-message)))
    (if (and cur
             (get-text-property 0 'shaoline-origin cur)
             (bound-and-true-p shaoline-mode)
             (shaoline--resolve-setting 'always-visible))
        'dont-clear-message
      nil)))

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
  (maphash (lambda (_name timer)
             (cancel-timer timer))
           shaoline--timer-registry)
  (clrhash shaoline--timer-registry))

;; ----------------------------------------------------------------------------
;; Hook Effects — Event Flow Manipulation
;; ----------------------------------------------------------------------------

(defvar shaoline--hook-registry nil
  "Registry of attached hooks.")

(shaoline-defeffect shaoline--attach-hook (hook-name function)
  "Attach FUNCTION to HOOK-NAME exactly once and record it only when active."
  (remove-hook hook-name function)
  (add-hook hook-name function)
  (when (member function (symbol-value hook-name))
    (push (cons hook-name function) shaoline--hook-registry)
    (push `(hook . ,hook-name) shaoline--active-effects)))

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

;; Inner-most guard to prevent unintended echo-area clears
(defun shaoline--advice-preserve-empty-message (orig &rest args)
  "Block spurious clears of the echo area.

If the first arg to `message' is nil or an empty/whitespace-only
string and `shaoline--allow-empty-message' is nil, suppress the
call; otherwise forward to ORIG with ARGS.

Yields to the minibuffer: when a completion UI (Vertico/Consult/Corfu)
owns the echo area, `(message nil)' and empty-string clears must pass
through so the candidate overlay can repaint. Without this, the
candidate list comes up empty until the user types a character
(which forces a redraw through a different path).  See the
`allows-in-minibuffer' and
`allows-when-minibuffer-active-from-regular-buffer' tests.

Important: the yield check is `(not (active-minibuffer-window))',
NOT `(not (minibufferp))'. The completion UI is owned by the
minibuffer even when `(message ...)' is called from a *source*
buffer hook (e.g. vertico/consult `minibuffer-setup-hook', or a
shaoline timer firing while `M-x' is in flight). The earlier
`(minibufferp)' check mistakenly looked at the current buffer and
let those clears through suppression, which is exactly the pro-nix
regression.

Fail-open: if anything goes wrong (e.g. hot reload / makunbound), do not
brick Emacs — just call ORIG."
  (condition-case _err
      (progn
        (shaoline--ensure-shared-vars)
        (let* ((fmt (car args)))
          (cond
           ;; nil means clear; block unless explicitly allowed OR a
           ;; minibuffer is active (a completion UI is in charge of
           ;; the echo). We check `active-minibuffer-window' rather
           ;; than `(minibufferp)' because `(message ...)' calls from
           ;; hooks (vertico/consult `minibuffer-setup-hook',
           ;; third-party packages, shaoline's own timers) run in the
           ;; *source* buffer, where `minibufferp' is nil even while
           ;; `M-x' is in flight. The completion UI still owns the
           ;; echo and must be allowed to clear it.
           ((and (null fmt)
                 (not (bound-and-true-p shaoline--allow-empty-message))
                 (not (active-minibuffer-window)))
            nil)
           ;; empty/whitespace strings — same yielding rule.
           ((and (stringp fmt)
                 (string-empty-p (string-trim (format "%s" fmt)))
                 (not (bound-and-true-p shaoline--allow-empty-message))
                 (not (active-minibuffer-window)))
            nil)
           ;; anything else — pass through
           (t
            (apply orig args)))))
    (error (apply orig args))))

;; ────────────────────────────────────────────────────────────
;;  新 advice: расширяем систему advice
;; ────────────────────────────────────────────────────────────

(defun shaoline--advice-read-event (orig &rest args)
  "Around-advice on `read-event'.  Use ORIG and ARGS.

Если `cursor-in-echo-area' установлена, увеличиваем
=shaoline--echo-area-input-depth' перед чтением события и
уменьшаем после, тем самым отмечая период реального ввода в
echo-area.

Fail-open: do not interfere with input if Shaoline is mid-reload."
  (condition-case _err
      (progn
        (shaoline--ensure-shared-vars)
        (if cursor-in-echo-area
            (progn
              (cl-incf shaoline--echo-area-input-depth)
              (unwind-protect
                  (apply orig args)
                (cl-decf shaoline--echo-area-input-depth)))
          (apply orig args)))
    (error (apply orig args))))

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
  "Hide traditional mode-line in current buffer when not preserved."
  (unless (member major-mode (or (and (boundp 'shaoline-preserve-modeline-modes)
                                     shaoline-preserve-modeline-modes)
                                nil))
    (unless (gethash (current-buffer) shaoline--modeline-backup-registry)
      ;; Save original mode-line-format (registry + buffer-local var)
      (puthash (current-buffer) mode-line-format shaoline--modeline-backup-registry)
      (setq-local shaoline--original-mode-line mode-line-format)
      (setq mode-line-format nil)
      (force-mode-line-update)
      (push `(modeline . ,(current-buffer)) shaoline--active-effects))))

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
  "Hide mode-lines in all buffers and also set the default to nil for new buffers.
Preserved modes are left untouched (kept as the original default)."
  ;; Save and override the default so brand-new buffers start hidden, too.
  (unless shaoline--original-default-modeline
    (setq shaoline--original-default-modeline (default-value 'mode-line-format)))
  (setq-default mode-line-format nil)
  ;; Hide existing buffers (except preserved modes)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (shaoline--hide-mode-line))))

(defun shaoline--ensure-preserved-modeline ()
  "For new buffers, restore default mode-line for preserved modes; otherwise hide."
  (when shaoline-mode
    (if (member major-mode (or (and (boundp 'shaoline-preserve-modeline-modes)
                                   shaoline-preserve-modeline-modes)
                              nil))
        (progn
          ;; Ensure preserved modes keep a proper modeline even though the default is nil
          (when shaoline--original-default-modeline
            (setq mode-line-format shaoline--original-default-modeline))
          (force-mode-line-update))
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
  (shaoline--ensure-shared-vars)
  (shaoline--cleanup-all-effects) ; Clean slate

  ;; ------------------------------------------------------------------
  ;; Yang re-assertion: keep Shaoline permanently visible.
  ;; Register hook in our registry so it is reliably removed on deactivate.
  ;; ------------------------------------------------------------------
  (when (memq strategy '(yang adaptive))
    (remove-hook 'post-command-hook #'shaoline--reassert-yang-visibility)
    (add-hook 'post-command-hook #'shaoline--reassert-yang-visibility -100)
    (when (member #'shaoline--reassert-yang-visibility post-command-hook)
      (push (cons 'post-command-hook #'shaoline--reassert-yang-visibility)
            shaoline--hook-registry))
    (when (and (boundp 'shaoline--yang-timer)
               (timerp shaoline--yang-timer))
      (cancel-timer shaoline--yang-timer))
    (setq shaoline--yang-timer
          (run-with-timer shaoline-yang-reassert-min-interval
                          shaoline-yang-reassert-min-interval
                          #'shaoline--maybe-reassert-yang-after-timer)))
  (unless (memq strategy '(yang adaptive))
    (remove-hook 'post-command-hook #'shaoline--reassert-yang-visibility)
    (setq shaoline--hook-registry
          (assq-delete-all 'post-command-hook shaoline--hook-registry))
    (when (and (boundp 'shaoline--yang-timer)
               (timerp shaoline--yang-timer))
      (cancel-timer shaoline--yang-timer)
      (setq shaoline--yang-timer nil)))

  ;; ------------------------------------------------------------------
  ;; Install the *inner-most* advice that blocks spurious (message nil)
  ;; BEFORE any other Shaoline or third-party advice appears.
  ;; depth 100 ⇒ executed last ⇒ closest to the original `message'.
  ;; Track it in our advice registry so cleanup removes it.
  ;; ------------------------------------------------------------------
  (unless (advice-member-p #'shaoline--advice-preserve-empty-message #'message)
    (advice-add #'message :around
                #'shaoline--advice-preserve-empty-message
                '(:depth 100))
    (push (list #'message :around #'shaoline--advice-preserve-empty-message)
          shaoline--advice-registry)
    (push '(advice . message) shaoline--active-effects))

  ;; Hook Emacs 30+ `clear-message-function' so external (message nil)
  ;; calls (agent-shell-active-message-hide, etc.) cannot erase our
  ;; echo. This is more reliable than the advice-trampoline because
  ;; Emacs itself skips the `echo_area_buffer[0] = Qnil' assignment
  ;; in xdisp.c when our guard returns `dont-clear-message'.
  (unless (eq clear-message-function #'shaoline--clear-message-guard)
    (setq clear-message-function #'shaoline--clear-message-guard))


  (when (or (eq strategy 'adaptive)
            (shaoline--resolve-setting 'use-hooks))
    (shaoline--attach-hook 'post-command-hook #'shaoline--smart-post-command-update)
    (shaoline--attach-hook 'after-save-hook #'shaoline--debounced-update)
    (shaoline--attach-hook 'window-configuration-change-hook #'shaoline--debounced-update)

    ;; Yang mode: gentle echo area reclaim
    (when (or (eq strategy 'adaptive)
              (shaoline--resolve-setting 'always-visible))
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

  (when (or (eq strategy 'adaptive)
            (shaoline--resolve-setting 'use-advice))
    (shaoline--attach-advice #'message :around #'shaoline--advice-capture-message)
    (shaoline--attach-advice #'minibuffer-message :around #'shaoline--advice-capture-minibuffer-message)
    (shaoline--attach-advice #'read-event :around #'shaoline--advice-read-event)
    ;; Eval results — capture and pin (use :filter-return so errors bypass us)
    (shaoline--attach-advice #'eval-last-sexp :filter-return #'shaoline--filter-return-eval-last-sexp)
    (shaoline--attach-advice #'eval-expression :filter-return #'shaoline--filter-return-eval-expression)
    (when (fboundp 'pp-eval-expression)
      (shaoline--attach-advice #'pp-eval-expression :filter-return #'shaoline--filter-return-eval-expression)))

  ;; `shaoline--advice-preserve-empty-message' is already installed above with proper depth.

  (when (or (eq strategy 'adaptive)
            (shaoline--resolve-setting 'use-timers))
    (shaoline--start-timer 'update 1.0 t #'shaoline-update)
    ;; More conservative guard timing
    (let ((guard-interval 1.0))
      (shaoline--start-timer 'guard guard-interval t #'shaoline--guard-visibility)))

  (when (shaoline--resolve-setting 'hide-modelines)
    (shaoline--hide-mode-lines-globally)
    (shaoline--attach-hook 'after-change-major-mode-hook #'shaoline--ensure-preserved-modeline))

  (shaoline--state-put :strategy strategy))

(defun shaoline--cleanup-all-effects ()
  "Clean up all active effects — return to stillness."
  (shaoline--cleanup-all-timers)
  (shaoline--cleanup-all-hooks)
  (shaoline--cleanup-all-advice)
  (shaoline--cleanup-all-modelines)
  ;; Cancel pending deferred restore, if any
  (when (and (boundp 'shaoline--restore-timer)
             (timerp shaoline--restore-timer))
    (cancel-timer shaoline--restore-timer)
    (setq shaoline--restore-timer nil))
  ;; Also cancel yang reassert timer if present, and ensure hook is removed
  (when (and (boundp 'shaoline--yang-timer) (timerp shaoline--yang-timer))
    (cancel-timer shaoline--yang-timer)
    (setq shaoline--yang-timer nil))
  (remove-hook 'post-command-hook #'shaoline--reassert-yang-visibility)
  ;; Release the `clear-message-function' hook if we own it.
  (when (eq clear-message-function #'shaoline--clear-message-guard)
    (setq clear-message-function nil))
  ;; Do *not* clear while always-visible – prevents one-frame blink.
  (unless (shaoline--resolve-setting 'always-visible)
    (shaoline--clear-echo-area))
  (setq shaoline--active-effects nil)
  ;; Reset the snapshot so a future activation doesn't think we already drew
  ;; the current compose output.
  (setq shaoline--last-displayed-content ""
        shaoline--last-displayed-content-time 0))

;; ----------------------------------------------------------------------------
;; Message Capture Advice
;; ----------------------------------------------------------------------------

(defvar shaoline--message-pinned-until 0
  "Float timestamp until which generic messages must not override a pinned one.

Used to keep the last eval result visible briefly, even if other packages
call `message` immediately afterwards.")


(defun shaoline--advice-capture-message (orig-fun format-string &rest args)
  "Store user messages for Shaoline around `message'.
Gracefully ignore echo-area clears such as (message nil).
Use ORIG-FUN, FORMAT-STRING and ARGS.

Fail-open: if Shaoline is mid-reload / vars are temporarily unbound, never
brick Emacs — fall back to calling ORIG-FUN."
  (condition-case _err
      (progn
        (shaoline--ensure-shared-vars)
        (if (not (shaoline--segment-enabled-p 'shaoline-segment-echo-message))
            (apply orig-fun format-string args)
          (let ((result (apply orig-fun format-string args))
                (cmsg (current-message))
                ;; Only try to format when the first arg is really a string.
                (clean-text (when (stringp format-string)
                              (apply #'format format-string args))))
            ;; Save non-empty messages that are NOT produced by Shaoline itself.
            (when (and (not (bound-and-true-p shaoline--composing-p))
                       (stringp clean-text)
                       (<= (length clean-text) 2000)
                       (not (string-empty-p clean-text))
                       (not (and cmsg (get-text-property 0 'shaoline-origin cmsg))))
              ;; Respect pinned eval result: do not override for a short TTL,
              ;; unless it's the same text.
              (let ((pinned-until (if (boundp 'shaoline--message-pinned-until)
                                      shaoline--message-pinned-until
                                    0)))
                (if (and (< (float-time) pinned-until)
                         (fboundp 'shaoline-msg-current)
                         (not (equal clean-text (shaoline-msg-current))))
                    nil
                  (when (fboundp 'shaoline-msg-save)
                    (shaoline-msg-save clean-text))
                  (shaoline--schedule-msg-update))))
            result)))
    (error (apply orig-fun format-string args))))

(defun shaoline--advice-capture-minibuffer-message (orig format-string &rest args)
  "Around advice on `minibuffer-message' to capture echo-only messages.
Save non-empty messages that are not produced by Shaoline itself.
Use ORIG, FORMAT-STRING and ARGS

Fail-open: never brick Emacs during hot reload — on any error call ORIG."
  (condition-case _err
      (progn
        (shaoline--ensure-shared-vars)
        (let ((res (apply orig format-string args))
              (cmsg (current-message)))
          (when (and (not (bound-and-true-p shaoline--composing-p))
                     (stringp format-string))
            (let ((text (apply #'format format-string args)))
              (when (and text
                         (<= (length text) 2000)
                         (not (string-empty-p text))
                         (not (and cmsg (get-text-property 0 'shaoline-origin cmsg))))
                (when (fboundp 'shaoline-msg-save)
                  (shaoline-msg-save text))
                (shaoline--schedule-msg-update))))
          res))
    (error (apply orig format-string args))))

(defun shaoline--save-eval-result (value)
  "Format and pin VALUE (result of eval) for Shaoline's message segment."
  (let* ((str (condition-case nil
                  (pp-to-string value)
                (error (prin1-to-string value))))
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
  "Capture result of `eval-last-sexp' and pin it briefly.  Use ORIG and ARGS."
  (let ((val (apply orig args)))
    (shaoline--save-eval-result val)
    val))

(defun shaoline--advice-capture-eval-expression (orig &rest args)
  "Capture result of `eval-expression'/`pp-eval-expression' and pin it briefly.
Use ORIG and ARGS."
  (let ((val (apply orig args)))
    (shaoline--save-eval-result val)
    val))

;; New filter-return advices: run only on success, stay out of error backtraces.
(defun shaoline--filter-return-eval-last-sexp (val)
  "Filter-return advice for `eval-last-sexp' — capture VAL and pin it.
Runs only on successful evaluation, so it stays out of error backtraces."
  (shaoline--save-eval-result val)
  val)

(defun shaoline--filter-return-eval-expression (val)
  "Filter-return advice for `eval-expression'/`pp-eval-expression'.  Use VAL."
  (shaoline--save-eval-result val)
  val)

;; ----------------------------------------------------------------------------
;; Persistent visibility ------------------------------------------------------



(defcustom shaoline-minibuffer-trigger-commands
  '(execute-extended-command find-file save-buffer
    eval-expression eval-defun eval-last-sexp eval-buffer
    write-file insert-file read-file
    yank-pop
    completing-read
    read-from-minibuffer
    describe-variable describe-function
    find-library locate-library
    apropos-documentation)
  "Commands known to open a minibuffer.
Reassert skips when `this-command' or `last-command' is in this list,
preventing one-frame flicker where shaoline overlays the soon-to-appear
minibuffer prompt."
  :type '(repeat symbol)
  :group 'shaoline)

(defun shaoline--minibuffer-trigger-cmd-p (cmd)
  "Return non-nil when CMD is known to open a minibuffer soon.
Checks the explicit list `shaoline-minibuffer-trigger-commands', the
`consult-' and `minibuffer-' name prefixes (covers consult-imenu,
consult-xref, consult-yank-pop, etc.), and `M-x' / `M-:' via
`execute-extended-command' / `eval-expression'."
  (and (symbolp cmd)
       (let ((name (symbol-name cmd)))
         (or (memq cmd shaoline-minibuffer-trigger-commands)
             (string-prefix-p "consult-" name)
             (string-prefix-p "minibuffer-" name)))))

(defun shaoline--reassert-yang-visibility ()
  "Forcefully re-display last Shaoline line if echo area lost it."
  (shaoline--ensure-shared-vars)
  (when (and shaoline-mode
             (shaoline--resolve-setting 'always-visible)
             (not (shaoline--should-yield-echo-area-p))
             (shaoline--echo-area-stable-p)
             (not (shaoline--minibuffer-trigger-cmd-p this-command))
             (not (shaoline--minibuffer-trigger-cmd-p last-command)))
    (let* ((content (shaoline--state-get :last-content))
           (cur (current-message))
           (content-np (and (stringp content) (substring-no-properties content)))
           (ours-and-same (and cur
                                (get-text-property 0 'shaoline-origin cur)
                                (string= (substring-no-properties cur) content-np))))
      (when (and content
                 (not (string-empty-p content))
                 (not ours-and-same))
        (shaoline--state-put :last-content content)
        (shaoline--display content)))))


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

;; shaoline--restore-timer is declared near the top of this file.
;; (kept there to ensure early declaration before any references)

(defun shaoline--post-command-restore ()
  "Delayed restoration after command completion (single pending timer)."
  (shaoline--ensure-shared-vars)
  (shaoline--log "post-command-restore: yield=%s pending=%S"
                 (shaoline--should-yield-echo-area-p)
                 (and (boundp 'shaoline--restore-timer)
                      (timerp shaoline--restore-timer)
                      shaoline--restore-timer))
  (unless (shaoline--should-yield-echo-area-p)
    (unless (and (boundp 'shaoline--restore-timer)
                 (timerp shaoline--restore-timer))
      (setq shaoline--restore-timer
            (run-with-idle-timer
             0.25 nil
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
  "Return non-nil when Shaoline should update after COMMAND."
  (shaoline--ensure-shared-vars)
  (and (not (shaoline--should-yield-echo-area-p))
       (cond
        ((memq command shaoline--commands-requiring-update) t)
        ((memq command shaoline--movement-commands)
         (> (- (float-time) shaoline--last-movement-update)
            (or (and (boundp 'shaoline-update-debounce)
                     shaoline-update-debounce)
                0.25)))
        (t (shaoline--significant-change-p)))))

;; shaoline--last-movement-update is declared near the top of this file.
;; (kept there to ensure early declaration before any references)



(defun shaoline--smart-post-command-update ()
  "Smart post-command update that reduces unnecessary update."
  (shaoline--ensure-shared-vars)
  ;; Логируем для отладки
  (shaoline--log "post-command: %s, busy: %s, yield: %s"
                 this-command
                 (shaoline--echo-area-busy-p)
                 (shaoline--should-yield-echo-area-p))

    (when (shaoline--should-update-after-command-p this-command)
      (shaoline--state-put :last-command this-command)
      (shaoline--state-put :last-update-time (float-time))
      (when (memq this-command shaoline--movement-commands)
        (setq shaoline--last-movement-update (float-time)))
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
                 (> since-last 0.6) ; Увеличенная задержка для стабильности
                 (or (null current-msg)
                     (and (not (get-text-property 0 'shaoline-origin current-msg))
                          ;; Не восстанавливаем поверх системных сообщений
                          (not (string-match-p "\\(?:Saving\\|Loading\\|Auto-saving\\|Mark set\\)"
                                               current-msg)))))))
      (when should-restore
        (shaoline--log "Guard gently reclaiming echo area after %.2fs: current=%S our=%S"
                       since-last current-msg our-content)
        (shaoline-update t)))))

(defun shaoline--display-cached ()
  "Display last cached content immediately without recomputation.
Skips when we already drew this exact visual content (race-free
snapshot check; see `shaoline--last-displayed-content')."
  (shaoline--ensure-shared-vars)
  (let* ((content (shaoline--state-get :last-content))
         (content-np (and content (substring-no-properties content)))
         (current (current-message))
         (ours-in-echo (and current
                            (get-text-property 0 'shaoline-origin current)
                            (string= (substring-no-properties current)
                                     content-np)))
         (already-drawn (and content-np
                             (string= content-np
                                      shaoline--last-displayed-content)))
         (already-visible (and already-drawn ours-in-echo)))
     (shaoline--log "display-cached: content-len=%s already-visible=%s"
                    (and (stringp content) (length content))
                    already-visible)
     (when (and (not (bound-and-true-p shaoline--composing-p))
                content (not (string-empty-p content))
                (not already-visible))

       (let* ((tagged (propertize content 'shaoline-origin t))
              (message-log-max nil)
              (resize-mini-windows nil)
              (max-mini-window-height 1)
              (message-truncate-lines t))
         (with-current-buffer (window-buffer (minibuffer-window (selected-frame)))
           (setq-local message-truncate-lines t))
         (message "%s" tagged)

        (setq shaoline--last-display-time (float-time)
              shaoline--last-displayed-content (or content-np "")
              shaoline--last-displayed-content-time (float-time))))))


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
;; Local Variables:
;; package-lint-main-file: "shaoline.el"
;; End:
;;; shaoline-effects.el ends here
