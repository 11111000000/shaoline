;;; shaoline-effects-test.el --- Effect system tests for Shaoline 3.0 -*- lexical-binding: t; -*-

;; Version: 3.0.0-dao

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'shaoline)
(require 'shaoline-effects)
(require 'cl-lib)
;; cl-format is not autoloaded; older Emacs may not have it in cl-lib
(unless (fboundp 'cl-format)
  (ignore-errors (require 'cl-extra)))

;; ----------------------------------------------------------------------------
;; Test: Effect logging
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-effect-logging ()
  "Test that effects are properly logged."
  (setq shaoline--effect-log nil)
  (shaoline--log-effect 'test-effect)
  (should (= (length shaoline--effect-log) 1))
  (should (eq (car (car shaoline--effect-log)) 'test-effect)))

;; ----------------------------------------------------------------------------
;; Test: Display effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-display-effect ()
  "Test display effect with tagging."
  (shaoline--state-put :last-content "")
  (setq shaoline--active-effects nil)

  ;; Mock should-display-p to return true; current-message is nil (no echo)
  (cl-letf (((symbol-function 'shaoline--should-display-p) (lambda (content) t))
            ((symbol-function 'current-message) (lambda () nil)))
    (shaoline--display "test content")
    (should (string= (shaoline--state-get :last-content) "test content"))
    (should (member 'display shaoline--active-effects))))

(ert-deftest shaoline-display-skips-when-content-unchanged-and-ours-in-echo ()
  "When shaoline's last content visually equals the new content (ignoring
text-properties from compose regeneration) AND the echo-area still shows
our message, `shaoline--display' must skip the `(message ...)' call.

This eliminates flicker on every cursor-move: previously compose regenerated
timestamps/cache values that produced a different (by `equal') string each
time even when the visible characters were identical, so display always
re-rendered — causing the echo-area to blink."
  (shaoline--state-put :last-content "test content")
  (setq shaoline--active-effects nil)
  (setq shaoline--last-displayed-content "test content")
  (let ((calls 0))
    (cl-letf (((symbol-function 'shaoline--should-display-p) (lambda (_) t))
              ((symbol-function 'current-message)
               (lambda () (propertize "test content" 'shaoline-origin t)))
              ((symbol-function 'message)
               (lambda (&rest _) (setq calls (1+ calls)) nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      ;; Pass a *different-property* but visually identical string
      (let ((same-visual-different-props
             (propertize "test content" 'compose-timestamp 12345)))
        (shaoline--display same-visual-different-props)))
    ;; No (message ...) call → no flicker
    (should (= calls 0))
    ;; last-content is NOT updated (we skipped the display)
    (should (string= (shaoline--state-get :last-content) "test content"))
    ;; display effect not pushed
    (should-not (member 'display shaoline--active-effects))))

(ert-deftest shaoline-display-cached-redraws-after-echo-was-cleared ()
  "Cached content is redrawn when the echo-area no longer contains it."
  (let ((calls 0))
    (setq shaoline--last-displayed-content "test content")
    (shaoline--state-put :last-content "test content")
    (cl-letf (((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) (setq calls (1+ calls)) nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (shaoline--display-cached))
    (should (= calls 1))))

(ert-deftest shaoline-display-rerenders-when-content-actually-changed ()
  "When content visually changes, display must re-render even if echo-area still shows our message."

  (shaoline--state-put :last-content "test [12] content")
  (setq shaoline--active-effects nil)
  (let ((calls 0))
    (cl-letf (((symbol-function 'shaoline--should-display-p) (lambda (_) t))
              ((symbol-function 'current-message)
               (lambda () (propertize "test [12] content" 'shaoline-origin t)))
              ((symbol-function 'message)
               (lambda (&rest _) (setq calls (1+ calls)) nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (shaoline--display "test [13] content"))
    (should (= calls 1))
    (should (string= (shaoline--state-get :last-content) "test [13] content"))))

(ert-deftest shaoline-display-skips-when-snapshot-already-set ()
  "Race-free skip: if `shaoline--last-displayed-content' equals the new
content (substring-no-properties), shaoline--display must skip even if
the live `current-message' is empty or owned by another module
(e.g. corfu cleared it between cursor-move and our redraw).

This is the scenario that produced the cursor-move flicker on pro-nix:
corfu-auto--post-command erased our message between our snapshot and our
redraw, so `current-message' was no longer ours, but `compose' had not
changed.  Comparing against `current-message' alone falsely concluded
the echo-area had lost our line."
  (let ((calls 0))
    (cl-letf (((symbol-function 'shaoline--should-display-p) (lambda (_) t))
              ((symbol-function 'current-message)
               ;; Live echo-area has been replaced by foreign content.
               (lambda () (propertize "corfu popup" 'face 'corfu)))
              ((symbol-function 'message)
               (lambda (&rest _) (setq calls (1+ calls)) nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      ;; We drew this exact content last time.
      (setq shaoline--last-displayed-content "test content")
       ;; Same content again with foreign echo → redraw.
       (shaoline--display "test content"))

     (should (= calls 1))))


(ert-deftest shaoline-display-rerenders-when-snapshot-differs ()
  "If our snapshot differs from the new content, redraw even if
`current-message' is empty or foreign.  Otherwise corfu's erase would
permanently hide our line."
  (let ((calls 0))
    (cl-letf (((symbol-function 'shaoline--should-display-p) (lambda (_) t))
              ((symbol-function 'current-message)
               (lambda () (propertize "foreign" 'face 'tooltip)))
              ((symbol-function 'message)
               (lambda (&rest _) (setq calls (1+ calls)) nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      ;; Last draw was "test [12] content"; new content has new position.
      (setq shaoline--last-displayed-content "test [12] content")
      (shaoline--display "test [13] content"))
    (should (= calls 1))
    ;; Snapshot updates to the freshly drawn content.
    (should (string= shaoline--last-displayed-content "test [13] content"))))

(ert-deftest shaoline-clear-echo-area-resets-snapshot ()
  "`shaoline--clear-echo-area' must reset `shaoline--last-displayed-content'
so the next display/redraw is not treated as a no-op."
  (let ((cleared nil))
    (cl-letf (((symbol-function 'shaoline--allow-empty-message) nil)
              ((symbol-function 'message)
               (lambda (&rest _) (setq cleared t) nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (setq shaoline--last-displayed-content "test content")
      ;; current-message is our string → clear should fire.
      (cl-letf (((symbol-function 'current-message)
                 (lambda () (propertize "test content" 'shaoline-origin t))))
        (shaoline--clear-echo-area)))
    (should cleared)
    (should (string= shaoline--last-displayed-content ""))))

(ert-deftest shaoline-clear-effect ()
  "Test clear echo area effect."
  (shaoline--state-put :last-content "some content")

  ;; Mock current-message to return tagged content
  (cl-letf (((symbol-function 'current-message)
             (lambda () (propertize "test" 'shaoline-origin t))))
    (shaoline--clear-echo-area)
    (should (string= (shaoline--state-get :last-content) ""))
    (should (member 'clear shaoline--active-effects))))

;; ----------------------------------------------------------------------------
;; Test: Timer effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-timer-effects ()
  "Test timer start and stop effects."
  (clrhash shaoline--timer-registry)

  ;; Start a timer
  (shaoline--start-timer 'test-timer 1.0 nil (lambda () (message "test")))
  (should (gethash 'test-timer shaoline--timer-registry))
  (should (member '(timer . test-timer) shaoline--active-effects))

  ;; Stop the timer
  (shaoline--stop-timer 'test-timer)
  (should-not (gethash 'test-timer shaoline--timer-registry)))

(ert-deftest shaoline-timer-cleanup ()
  "Test cleanup of all timers."
  (clrhash shaoline--timer-registry)

  ;; Start multiple timers
  (shaoline--start-timer 'timer1 1.0 nil (lambda () (message "test1")))
  (shaoline--start-timer 'timer2 1.0 nil (lambda () (message "test2")))

  (should (= (hash-table-count shaoline--timer-registry) 2))

  ;; Cleanup all
  (shaoline--cleanup-all-timers)
  (should (= (hash-table-count shaoline--timer-registry) 0)))

;; ----------------------------------------------------------------------------
;; Test: Hook effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-hook-registry-global-entry-is-reconciled ()
  "Global hook registry entries use an explicit scope marker."
  (setq shaoline--hook-registry nil)
  (defun shaoline-test-global-hook () nil)
  (shaoline--attach-hook 'post-command-hook #'shaoline-test-global-hook)
  (should (cl-some (lambda (entry)
                     (and (eq (car entry) 'post-command-hook)
                          (eq (if (functionp (cdr entry))
                                  (cdr entry)
                                (cadr entry))
                              #'shaoline-test-global-hook)))
                   shaoline--hook-registry)))

(ert-deftest shaoline-hook-effects ()
  "Test hook attach and detach effects."
  (setq shaoline--hook-registry nil)
  (defun test-hook-function () (message "test"))
  (shaoline--attach-hook 'test-hook 'test-hook-function)
  (should (member '(test-hook . test-hook-function) shaoline--hook-registry))
  (should (member '(hook . test-hook) shaoline--active-effects))
  (shaoline--detach-hook 'test-hook 'test-hook-function)
  (should-not (assoc 'test-hook shaoline--hook-registry)))


(ert-deftest shaoline-hook-cleanup ()
  "Test cleanup of all hooks."
  (setq shaoline--hook-registry nil)

  ;; Add some hooks to registry
  (push '(hook1 . func1) shaoline--hook-registry)
  (push '(hook2 . func2) shaoline--hook-registry)

  (should (= (length shaoline--hook-registry) 2))

  ;; Mock remove-hook
  (cl-letf (((symbol-function 'remove-hook) (lambda (hook func) nil)))
    (shaoline--cleanup-all-hooks)
    (should (= (length shaoline--hook-registry) 0))))

;; ----------------------------------------------------------------------------
;; Test: Advice effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-advice-effects ()
  "Test advice attach and detach effects."
  (setq shaoline--advice-registry nil)

  ;; Test advice function
  (defun test-advice (orig-fn &rest args) (apply orig-fn args))

  ;; Attach advice
  (shaoline--attach-advice 'message :around 'test-advice)
  (should (member '(message :around test-advice) shaoline--advice-registry))
  (should (member '(advice . message) shaoline--active-effects))

  ;; Detach advice
  (shaoline--detach-advice 'message 'test-advice)
  (should-not (cl-find-if (lambda (entry)
                            (and (eq (car entry) 'message)
                                 (eq (caddr entry) 'test-advice)))
                          shaoline--advice-registry)))

;; ----------------------------------------------------------------------------
;; Test: Mode line effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-mode-line-effects ()
  "Test mode line hide and restore effects."
  (with-temp-buffer
    (let ((original-mode-line mode-line-format))
      ;; Hide mode line
      (shaoline--hide-mode-line)
      (should (null mode-line-format))
      (should (local-variable-p 'shaoline--original-mode-line))

      ;; Restore mode line
      (shaoline--restore-mode-line)
      (should mode-line-format)
      (should-not (local-variable-p 'shaoline--original-mode-line)))))

;; ----------------------------------------------------------------------------
;; Test: Message capture advice
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-message-capture ()
  "Test message capture advice."
  (shaoline-msg-clear)

  ;; Mock original message function
  (shaoline-msg-clear)
  (shaoline--advice-capture-message (lambda (fmt &rest args)
                                      (format fmt args))
                                    "Test %s" "message")
  (should (string= (shaoline-msg-current) "Test message"))
  )

;; ----------------------------------------------------------------------------
;; Test: Guard timer
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-guard-visibility ()
  "Test guard timer behavior."
  (let ((update-called nil))
    ;; Set up state to trigger guard update
    (setq shaoline-mode t)
    (shaoline--state-put :last-content "test content")
    (setq shaoline--last-display-time 0) ; Force stale timestamp

    ;; Mock functions
    (cl-letf (((symbol-function 'shaoline--echo-area-busy-p) (lambda () nil))
              ((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'shaoline-update) (lambda (&rest _args) (setq update-called t))))

      (shaoline--guard-visibility)
      (should update-called))))

;; ----------------------------------------------------------------------------
;; Test: Effect orchestration
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-strategy-application ()
  "Test strategy application orchestrates effects correctly."
  (setq shaoline--active-effects nil)

  ;; Mock resolve-setting for yang strategy
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting)
               (pcase setting
                 ('use-hooks t)
                 ('use-advice t)
                 ('use-timers t)
                 (_ nil)))))

    (shaoline--apply-strategy 'yang)
    (should (eq (shaoline--state-get :strategy) 'yang))))

;; ----------------------------------------------------------------------------
;; Test: Cleanup all effects
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-cleanup-all-effects ()
  "Test that cleanup properly clears all effects."
  ;; Set up some state
  (setq shaoline--active-effects '(display timer hook))
  (shaoline--state-put :last-content "some content")

  ;; Mock cleanup functions
  (cl-letf (((symbol-function 'shaoline--cleanup-all-timers) (lambda () nil))
            ((symbol-function 'shaoline--cleanup-all-hooks) (lambda () nil))
            ((symbol-function 'shaoline--cleanup-all-advice) (lambda () nil))
            ((symbol-function 'shaoline--clear-echo-area) (lambda () nil)))

    (shaoline--cleanup-all-effects)
    (should (null shaoline--active-effects))))

;; ----------------------------------------------------------------------------
;; Test: clear-message-function guard
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-clear-message-guard-protects-shaoline-content ()
  "When current-message is shaoline's, guard returns `dont-clear-message'.
This is the mechanism that prevents external `(message nil)' calls
(e.g. agent-shell-active-message-hide) from erasing shaoline's
modeline from the echo area."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (eq setting 'always-visible)))
            ((symbol-function 'current-message)
             (lambda () (propertize "🤖 pro-nix" 'shaoline-origin t))))
    (let ((shaoline-mode t))
      (should (eq (shaoline--clear-message-guard) 'dont-clear-message)))))

(ert-deftest shaoline-clear-message-guard-allows-external-clear ()
  "When current-message is not shaoline's, guard returns nil."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (eq setting 'always-visible)))
            ((symbol-function 'current-message)
             (lambda () "external message")))
    (let ((shaoline-mode t))
      (should (null (shaoline--clear-message-guard))))))

(ert-deftest shaoline-clear-message-guard-inactive-without-always-visible ()
  "In yin/pure strategies the guard is permissive even for shaoline content."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (and (eq setting 'use-hooks) t)))
            ((symbol-function 'current-message)
             (lambda () (propertize "🤖 pro-nix" 'shaoline-origin t))))
    (let ((shaoline-mode t))
      (should (null (shaoline--clear-message-guard))))))

(ert-deftest shaoline-clear-message-guard-inactive-when-shaoline-mode-off ()
  "When shaoline-mode is off, the guard is permissive."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (eq setting 'always-visible)))
            ((symbol-function 'current-message)
             (lambda () (propertize "🤖 pro-nix" 'shaoline-origin t))))
    (let ((shaoline-mode nil))
      (should (null (shaoline--clear-message-guard))))))

(ert-deftest shaoline-clear-message-guard-inactive-without-current-message ()
  "When echo area is empty, the guard is permissive."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (eq setting 'always-visible)))
            ((symbol-function 'current-message) (lambda () nil)))
    (let ((shaoline-mode t))
      (should (null (shaoline--clear-message-guard))))))

(ert-deftest shaoline-preserve-empty-message-allows-in-minibuffer ()
  "When minibuffer is active, `shaoline--advice-preserve-empty-message'
must NOT block `(message nil)' calls.

Vertical completion UIs (Vertico, Consult, Corfu in minibuffer) rely on
`(message nil)' being able to clear the echo area so the candidate
overlay can repaint. If we suppress that clear, the display comes up
empty until the user types a character (which forces a redraw through
a different path — that's the symptom observed in pro-nix).

The guard exists to prevent external `(message nil)' calls (e.g.
agent-shell-active-message-hide) from erasing shaoline's own modeline
*in regular buffers*. The minibuffer is a separate concern: the
echo-area is dominated by the completion UI there, and we must yield."
  (let ((calls 0))
    (cl-letf (((symbol-function 'minibufferp) (lambda () t))
              ((symbol-function 'active-minibuffer-window) (lambda () t))
              ((symbol-function 'shaoline--ensure-shared-vars) (lambda () nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline--allow-empty-message nil))
        (shaoline--advice-preserve-empty-message
         (lambda (&rest _) (setq calls (1+ calls)) nil)
         nil)))
    ;; orig MUST be called once (in minibuffer). If the guard blocked it,
    ;; calls stays at 0 and the test fails — that is the regression.
    (should (= calls 1))))

(ert-deftest shaoline-preserve-empty-message-allows-when-minibuffer-active-from-regular-buffer ()
  "Regression for pro-nix: `(message nil)' called from a regular
buffer (e.g. from a shaoline timer / `minibuffer-setup-hook' running
in the source buffer) MUST pass through when a minibuffer is active.

Real-world flow that broke:
  1. User presses `M-x'.
  2. Minibuffer is activated, vertico/consult runs `minibuffer-setup-hook'.
  3. Completion UI calls `(message nil)' from the *source* buffer to
     clear the echo area for the candidate overlay.
  4. `shaoline--advice-preserve-empty-message' is called with
     `current-buffer' = source buffer (so `minibufferp' = nil) but
     `active-minibuffer-window' non-nil.
  5. The guard previously checked `(minibufferp)' and wrongly
     suppressed the clear → candidate list invisible until first
     character is typed.
  6. The fix: yield when `active-minibuffer-window' is non-nil, not
     when the current buffer is a minibuffer."
  (let ((calls 0))
    (cl-letf (((symbol-function 'minibufferp) (lambda () nil))    ; source buffer
              ((symbol-function 'active-minibuffer-window)        ; minibuffer is up
               (lambda () t))
              ((symbol-function 'shaoline--ensure-shared-vars) (lambda () nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline--allow-empty-message nil))
        (shaoline--advice-preserve-empty-message
         (lambda (&rest _) (setq calls (1+ calls)) nil)
         nil)))
    ;; orig MUST be called once. The old `(not (minibufferp))' check
    ;; suppressed this; the new `(not (active-minibuffer-window))' lets it through.
    (should (= calls 1))))

(ert-deftest shaoline-preserve-empty-message-blocks-in-regular-buffer ()
  "In a regular (non-minibuffer) buffer, `(message nil)' must still be
suppressed. This is the *original* purpose of the guard — protecting
shaoline's modeline from external `(message nil)' attacks."
  (let ((calls 0))
    (cl-letf (((symbol-function 'minibufferp) (lambda () nil))
              ((symbol-function 'active-minibuffer-window) (lambda () nil))
              ((symbol-function 'shaoline--ensure-shared-vars) (lambda () nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline--allow-empty-message nil))
        (shaoline--advice-preserve-empty-message
         (lambda (&rest _) (setq calls (1+ calls)) nil)
         nil)))
    ;; orig must NOT be called in a regular buffer.
    (should (= calls 0))))

(ert-deftest shaoline-preserve-empty-message-allows-empty-in-minibuffer ()
  "In a minibuffer, even empty/whitespace strings must pass through —
they are how completion UIs reset the echo area between renders."
  (let ((calls 0))
    (cl-letf (((symbol-function 'minibufferp) (lambda () t))
              ((symbol-function 'active-minibuffer-window) (lambda () t))
              ((symbol-function 'shaoline--ensure-shared-vars) (lambda () nil))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline--allow-empty-message nil))
        (shaoline--advice-preserve-empty-message
         (lambda (&rest _) (setq calls (1+ calls)) nil)
         "")))
    (should (= calls 1))))

;; ----------------------------------------------------------------------------
;; Test: reassert throttle through shaoline--last-display-time
;; ----------------------------------------------------------------------------

(ert-deftest shaoline-reassert-updates-last-display-time ()
  "A real re-assert must bump `shaoline--last-display-time' so subsequent
external (message nil) attacks don't see a stale anchor."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (eq setting 'always-visible)))
            ((symbol-function 'shaoline--should-yield-echo-area-p) (lambda () nil))
            ((symbol-function 'shaoline--echo-area-stable-p) (lambda () t))
            ((symbol-function 'current-message) (lambda () nil))
            ((symbol-function 'message) (lambda (&rest _) nil))
            ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
    (let ((shaoline-mode t)
          (shaoline--last-display-time 0))
      (shaoline--state-put :last-content "test content")
      (shaoline--reassert-yang-visibility)
      (should (> shaoline--last-display-time 0)))))

(ert-deftest shaoline-reassert-skipped-when-our-line-already-in-echo ()
  "When the current message is ours, re-assert must not run, and
therefore must not bump `shaoline--last-display-time'."
  (cl-letf (((symbol-function 'shaoline--resolve-setting)
             (lambda (setting) (eq setting 'always-visible)))
            ((symbol-function 'shaoline--should-yield-echo-area-p) (lambda () nil))
            ((symbol-function 'shaoline--echo-area-stable-p) (lambda () t))
            ((symbol-function 'current-message)
             (lambda () (propertize "test content" 'shaoline-origin t)))
            ((symbol-function 'message) (lambda (&rest _) nil))
            ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
    (let ((shaoline-mode t)
          (shaoline--last-display-time 0))
      (shaoline--state-put :last-content "test content")
      (shaoline--reassert-yang-visibility)
      ;; Not bumped because re-assert was a no-op.
      (should (= shaoline--last-display-time 0)))))

(ert-deftest shaoline-reassert-skips-when-this-command-opens-minibuffer ()
  "When `this-command' is `execute-extended-command' (M-x), `find-file'
(C-x C-f), or any consult-* / minibuffer-* prefixed command, re-assert
must NOT draw over the soon-to-appear minibuffer prompt.

Real-world flow that broke (fixed here):
  1. User presses `M-x'.
  2. `execute-extended-command' is queued; `post-command-hook' fires.
  3. `shaoline--echo-area-busy-p' returns nil (minibuffer not active yet),
     `shaoline--should-yield-echo-area-p' returns nil.
  4. `shaoline--reassert-yang-visibility' redraws shaoline over the
     about-to-be-rendered `M-x ' prompt.
  5. `minibuffer-setup-hook' then erases our line.
  6. User sees an empty echo-area until they type the first filter char
     (which triggers `completing-read' to refresh the candidate list).
  7. With this fix: re-assert must skip when `this-command' is a known
     minibuffer trigger."
  (let ((message-called nil)
        (cmds '(execute-extended-command find-file save-buffer
                eval-expression eval-defun eval-last-sexp)))
    (cl-letf (((symbol-function 'shaoline--resolve-setting)
               (lambda (s) (eq s 'always-visible)))
              ((symbol-function 'shaoline--should-yield-echo-area-p) (lambda () nil))
              ((symbol-function 'shaoline--echo-area-stable-p) (lambda () t))
              ((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _) (setq message-called t)))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      ;; Pre-populate state so a non-skipping re-assert would call `message'.
      (let ((shaoline-mode t)
            (shaoline--last-display-time 0))
        (shaoline--state-put :last-content "test content")
        (dolist (cmd cmds)
          (let ((message-called nil)
                (this-command cmd)
                (last-command 'forward-line))
            (shaoline--reassert-yang-visibility)
            (should-not message-called))))))
  ;; Also test the dynamic shortcuts: consult-* and minibuffer-* prefixes.
  (let ((message-called nil))
    (cl-letf (((symbol-function 'shaoline--resolve-setting)
               (lambda (s) (eq s 'always-visible)))
              ((symbol-function 'shaoline--should-yield-echo-area-p) (lambda () nil))
              ((symbol-function 'shaoline--echo-area-stable-p) (lambda () t))
              ((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _) (setq message-called t)))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline-mode t)
            (shaoline--last-display-time 0))
        (shaoline--state-put :last-content "test content")
        (dolist (cmd '(consult-find-file consult-apropos
                       minibuffer-complete minibuffer-completion-help))
          (let ((message-called nil)
                (this-command cmd)
                (last-command 'forward-line))
            (shaoline--reassert-yang-visibility)
            (should-not message-called))))))
  ;; Negative case: a non-minibuffer command must NOT be skipped.
  (let ((message-called nil))
    (cl-letf (((symbol-function 'shaoline--resolve-setting)
               (lambda (s) (eq s 'always-visible)))
              ((symbol-function 'shaoline--should-yield-echo-area-p) (lambda () nil))
              ((symbol-function 'shaoline--echo-area-stable-p) (lambda () t))
              ((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _) (setq message-called t)))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline-mode t)
            (shaoline--last-display-time 0))
        (shaoline--state-put :last-content "test content")
        (let ((this-command 'forward-line)
              (last-command 'previous-line))
          (setq message-called nil)
          (shaoline--reassert-yang-visibility)
          (should message-called))))))

(ert-deftest shaoline-reassert-skips-when-last-command-opened-minibuffer ()
  "When `last-command' is `execute-extended-command' (cancelled M-x), re-assert
must still skip. The user is back at the source buffer after `abort',
but the minibuffer clean-up may not have fully settled, and any stragglers
on the echo-area would confuse the user."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'shaoline--resolve-setting)
               (lambda (s) (eq s 'always-visible)))
              ((symbol-function 'shaoline--should-yield-echo-area-p) (lambda () nil))
              ((symbol-function 'shaoline--echo-area-stable-p) (lambda () t))
              ((symbol-function 'current-message) (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _) (setq message-called t)))
              ((symbol-function 'shaoline--log) (lambda (&rest _) nil)))
      (let ((shaoline-mode t)
            (shaoline--last-display-time 0))
        (shaoline--state-put :last-content "test content")
        ;; last = execute-extended-command (cancelled M-x), this = next command.
        (let ((this-command 'forward-line)
              (last-command 'execute-extended-command))
          (setq message-called nil)
          (shaoline--reassert-yang-visibility)
          (should-not message-called))))))

(provide 'shaoline-effects-test)
;;; shaoline-effects-test.el ends here
