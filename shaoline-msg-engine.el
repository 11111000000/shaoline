;;; shaoline-msg-engine.el --- Shaoline echo message engine -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; Silent implementation. Not for loading by user.

;; ----------------------------------------------------------------------------
;; State.

(defvar shaoline-msg--last-user-message nil
  "Last user message captured by Shaoline (string or nil).")

(defvar shaoline-msg--last-user-message-ts 0
  "Timestamp (float value) when the last user message appeared.")

;; ----------------------------------------------------------------------------
;; Actions.

(defun shaoline-msg-clear ()
  "Clear last user message and timestamp."
  (setq shaoline-msg--last-user-message nil
        shaoline-msg--last-user-message-ts 0))

(defun shaoline-msg-save (str)
  "Save STR as the last user message and record its timestamp."
  (setq shaoline-msg--last-user-message str
        shaoline-msg--last-user-message-ts (float-time)))

;; ----------------------------------------------------------------------------
;; Query.

(defun shaoline-msg-active-p (timeout)
  "Return non-nil if the current user message is still within its timeout.
If the message exceeds 40 characters, timeout is multiplied accordingly."
  (and shaoline-msg--last-user-message
       (let* ((base (max 0 timeout))
              (mult (max 1.0 (/ (length shaoline-msg--last-user-message) 40.0))))
         (< (float-time (time-since shaoline-msg--last-user-message-ts))
            (* base mult)))))

(defun shaoline-msg-current ()
  "Return the current user message string, or nil."
  shaoline-msg--last-user-message)

(defun shaoline-msg-age ()
  "Return the age (seconds, float) of the current user message, or 0."
  (if shaoline-msg--last-user-message-ts
      (float-time (time-since shaoline-msg--last-user-message-ts))
    0))

(provide 'shaoline-msg-engine)
