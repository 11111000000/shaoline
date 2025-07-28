;;; shaoline-macros.el --- Macros for Shaoline -*- lexical-binding: t; -*-

;; Version: 2.3.0

;; Copyright (C) 2025 Peter
;; Author: Peter <11111000000@email.com>
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/11111000000/shaoline

;; This file contains only macros for compile-time use in shaoline.el and shaoline-segments.el.

;;; Code:

(defmacro shaoline-define-segment (name args &rest body)
  "Define segment NAME taking ARGS, register it, and return its symbol.
Every segment is independent; every function a ripple in the pond."
  (declare (indent defun))
  (let ((func `(defun ,name ,args ,@body)))
    `(progn
       ,func
       (puthash ',name #',name shaoline--segment-table)
       ',name)))

(defmacro shaoline-define-simple-segment (name docstring &rest body)
  "Define a segment NAME with no arguments. BODY is run on each render.
True elegance: nothing more than necessary."
  `(shaoline-define-segment ,name ()
     ,docstring
     ,@body))

;; Cached segment helper
;;
;; Simple TTL-based caching to avoid heavy I/O on every post-command.
;;
;;   (shaoline-define-cached-segment name ttl "Docstring" BODY…)
;;
;; BODY is evaluated only when the cached value is older than TTL seconds;
;; otherwise the cached value is returned immediately.
(defmacro shaoline-define-cached-segment (name ttl docstring &rest body)
  "Define NAME as a cached segment.
TTL is time-to-live in seconds. DOCSTRING documents the segment.
BODY computes and returns the segment string."
  (declare (indent defun) (doc-string 3))
  (let* ((cache-var (intern (format "shaoline--%s-cache" (symbol-name name))))
         (ts-var    (intern (format "shaoline--%s-ts" (symbol-name name)))))
    `(progn
       (defvar ,cache-var nil)
       (defvar ,ts-var 0)
       (shaoline-define-simple-segment ,name ,docstring
                                       (let ((now (float-time)))
                                         (if (and ,cache-var
                                                  (< (- now ,ts-var) ,ttl))
                                             ,cache-var
                                           (setq ,cache-var (progn ,@body)
                                                 ,ts-var now)))))))

(provide 'shaoline-macros)

;;; shaoline-macros.el ends here
