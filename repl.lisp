;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; fill-column: 80; -*-
;;;; jwa's REPL utilities

;;; Functions/macros either stolen from elsewhere (free or public domain
;;; sources) or written by me, for easing the REPL experience.

(defpackage #:jwa-repl
  (:use #:cl)
  (:nicknames #:r) ; R for REPL
  (:export #:me1 #:me #:deflex #:lex #:de #:ap #:hex #:bin))

(in-package #:jwa-repl)

(defmacro me1 (&rest args)
  "Alias for MACROEXPAND-1."
  `(pprint (macroexpand-1 ,@args)))

(defmacro me (&rest args)
  "Alias for MACROEXPAND."
  `(macroexpand ,@args))

(defmacro deflex (var val &optional (doc nil docp))
  "Define a top level (global) lexical VAR with initial value VAL,
which is assigned unconditionally as with DEFPARAMETER. If a DOC
string is provided, it is attached to both the name |VAR| and the
name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
kind 'VARIABLE. The new VAR will have lexical scope and thus may be
shadowed by LET bindings without affecting its dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
         (s1 (symbol-name var))
         (s2 (symbol-name '#:*))
         (s3 (symbol-package var))	; BUGFIX [see above]
         (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must be the last thing we do so
    ;; that the value of the form is the symbol VAR.
    (if docp
        `(progn
           (defparameter ,backing-var ,val ,doc)
           (setf (documentation ',var 'variable) ,doc)
           (define-symbol-macro ,var ,backing-var))
        `(progn
           (defparameter ,backing-var ,val)
           (define-symbol-macro ,var ,backing-var))))
  ;;; DEFLEX is
  ;;; Copyright (c) 2003-2007, 2011 Rob Warnock <rpw3@rpw3.org>.
  ;;; All Rights Reserved.
  ;;;
  ;;; Permission to use, copy, modify, and/or distribute this software for any
  ;;; purpose with or without fee is hereby granted, provided that the above
  ;;; copyright notice and this permission notice appear in all copies.
  ;;;
  ;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  ;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  ;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  ;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  ;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
)

(defmacro lex (&rest args)
  "Alias for DEFLEX."
  `(deflex ,@args))

(defun de (&rest args)
  "Alias for DESCRIBE."
  (apply #'describe args))

(defun ap (&rest args)
  "Alias for APROPOS."
  (apply #'apropos args))

(defun hex (value &optional (size 4))
  "Print value as hexadecimal."
  (format t "~v,'0X" size value))

(defun bin (value &optional (size 8))
  "Print value as binary."
  (format t "~v,'0B" size value))

(defun dis (&rest args)
  "Alias for DISASSEMBLE."
  (apply #'disassemble args))
