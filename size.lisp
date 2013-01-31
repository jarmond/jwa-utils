;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; fill-column: 80; -*-

(in-package #:jwa-utils.size)

;;;; Utilities for dealing with sizes.

(defparameter *byte-prefixes*
  '("B" "KiB" "MiB" "GiB" "TiB" "PiB" "EiB" "ZiB" "YiB"))
(defparameter *max-byte-prefix* (1- (length *byte-prefixes*)))

(defun bytes-prefixed-string (bytes &optional (precision 1))
  "Formats a number of bytes to a string with appropriate suffix."
  (let ((exp (min (floor (log bytes 1024)) *max-byte-prefix*)))
    (format nil "~,vf ~a" precision
            (/ bytes (expt 1024 exp)) (nth exp *byte-prefixes*))))
