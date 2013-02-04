;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; fill-column: 80; -*-

;;;; Text processing.

(in-package #:jwa-utils.text)

;;;
;;; Constants
;;;

(define-constant +first-ascii-printable-code+ (char-code #\Space))
(define-constant +last-ascii-printable-code+ (char-code #\~))

;;;
;;; Predicates
;;;

(defun sentence-delimiter-p (c)
  "Return true if c is a sentence delimiting character."
  (and (characterp c)
       (or (char= c #\.)
           (char= c #\!)
           (char= c #\?))))

(defun intra-word-punctuation-p (c)
  "Return true if c is an character which is a valid punctuation character
within a (English) word."
  (and (characterp c)
       (or (char= c #\')
           (char= c #\-))))

(defun word-char-p (c)
  "Return true if c is an character which can form part of a (English) word."
  (or (alpha-char-p c)
      (intra-word-punctuation-p c)))

(defun ascii-printable-p (c)
  "Return true if c is an ASCII printable character."
  (let ((code (char-code c)))
    (and (characterp c)
         (>= code +first-ascii-printable-code+)
         (<= code +last-ascii-printable-code+))))

;;;
;;; Splitters
;;;

(defun split-sentences (text)
  "Split text into list of sentences."
  (split-sequence:split-sequence-if #'sentence-delimiter-p text
                                    :remove-empty-subseqs t))

(defun split-words (sentence)
  "Break sentence into a list of words."
  (split-sequence:split-sequence-if-not #'word-char-p sentence
                                        :remove-empty-subseqs t))

;;;
;;; Iteration
;;;

;; ??? use (mapc ?)
;; (defmacro dochars ((var string &key test) &body body)
;;   "Execute body for characters passing test with character bound to var."
  