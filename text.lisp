;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; fill-column: 80; -*-

;;;; Text processing.

(in-package #:jwa-utils.text)

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

(defun punctuation-char-p (c)
  "Return true if c is an (English) punctuation character."
  (and (characterp c)
       (member c '(#\( #\) #\: #\, #\- #\! #\. #\? #\` #\' #\" #\; #\/))))

(defun whitespace-char-p (c)
  "Return true if c is whitespace."
  (and (characterp c)
       (or (char= c #\Space)
           (char= c #\Tab)
           (char= c #\Newline))))

;;;
;;; Trimming
;;;

(defun trim-if (predicate sequence)
  "Remove elements satisfying predicate from beginning and end of sequence."
  (let* ((length (length sequence)) (start 0) (end (1- length)))
    (while (and (< start length) (funcall predicate (elt sequence start)))
      (incf start))
    (while (and (> end start) (funcall predicate (elt sequence end)))
      (decf end))
    (subseq sequence start (1+ end))))

;;;
;;; Splitters
;;;

(defun split-sentences (text)
  "Split text into list of sentences."
  (split-sequence:split-sequence-if #'sentence-delimiter-p text
                                    :remove-empty-subseqs t))

(defun split-words (sentence)
  "Break sentence into a list of words."
  (mapcar (lambda (word) (trim-if #'punctuation-char-p word))
          (split-sequence:split-sequence-if #'whitespace-char-p sentence
                                            :remove-empty-subseqs t)))

;;;
;;; Iteration
;;;

;; ??? use (mapc ?)
;; (defmacro dochars ((var string &key test) &body body)
;;   "Execute body for characters passing test with character bound to var."
  
