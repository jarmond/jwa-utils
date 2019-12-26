(in-package #:cl-user)
(defpackage #:jwa-utils
  (:use #:common-lisp)
  (:export

   :while
   :until
   :for

   :string-cat
   :string-join
   :count-occurences

   :getf-many
   :plist-values

   :yes-no-ignore-or-quit

   :dolines
   :maplines))

(defpackage #:jwa-utils.size
  (:use #:cl
        #:jwa-utils)
  (:export
   :bytes-prefixed-string))

(defpackage #:jwa-utils.text
  (:use #:cl
        #:jwa-utils
        #:alexandria)
  (:export

   :sentence-delimiter-p
   :intra-word-punctuation-p
   :word-char-p
   :punctuation-char-p
   :whitespace-char-p

   :trim-if

   :split-sentences
   :split-words))
