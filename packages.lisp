(in-package #:cl-user)
(defpackage #:jwa-utils
  (:use #:common-lisp)
  (:export

   :last1
   :singleton-p
   :append1
   :conc1
   :mklist

   :longer
   :filter
   :group

   :prune

   :find-with-values
   :before
   :after
   :duplicate
   :split-if

   :most
   :best
   :mostn

   :mapa-b
   :map1-n
   :map0-n
   :map->
   :mapcars
   :rmapcar

   :readlist
   :prompt
   :break-loop

   :mkstr
   :reread
   :explode

   :while
   :until
   :for
   :if-result

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

(defpackage #:jwa-utils.reader
  (:use #:cl #:alexandria))

(defpackage #:jwa-utils.macros
  (:use #:cl))

