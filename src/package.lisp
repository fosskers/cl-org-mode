(defpackage org-mode
  (:use :cl)
  (:shadow #:quote #:block #:time #:ratio)
  (:import-from :parcom #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)
                    (#:d #:parcom/datetime))
  ;; --- Entry --- ;;
  (:export #:from-string #:from-file)
  ;; --- Generics --- ;;
  (:export #:text)
  ;; --- Blocks --- ;;
  (:export #:paragraph #:paragraph-words)
  ;; --- Text Markup --- ;;
  (:export #:url #:url-text
           #:link #:link-url #:link-text
           #:image #:image-url
           #:plain #:plain-text)
  ;; --- Timestamps --- ;;
  (:export #:timestamp #:timestamp-day #:timestamp-day-of-week #:timestamp-time #:timestamp-repeat #:timestamp-delay
           #:delay #:delay-mode #:delay-value #:delay-interval)
  (:documentation "An Emacs org-mode parser."))

(in-package :org-mode)

;; --- Entry --- ;;

(defun from-string (str)
  "Parse an entire org file from a string."
  (p:parse (<* #'file #'p:eof) str))

(defun from-file (path)
  "Parse an entire org file given a path to it."
  (from-string (string-from-file path)))

;; --- Utilities --- ;;

(defun string-starts-with? (s prefix &key (from 0))
  (string= prefix s :start2 from :end2 (min (+ from (length prefix))
                                            (length s))))

#+nil
(string-starts-with? "hello" "he")

(defun string-ends-with? (s postfix)
  (string= postfix s :start2 (max 0 (- (length s) (length postfix)))))

#+nil
(string-ends-with? "hello" "lo")

(declaim (ftype (function ((or string pathname)) (simple-array character (*))) string-from-file))
(defun string-from-file (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input :element-type 'character)
    (with-output-to-string (out)
      (loop :for c := (read-char stream nil :eof)
            :until (eq c :eof)
            :do (write-char c out)))))

(defun list->vector (list)
  (coerce list 'vector))
