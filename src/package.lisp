(defpackage org-mode
  (:use :cl)
  (:shadow #:quote #:block #:time #:ratio)
  (:import-from :parcom #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)
                    (#:d #:parcom/datetime))
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

(defun string-starts-with? (s prefix &key (from 0))
  (string= prefix s :start2 from :end2 (min (+ from (length prefix))
                                            (length s))))

#+nil
(string-starts-with? "hello" "he")

(defun string-ends-with? (s postfix)
  (string= postfix s :start2 (max 0 (- (length s) (length postfix)))))

#+nil
(string-ends-with? "hello" "lo")
