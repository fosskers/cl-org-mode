(defpackage org-mode/tests
  (:use :cl :parachute)
  (:local-nicknames (#:o #:org-mode)
                    (#:p #:parcom)))

(in-package :org-mode/tests)

(define-test markup)

(define-test links
  :parent markup
  (let ((l (p:parse #'o:link "[[https://www.fosskers.ca][Site]]")))
    (is string= "https://www.fosskers.ca" (o:url-text (o:link-url l)))
    (is string= "Site" (o:link-text l))))
