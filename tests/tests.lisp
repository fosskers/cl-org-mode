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
    (is string= "Site" (o:link-text l)))
  (let ((l (p:parse #'o:link "[[https://www.fosskers.ca]]")))
    (is string= "https://www.fosskers.ca" (o:url-text (o:link-url l)))))

(define-test images
  :parent markup
  (let ((l (p:parse #'o:image "[[/path/to/img.jpeg]]")))
    (is string= "/path/to/img.jpeg" (o:url-text (o:image-url l)))))
