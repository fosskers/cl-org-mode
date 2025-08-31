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

(define-test time)

(define-test timestamps
  :parent time
  (finish (p:parse #'o::timestamp "2025-08-31 So"))
  (let ((ts (p:parse #'o::timestamp "2021-04-28 Wed 13:00 .+1w -1d")))
    (is = 1 (o:delay-value (o:timestamp-delay ts)))))
