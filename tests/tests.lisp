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

(define-test headings)

(define-test tags
  :parent headings
  (is equalp #("foo" "bar" "baz") (p:parse #'o::tags ":foo:bar:baz:"))
  (is equalp #("foo" "bar" "baz") (p:parse #'o::tags ":foo:bar:baz:
SCHEDULED: <2025-09-01>"))
  (is equalp #(")") (o::heading-tags (p:parse #'o::heading "** Is a tag! :):")))
  (is equalp #() (o::heading-tags (p:parse #'o::heading "** Not a tag! :)"))))

(define-test bullets
  :parent headings
  (is = 3 (p:parse #'o::bullets-of-heading "*** Hello"))
  (fail (p:parse #'o::bullets-of-heading "Hello")))

(define-test illegal-headings
  :parent headings
  (fail (p:parse #'o::heading "**Foo")))

(define-test blocks)

(define-test paragraphs
  :parent blocks
  (is string= "A" (car (p:parse #'o::line "A
B")))
  (is = 6 (length (o:paragraph-words (p:parse #'o::paragraph "First line.
Second line.
Third line.

Fourth line - shouldn't parse!"))))
  (is = 2 (length (o:paragraph-words (p:parse #'o::paragraph "Last line.
*** A header!
Paragraph of next section.")))))

(define-test comments
  :parent blocks
  (finish (p:parse #'o::comment "# hello"))
  (fail (p:parse #'o::comment "#+hello: not a comment!")))

(define-test files)

(define-test parsing-files
  :parent files
  (finish (o:from-file "tests/empty.org"))
  (finish (o:from-file "tests/simple.org"))
  (finish (o:from-file "tests/src.org"))
  (finish (o:from-file "tests/tables.org"))
  (finish (o:from-file "tests/everything.org")))
