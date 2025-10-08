(defpackage org-mode/tests
  (:use :cl :parachute)
  (:local-nicknames (#:o #:org-mode)
                    (#:p #:parcom)))

(in-package :org-mode/tests)

(define-test markup)

(define-test links
  :parent markup
  (let ((l (p:parse #'o:link "[[https://www.fosskers.ca][Amazing Site]]")))
    (is string= "https://www.fosskers.ca" (o:url-text (o:link-url l)))
    (is equalp #("Amazing" "Site") (o:link-text l)))
  (let ((l (p:parse #'o:link "[[https://www.fosskers.ca]]")))
    (is string= "https://www.fosskers.ca" (o:url-text (o:link-url l))))
  (let ((l (p:parse #'o:link "[[https://www.fosskers.ca][Amazing *Site* Here]]")))
    (is equalp (vector "Amazing" (o::make-bold :text "Site") "Here") (o:link-text l)))
  (finish (p:parse #'o:link "#+ATTR_HTML: :title foo
[[https://www.fosskers.ca]]")))

(define-test images
  :parent markup
  (let ((l (p:parse #'o:image "[[/path/to/img.jpeg]]")))
    (is string= "/path/to/img.jpeg" (o:url-text (o:image-url l))))
  (finish (p:parse #'o:image "#+CAPTION: Hello
#+ATTR_HTML: :title foo
#+NAME: foo
[[/path/to/img.jpeg]]")))

(define-test timestamps
  (finish (p:parse #'o::timestamp "2025-08-31 So"))
  (let ((ts (p:parse #'o::timestamp "2021-04-28 Wed 13:00 .+1w -1d")))
    (is = 1 (o:delay-value (o:timestamp-delay ts)))))

(define-test headings
  (fail (p:parse #'o::heading "**Foo"))
  (is equalp
      #("A" "capital" "letter")
      (o::heading-text (p:parse #'o::heading "* A capital letter"))))

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

(define-test blocks)

(define-test paragraphs
  :parent blocks
  ;; `words' shouldn't parse an empty line.
  (fail (p:parse #'o::words "
"))
  (is equalp '("A") (p:parse #'o::line "A
B"))
  (is = 6 (length (o:paragraph-words (p:parse #'o::paragraph "First line.
Second line.
Third line.

Fourth line - shouldn't parse!"))))
  (is = 2 (length (o:paragraph-words (p:parse #'o::paragraph "Last line.
*** A header!
Paragraph of next section.")))))

(define-test drawers
  :parent blocks
  (fail (p:parse #'o::drawer ":MYDRAWER:
Hi"))
  (finish (p:parse #'o::drawer ":MYDRAWER:
:END:"))
  (finish (p:parse #'o::drawer ":MYDRAWER:
Great content.
This line too.
:END:"))
  (finish (p:parse #'o::drawer ":MYDRAWER:
Great content.

[[https://www.fosskers.ca]]

More!
:END:"))
  (let ((v (o:document-blocks (o:file-document (o:from-string "Still outside the drawer
:DRAWERNAME:
This is inside the drawer.
:END:
After the drawer.")))))
    (of-type o::paragraph (aref v 0))
    (of-type o::drawer (aref v 1))
    (of-type o::paragraph (aref v 2)))
  (finish (p:parse #'o::properties ":PROPERTIES:
:END:"))
  (finish (p:parse #'o::properties ":PROPERTIES:
:Yes: Fun
:END:"))
  (finish (p:parse #'o::logbook ":LOGBOOK:
:END:"))
  (finish (p:parse #'o::logbook ":LOGBOOK:
CLOCK: [2025-10-08 Mi 04:50]--[2025-10-08 Mi 06:20] =>  1:30
CLOCK: [2025-10-07 Di 07:08]--[2025-10-07 Di 07:57] =>  0:49
:END:")))

(define-test comments
  :parent blocks
  (finish (p:parse #'o:comment "# hello"))
  (fail (p:parse #'o:comment "#+hello: not a comment!"))
  (let ((blocks (o:document-blocks (o:file-document (p:parse #'o::file "Hello

# Comment
# Comment 2

* Heading")))))
    (of-type o:paragraph (aref blocks 0))
    (of-type o:comment (aref blocks 1))
    (is = 2 (length blocks))
    (is = 2 (length (o:text (p:parse #'o:comment "# hello
# goodbye

# this shouldn't be parsed as the same comment"))))
    (is = 2 (length (o:document-blocks (p:parse (o:document 0) "# hello
# goodbye

# this shouldn't be parsed as the same comment"))))))

(define-test lists
  :parent blocks
  (finish (p:parse #'o::listing "- A"))
  (finish (p:parse (o::depth-sensitive-listing 0) "  - B"))
  (is = 3 (length (o::listing-items (p:parse #'o::listing "- A
- B
- C

- D"))))
  (is = 2 (length (o::listing-items (p:parse #'o::listing "- A
  1. B
- C"))))
  (is equalp #("A" "B") (o::item-words (aref (o::listing-items (p:parse #'o::listing "- A
  B"))
                                             0)))
  (is equalp #("A") (o::item-words (aref (o::listing-items (p:parse #'o::listing "- A
B"))
                                         0)))
  (of-type o::ratio (o::item-progress (aref (o::listing-items (p:parse (p:<* #'o::listing #'p:eof) "- A [1/2]"))
                                            0)))
  (of-type o::ratio (o::item-progress (aref (o::listing-items (p:parse (p:<* #'o::listing #'p:eof) "- A [1/2]
  - B
  - C"))
                                            0))))

(define-test files)

(define-test parsing-files
  :parent files
  (finish (o:from-file "tests/empty.org"))
  (finish (o:from-file "tests/simple.org"))
  (finish (o:from-file "tests/src.org"))
  (finish (o:from-file "tests/tables.org"))
  (finish (o:from-file "tests/lists.org"))
  (finish (o:from-file "tests/headings.org"))
  (finish (o:from-file "tests/everything.org")))
