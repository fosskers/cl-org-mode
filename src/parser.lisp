;;; A grammar for org-mode files. Ideally this parses everything that the
;;; org-mode Emacs package itself would parse. If it does not, a test case
;;; should be added demonstrating the failure, followed by a patch to fix it.
;;; org-mode otherwise has no spec; the org-mode Emacs package itself is the
;;; living reality which we here have to keep up with. That said, 95% of use
;;; cases of 95% of users should already be covered here.
;;;
;;; A note on lists: Their "bullet type" can be mixed, but org-mode itself will
;;; unify them if you S-Right or S-Left, implying that it is still a single
;;; list. This Lisp library will also only recognize the bullet type of the
;;; first item and assume all others obey it, thereby unifying them
;;; automatically upon rerendering. With regards to list item indentation,
;;; content must be aligned with the start of the previous line in order to be
;;; considered the same item:
;;;
;;; - A
;;; - B
;;;   This is also B.
;;; - C
;;;
;;; However:
;;;
;;; - A
;;; - B
;;; This is not B.
;;; - C (a separate list)
;;;
;;; We also consider a multiline break between two list items to indicate two
;;; lists.
;;;
;;; A note on comments: they can only appear on their own line, never after some
;;; content on the same line like in other programming languages. This prevents
;;; confusion around the content of headings and list items.

(in-package :org-mode)

;; --- Static Parsers --- ;;

(defparameter +h+     (p:char #\h))
(defparameter +d+     (p:char #\d))
(defparameter +w+     (p:char #\w))
(defparameter +m+     (p:char #\m))
(defparameter +y+     (p:char #\y))
(defparameter +x+     (p:char #\x))
(defparameter +capital-x+ (p:char #\X))
(defparameter +colon+ (p:char #\:))
(defparameter +dash+  (p:char #\-))
(defparameter +dashes+  (p:string "--"))
(defparameter +arrow+  (p:string "=>"))
(defparameter +equal+ (p:char #\=))
(defparameter +plus+  (p:char #\+))
(defparameter +slash+ (p:char #\/))
(defparameter +star+  (p:char #\*))
(defparameter +tilde+ (p:char #\~))
(defparameter +under+ (p:char #\_))
(defparameter +zero+  (p:char #\0))
(defparameter +space+ (p:char #\space))
(defparameter +pipe+  (p:char #\|))
(defparameter +paren+ (p:char #\)))
(defparameter +period+ (p:char #\.))
(defparameter +newline+ (p:char #\newline))
(defparameter +scheduled+ (p:string "SCHEDULED:"))
(defparameter +deadline+  (p:string "DEADLINE:"))
(defparameter +closed+    (p:string "CLOSED:"))
(defparameter +properties+ (p:string ":PROPERTIES:"))
(defparameter +logbook+ (p:string ":LOGBOOK:"))
(defparameter +clock+ (p:string "CLOCK:"))
(defparameter +end+        (p:string ":END:"))
(defparameter +footnote-start+ (p:string "[fn:"))
(defparameter +label-start+ (p:string "#+"))
(defparameter +results-start+ (p:string "#+RESULTS:"))
(defparameter +caption-start+ (p:alt (p:string "#+CAPTION") (p:string "#+caption")))
(defparameter +plot-start+ (p:alt (p:string "#+PLOT: ") (p:string "#+plot: ")))
(defparameter +name+ (p:alt (p:string "#+NAME: ") (p:string "#+name: ")))
(defparameter +angle-open+  (p:char #\<))
(defparameter +angle-close+ (p:char #\>))
(defparameter +percent+ (p:char #\%))
(defparameter +octothorp+ (p:char #\#))
(defparameter +bracket-open+  (p:char #\[))
(defparameter +bracket-close+ (p:char #\]))
(defparameter +quote-open+ (p:alt (p:string "#+BEGIN_QUOTE") (p:string "#+begin_quote")))
(defparameter +quote-close+ (p:alt (p:string "#+END_QUOTE") (p:string "#+end_quote")))
(defparameter +example-open+ (p:alt (p:string "#+BEGIN_EXAMPLE") (p:string "#+begin_example")))
(defparameter +example-close+ (p:alt (p:string "#+END_EXAMPLE") (p:string "#+end_example")))
(defparameter +code-open+ (p:alt (p:string "#+BEGIN_SRC") (p:string "#+begin_src")))
(defparameter +code-close+ (p:alt (p:string "#+END_SRC") (p:string "#+end_src")))
(defparameter +tblfm+ (p:string "#+TBLFM: "))
(defparameter +hline-start+ (p:string "|-"))
(defparameter +consume-space+ (p:consume (lambda (c) (char= c #\space))))
(defparameter +consume-between-a-line+ (*> +consume-space+ +newline+ +consume-space+))
(defparameter +consume-junk+ (p:consume #'p:space?))
(defparameter +consume-til-end+ (p:consume (lambda (c) (not (char= c #\newline)))))
(defparameter +between-brackets+ (p:between +bracket-open+
                                            (p:take-while1 (lambda (c) (not (char= c #\]))))
                                            +bracket-close+))
(defparameter +take1-til-end+ (p:take-while1 (lambda (c) (not (char= c #\newline)))))
(defparameter +take1-til-break+ (p:take-while1 (lambda (c) (not (or (char= c #\space)
                                                                    (char= c #\newline))))))
(defparameter +take-til-pipe+ (p:take-while (lambda (c) (not (char= c #\|)))))
(defparameter +pipe-then-space+ (*> +pipe+ +consume-space+))
(defparameter +any-small+ (p:any-if (lambda (c) (char<= #\a c #\z))))
(defparameter +any-big+ (p:any-if (lambda (c) (char<= #\A c #\Z))))

;; --- Utilities --- ;;

(defun consume-n (n p)
  "Combinator: A logical combination of `consume' and `take', such that only N-many
of the to-be-consumed characters are consumed."
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((keep (loop :for i fixnum :from offset :below p::*input-length*
                       :while (< (- i offset) n)
                       :while (funcall p (schar p::*input* i))
                       :finally (return (- i offset))))
           (next (p::off keep offset)))
      (cond ((< keep n) (p:fail offset))
            (t (values next next))))))

#+nil
(p:parse (consume-n 3 (lambda (c) (char= c #\a))) "aabbb")

(defun valid (pred par)
  "Combinator: The result of a given parser must pass a certain predicate,
or the parse will fail."
  (lambda (offset)
    (multiple-value-bind (res next) (funcall par offset)
      (cond ((p:failure? res) (p:fail next))
            ((not (funcall pred res)) (p:fail offset))
            (t (values res next))))))

#+nil
(p:parse (valid (lambda (n) (> n 5)) #'p:unsigned) "2")

;; --- Whole Files --- ;;

(defun file (offset)
  "Parser: An entire .org file."
  (funcall (p:ap (lambda (meta doc) (make-file :metadata meta :document doc))
                 #'metadata
                 (*> +consume-junk+ (document 0)))
           offset))

#+nil
(from-file "tests/lists.org")

(defun metadata (offset)
  "Parser: All extra information at the top of the file."
  (funcall (p:ap (lambda (comments props pairs)
                   (make-metadata :comments (coerce comments 'vector)
                                  :properties props
                                  :metadata pairs))
                 (*> +consume-junk+ (p:sep-end +newline+ #'comment))
                 (*> +consume-junk+ (p:opt #'properties))
                 (*> +consume-junk+ #'metadata-kv-pairs))
           offset))

#+nil
(p:parse #'metadata "#+title: 2025")

#+nil
(p:parse #'metadata "
# foo
# bar

:PROPERTIES:
:ID:       666c8e46-356f-40c5-9c07-e355b890f0a8
:END:

#+title: Fun Article
#+filetags: writeup
")

(defun metadata-kv-pairs (offset)
  "Parser: All the key-value pairs at the top of the file."
  (p:fmap (lambda (list)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (pair list)
                (setf (gethash (car pair) ht) (cdr pair)))
              ht))
          (funcall (p:sep-end +newline+ #'kv-pair) offset)))

#+nil
(p:parse #'metadata-kv-pairs "#+title: great
#+date: 2025-09-19
#+author: Colin

Content")

(defun comment (offset)
  ;; FIXME: 2025-09-19 Should this proactively trim whitespace off the end?
  ;;
  ;; 2025-10-02 At least it should not do so for the beginning of the string, as
  ;; it's common for users to format messages by hand with leading spaces.
  (funcall (p:ap (lambda (text) (make-comment :text (coerce text 'vector)))
                 (p:sep-end1
                  +newline+
                  (*> (p:not #'label)
                      +octothorp+
                      (p:take-while (lambda (c) (not (char= c #\newline)))))))
           offset))

#+nil
(p:parse #'comment "# hello ")
#+nil
(p:parse #'comment "#+hello: not a comment!")
#+nil
(p:parse #'comment "# hello
# goodbye

# this shouldn't be parsed as the same comment")

(defun label (offset)
  "Parser: The key of some key-value pair. Used at the top level, but also on
tables and source blocks."
  (funcall (*> +label-start+
               (<* (p:take-while1 (lambda (c) (not (char= c #\:))))
                   +colon+))
           offset))

#+nil
(p:parse #'label "#+name: the100")

(defun kv-pair (offset)
  (funcall (p:ap #'cons
                 #'label
                 (*> +consume-space+
                     (p:take-while (lambda (c) (not (char= c #\newline))))))
           offset))

#+nil
(p:parse #'kv-pair "#+name: table")

;; --- Documents and Sections --- ;;

(defun document (stars)
  "Parser: Many blocks and any subsections of deeper depth."
  (lambda (offset)
    (funcall (p:ap (lambda (blocks sections)
                     (make-document :blocks (coerce blocks 'vector)
                                    :sections (coerce sections 'vector)))
                   (p:sep-end +consume-junk+ #'block)
                   (*> +consume-junk+
                       ;; NOTE: 2025-09-17 There was originally a `sep-end'
                       ;; here, but it turned out that deeper steps already
                       ;; consume all the junk between major blocks of text, so
                       ;; we couldn't reliably expect there to be a newline here
                       ;; to separate the sections. Hence this was relaxed to
                       ;; just a `many'.
                       (p:many (section (1+ stars)))))
             offset)))

#+nil
(p:parse #'file "Hello

# Comment
# Comment 2

* Heading

Stuff")

(defun section (stars)
  "Parser: A heading and any subsequent content."
  (lambda (offset)
    (funcall (p:ap (lambda (head doc) (make-section :heading head :document doc))
                   (depth-sensitive-heading stars)
                   (*> +consume-junk+ (document stars)))
             offset)))

#+nil
(p:parse (document 0) "*Bold text* and not a heading.

This should really just be a paragraph.

* Now /this/ is a heading!")

#+nil
(draw-doc-tree (p:parse (document 0) "* Grand Plans

Eloquent thoughts.

** Details

Minute descriptions.

Extra things. A second paragraph!

*** Three deep

** Addendum

Extra things.

* Back to the top
* Nothing in between

Yes."))

;; --- Blocks --- ;;

(defun complex-object (offset)
  "Parser: All the major org-mode 'objects' that can appear in a section of text."
  (funcall (p:alt #'complex-object-not-drawer #'drawer) offset))

(defun complex-object-not-drawer (offset)
  (funcall (p:alt #'comment #'quote #'example #'code #'result #'table #'listing) offset))

(defun block (offset)
  "Parser: A complex object or a plain paragraph."
  (funcall (p:alt #'complex-object #'paragraph) offset))

#+nil
(p:parse #'block "(/Markup/).")

;; NOTE: 2025-10-08 As with `paragraph-in-drawer', this is explicitly duplicated
;; for performance / code organization reasons. Since the generalization cases
;; for `paragraph' are O(c) in number, we need not make it fully parameterized.
;; This should also help me keep track of things easier within tracing results.
;;
;; Note also that blocks within drawers can't contain other drawers.
(defun block-in-drawer (offset)
  "Parser: Like `block', but can't contain other drawers."
  (funcall (p:alt #'complex-object-not-drawer #'paragraph-in-drawer) offset))

(defun listing (offset)
  (funcall (depth-sensitive-listing -1) offset))

#+nil
(p:parse #'listing "- A [1/2]
  - [x] B
  - [ ] C")

(defun depth-sensitive-listing (depth)
  (lambda (offset)
    (multiple-value-bind (res next)
        ;; Aren't I clever. We want to test for the existence of a list bullet,
        ;; but the value we want to continue with is the number of spaces parsed
        ;; before that bullet, hence `<*'.
        (funcall (p:peek (<* +consume-space+ #'list-bullet)) offset)
      (declare (ignore next))
      (cond
        ;; This was not at all anything that looks like a list item.
        ((p:failure? res) (p:fail offset))
        ;; Yes we found something that looks like a list item, but it has a
        ;; smaller indent than what we're currently looking for. This implies
        ;; that we are currently try to parse a child list, but found an item
        ;; that is actually a child of a higher layer.
        ((<= (- res offset) depth) (p:fail offset))
        (t (funcall (p:ap (lambda (type items)
                            (make-listing :type type
                                          :items (coerce items 'vector)))
                          ;; FIXME: 2025-09-29 We do have to repeat the parse of
                          ;; the bullet type here though, which is pretty
                          ;; annoying. And it'll get parsed a third time down in
                          ;; `list-item'.
                          (p:peek (*> +consume-space+ #'list-bullet))
                          ;; NOTE: 2025-09-30 This initial search for a newline
                          ;; might look strange, but trust me that this was a
                          ;; bugfix for correctly parsing further top-level
                          ;; items after children had been parsed.
                          (p:many1 (*> (p:opt +newline+)
                                       (list-item (- res offset)))))
                    offset))))))

#+nil
(p:parse #'listing "- A
  1. B
- C")

#+nil
(p:parse #'listing "- A
  *B*")

#+nil
(p:parse #'listing "- A
B")

#+nil
(p:parse #'listing "- *A
  B*")

#+nil
(p:parse #'bold "*A
B*")

(defun list-bullet (offset)
  (funcall (p:alt (<$ :bulleted +dash+)
                  (<$ :plussed  +plus+)
                  (<$ :numbered (*> #'p:unsigned +period+))
                  (<$ :numpar   (*> #'p:unsigned +paren+))
                  (<$ :letter-small (*> +any-small+ +period+))
                  (<$ :letter-big (*> +any-big+ +period+))
                  (<$ :letter-par-small (*> +any-small+ +paren+))
                  (<$ :letter-par-big (*> +any-big+ +paren+)))
           offset))

#+nil
(p:parse #'list-bullet "1.")

(defun list-item (depth)
  (lambda (offset)
    (funcall
     (p:ap (lambda (status words progress sublist)
             (make-list-item :status status
                             :words (apply #'concatenate 'vector words)
                             :progress progress
                             :sublist sublist))
           (*> (consume-n depth (lambda (c) (char= c #\space)))
               #'list-bullet
               +space+ +consume-space+
               (p:opt #'status-of-list-item))
           (*> +consume-space+
               (p:sep1
                (*> +newline+
                    ;; NOTE: 2025-10-01 There's some sensitivity to the level of
                    ;; indentation here. It can't be identical to `depth' as
                    ;; that causes false positives in cases like:
                    ;;
                    ;; - A
                    ;; B
                    ;;
                    ;; where B should not be considered part of the list item,
                    ;; but a new paragraph in its own right. This is even
                    ;; clearer in the case of child lists:
                    ;;
                    ;; - A
                    ;;   - B
                    ;; C
                    ;;
                    ;; Here, clearly C should not be considered part of the same
                    ;; line content as A, as upon reformatting it would appear
                    ;; as:
                    ;;
                    ;; - A C
                    ;;   - B
                    (consume-n (1+ depth) (lambda (c) (char= c #\space)))
                    +consume-space+
                    (p:not #'list-bullet)
                    (p:not +newline+))
                #'list-item-line))
           (p:opt (*> +consume-space+ #'progress))
           (p:opt (*> +consume-space+ +newline+ (depth-sensitive-listing depth))))
     offset)))

#+nil
(p:parse (list-item 0) "- Hello there")
#+nil
(p:parse (list-item 0) "- [x] Water the cat")
#+nil
(p:parse (list-item 0) "- A
  1. B
- C")
#+nil
(p:parse (list-item 0) "- Hello there [1/2]")

(defun status-of-list-item (offset)
  (funcall (p:between +bracket-open+
                      (p:alt (<$ :open +space+)
                             (<$ :progress +dash+)
                             (<$ :done (p:alt +x+ +capital-x+)))
                      +bracket-close+)
           offset))

#+nil
(p:parse #'list-item-status "[x]")

(defun list-item-line (offset)
  (funcall (p:ap (lambda (list) (coerce list 'vector))
                 (p:sep-end1 (*> +consume-space+
                                 (p:not #'progress))
                             #'words))
           offset))

#+nil
(p:parse #'list-item-line "Hello *there* [1/2]")

(defun table (offset)
  (funcall (p:ap (lambda (caption attrs plot name rows form)
                   (make-table :caption caption
                               :attrs attrs
                               :plot plot
                               :name name
                               :rows (coerce rows 'vector)
                               :form form))
                 (p:opt (<* #'caption +consume-space+ +newline+))
                 (p:opt (<* #'attrs +newline+))
                 (p:opt (<* #'plot +newline+))
                 (p:opt (<* #'name +newline+))
                 (p:sep-end1 +newline+ #'row)
                 (p:opt #'formula))
           offset))

#+nil
(p:parse #'table "| A | B | C |
|---+---+---|
| D |   | E |")

#+nil
(p:parse #'table "#+name: table
| A | B | C |
|---+---+---|
| D |   | E |")

#+nil
(p:parse #'table "| A | B | C |
|---+---+---|
| D |   | E |
#+TBLFM: $total=vsum(@I..@II)")

#+nil
(p:parse #'table "| A | *B* | C |
|---+---+---|
| D |   | E |")

#+nil
(p:parse #'table "#+CAPTION[short]: long
#+ATTR_HTML: :border 2 :rules all :frame border
#+PLOT: title:\"Citas\" ind:1 deps:(3) type:2d with:histograms set:\"yrange [0:]\"
#+NAME: cities
| Sede      | Max cites | H-index |
|-----------+-----------+---------|
| Chile     |    257.72 |   21.39 |
| Leeds     |    165.77 |   19.68 |
| Sao Paolo |     71.00 |   11.50 |
| Stockholm |    134.19 |   14.33 |
| Morelia   |    257.56 |   17.67 |
#+TBLFM: $total=vsum(@I..@II)")

(defun caption (offset)
  (funcall (p:ap (lambda (short long) (make-caption :short short :long long))
                 (*> +caption-start+
                     (p:opt (p:between +bracket-open+
                                       (p:take-while (lambda (c) (and (not (char= c #\newline))
                                                                      (not (char= c #\])))))
                                       +bracket-close+)))
                 (*> +colon+ +space+ +consume-space+ +take1-til-end+))
           offset))

#+nil
(p:parse #'caption "#+CAPTION[short]: long")

(defun plot (offset)
  "Parser: A GnuPlot line above a table."
  (funcall (*> +plot-start+ +consume-space+ +take1-til-end+) offset))

#+nil
(p:parse #'plot "#+PLOT: title:\"Citas\" ind:1 deps:(3) type:2d with:histograms set:\"yrange [0:]\"")

(defun formula (offset)
  "Parser: A table formula."
  (funcall (*> +tblfm+ +take1-til-end+) offset))

#+nil
(p:parse #'formula "#+TBLFM: $total=vsum(@I..@II)")

(defun row (offset)
  (funcall (p:alt (<$ :hline (*> +hline-start+ +consume-til-end+))
                  (p:ap #'list->vector
                        (*> +pipe-then-space+
                            (p:sep-end +pipe-then-space+
                                       (*> (p:not +newline+)
                                           (p:not #'p:eof)
                                           #'cell)))))
           offset))

#+nil
(p:parse #'row "|----+----|")
#+nil
(p:parse #'row "| A | B ||")
#+nil
(p:parse #'row "| A | B *B* B | C |")

(defun cell (offset)
  "Like `line', but limited to a single table cell."
  (funcall (p:ap #'list->vector
                 (p:sep-end +consume-space+ (*> (p:not +pipe+) #'words-in-cell)))
           offset))

#+nil
(p:parse #'cell "hello *there* sir |")

(defun paragraph (offset)
  "A single body of text which runs until a double-newline or a header is
encountered."
  (p:fmap (lambda (lists) (make-paragraph :words (coerce (apply #'append lists) 'vector)))
          (funcall (p:sep-end1 +newline+
                               (*> (p:not #'heading)
                                   (p:not #'complex-object)
                                   #'line))
                   offset)))

#+nil
(p:parse #'paragraph "Single line.")

#+nil
(p:parse #'paragraph "First line.
Second line.
Third line.

Fourth line - shouldn't parse!")

#+nil
(p:parse #'paragraph "Last line.
*** A header!
Paragraph of next section.")

;; NOTE: 2025-10-08 At first glance it appears strange that this is mostly
;; duplicated from `paragraph', but I've done so for ergonomic / performance /
;; code organization reasons. See also `block'.
(defun paragraph-in-drawer (offset)
  "A single body of text which runs until a double-newline, header, or drawer :END:
is encountered."
  (p:fmap (lambda (lists) (make-paragraph :words (coerce (apply #'append lists) 'vector)))
          (funcall (p:sep-end1 +newline+
                               (*> (p:not #'heading)
                                   (p:not +end+)
                                   (p:not #'complex-object-not-drawer)
                                   #'line))
                   offset)))

;; FIXME: 2025-09-01 Account for internal markup?
(defun quote (offset)
  "Parser: A quote block."
  (p:fmap (lambda (list) (make-quote :text (coerce list 'vector)))
          (funcall (p:between (*> +quote-open+ +newline+)
                              (p:sep-end +newline+
                                         (*> (p:not +quote-close+)
                                             (p:take-while (lambda (c) (not (char= c #\newline))))))
                              +quote-close+)
                   offset)))

#+nil
(p:parse #'quote "#+begin_quote
人生遍路なり

同行二人
#+end_quote")

#+nil
(p:parse #'quote "#+begin_quote
#+end_quote")

(defun example (offset)
  "Parser: An example block."
  (p:fmap (lambda (list) (make-example :text (coerce list 'vector)))
          (funcall (p:between (*> +example-open+ +newline+)
                              (p:sep-end +newline+
                                         (*> (p:not +example-close+)
                                             (p:take-while (lambda (c) (not (char= c #\newline))))))
                              +example-close+)
                   offset)))

#+nil
(p:parse #'example "#+begin_example
The first thing you need to do.

Now the second thing.
#+end_example")

#+nil
(p:parse #'example "#+begin_example
#+end_example")

(defun code (offset)
  "Parser: A src code block."
  (funcall (p:ap (lambda (name lang vars code)
                   (make-code :name name
                              :lang lang
                              :vars vars
                              :text (coerce code 'vector)))
                 (p:opt (<* #'name +newline+))
                 (*> +code-open+
                     +consume-space+
                     (p:take-while (lambda (c) (not (or (char= c #\space)
                                                        (char= c #\newline))))))
                 (p:opt (*> +consume-space+ #'variables))
                 (*> +newline+
                     (<* (p:sep-end +newline+
                                    (*> (p:not +code-close+)
                                        +take1-til-end+))
                         +code-close+)))
           offset))

#+nil
(p:parse #'code "#+begin_src lisp
(+ 1 1)
#+end_src")

#+nil
(p:parse #'code "#+name: foo
#+begin_src lisp
(+ 1 1)
#+end_src")

#+nil
(p:parse #'code "#+begin_src lisp :results verbatim :exports both
(+ 1 1)

(+ 1 1)
#+end_src")

(defun result (offset)
  "Parser: The RESULTS block that can follow a src block."
  (funcall (p:ap (lambda (name text) (make-result :name name :text text))
                 (*> +results-start+
                     +consume-space+
                     (p:opt +take1-til-break+))
                 (*> +newline+
                     (p:alt #'example
                            (p:ap (lambda (text) (coerce text 'vector))
                                  (p:sep-end +newline+ +take1-til-end+)))))
           offset))

#+nil
(p:parse #'result "#+RESULTS: sum
: 4")

#+nil
(p:parse #'result "#+RESULTS:
: 4")

#+nil
(p:parse #'result "#+RESULTS:
4")

#+nil
(p:parse #'result "#+RESULTS:
#+begin_example
0
1
3
#+end_example")

;; FIXME: 2025-09-01 Support any Lisp symbol for the value, including sexps. At
;; the moment it just assumes any normal text.
(defun variables (offset)
  "Parser: Variables of a src block, like ':results verbatim'."
  (funcall (p:sep-end1 (*> +space+ +consume-space+)
                       (p:pair (*> +colon+
                                   (p:take-while1 (lambda (c) (not (char= c #\space)))))
                               (*> +consume-space+
                                   (p:take-while1 (lambda (c) (not (or (char= c #\space)
                                                                       (char= c #\newline))))))))
           offset))

#+nil
(p:parse #'variables ":results verbatim :exports both")

;; --- Timestamps --- ;;

(defun timestamps (offset)
  "Parser: All things time-related that can appear one the same line."
  (p:fmap (lambda (list) (apply #'append list))
          (funcall (p:sep-end1 +consume-space+
                               (p:alt (p:pmap (lambda (ts) (list :closed ts)) #'closed)
                                      (p:pmap (lambda (ts) (list :scheduled ts)) #'scheduled)
                                      (p:pmap (lambda (ts) (list :deadline ts)) #'deadline)))
                   offset)))

#+nil
(p:parse #'timestamps "CLOSED: [2021-04-28 Wed 15:10] DEADLINE: <2021-04-29 Thu> SCHEDULED: <2021-04-28 Wed>")

(defun scheduled (offset)
  (funcall (*> +scheduled+
               +consume-space+
               (p:between +angle-open+
                          #'timestamp
                          +angle-close+))
           offset))

#+nil
(p:parse #'scheduled "SCHEDULED: <2021-04-30 Fri>")
#+nil
(p:parse #'scheduled "SCHEDULED: <2021-04-30 Fri 13:00 .+1w -1d>")

(defun deadline (offset)
  (funcall (*> +deadline+
               +consume-space+
               (p:between +angle-open+
                          #'timestamp
                          +angle-close+))
           offset))

#+nil
(p:parse #'deadline "DEADLINE: <2021-04-30 Fri>")

(defun closed (offset)
  (funcall (*> +closed+
               +consume-space+
               #'bracketed-timestamp)
           offset))

#+nil
(p:parse #'closed "CLOSED: [2021-04-30 Fri 12:34]")

(defun bracketed-timestamp (offset)
  (funcall (p:between +bracket-open+
                      #'timestamp
                      +bracket-close+)
           offset))

#+nil
(p:parse #'bracketed-timestamp "[2021-04-30 Fri 12:34]")

(defun timestamp (offset)
  (funcall (p:ap (lambda (day dow time repeat delay)
                   (make-timestamp :day day :day-of-week dow :time time :repeat repeat :delay delay))
                 #'d:local-date
                 (*> +consume-space+ (p:take-while #'p:ascii-letter?))
                 (p:opt (*> +consume-space+ #'d:simple-local-time))
                 (p:opt (*> +consume-space+ #'repeat))
                 (p:opt (*> +consume-space+ #'delay)))
           offset))

#+nil
(p:parse #'timestamp "2021-04-28 Wed 13:00 .+1w -1d")

#+nil
(p:parse #'timestamp "2025-08-31 So")

(defun repeat (offset)
  (funcall (p:ap (lambda (mode value interval)
                   (make-repeat :mode mode :value value :interval interval))
                 (p:alt (<$ :from-today (p:string ".+"))
                        (<$ :jump (p:string "++"))
                        (<$ :single +plus+))
                 #'p:unsigned
                 #'interval)
           offset))

#+nil
(p:parse #'repeat ".+1w")

(defun delay (offset)
  (funcall (p:ap (lambda (mode value interval)
                   (make-delay :mode mode :value value :interval interval))
                 (p:alt (<$ :one (p:string "--"))
                        (<$ :all +dash+))
                 #'p:unsigned
                 #'interval)
           offset))

#+nil
(p:parse #'delay "--2d")

(defun interval (offset)
  (funcall (p:alt (<$ :hour  +h+)
                  (<$ :day   +d+)
                  (<$ :week  +w+)
                  (<$ :month +m+)
                  (<$ :year  +y+))
           offset))

;; --- Headings --- ;;

;; TODO: 2025-09-16 Create a vector-based cache for the inner lambdas here.
(defun depth-sensitive-heading (stars)
  "A variant of `heading' which knows how deep it should be parsing."
  (lambda (offset)
    (multiple-value-bind (res next) (heading offset)
      (cond ((p:failure? res) (p:fail next))
            ((not (= stars (heading-depth res))) (p:fail offset))
            (t (values res next))))))

#+nil
(p:parse (depth-sensitive-heading 2) "* Simplest")

(defun heading (offset)
  (funcall (p:ap (lambda (depth todo priority text progress tags tss ts ps log)
                   (make-heading :depth depth
                                 :todo todo
                                 :priority priority
                                 :text text
                                 :progress progress
                                 :tags (or tags (vector))
                                 :closed (getf tss :closed)
                                 :deadline (getf tss :deadline)
                                 :scheduled (getf tss :scheduled)
                                 :timestamp ts
                                 :properties ps
                                 :logbook (or log (vector))))
                 (<* #'bullets-of-heading
                     +space+
                     +consume-space+)
                 (p:opt (<* #'todo (p:sneak #\space)))
                 (*> +consume-space+ (p:opt #'priority))
                 (*> +consume-space+ #'text-of-heading)
                 (*> +consume-space+ (p:opt #'progress))
                 (*> +consume-space+ (p:opt #'tags))
                 (p:opt (*> +consume-between-a-line+
                            #'timestamps))
                 (p:opt (*> +consume-between-a-line+
                            (p:between +angle-open+
                                       #'timestamp
                                       +angle-close+)))
                 (p:opt (*> +consume-between-a-line+
                            #'properties))
                 (p:opt (*> +consume-between-a-line+
                            #'logbook)))
           offset))

#+nil
(p:parse #'heading "* A capital letter")

#+nil
(p:parse #'heading "Not a heading!")

#+nil
(p:parse #'heading "* Simplest")

#+nil
(p:parse #'heading "** Not *a* tag! :)")

#+nil
(p:parse #'heading "*** TODO [#A] Fix the code [1/2] :bug:
<2022-01-01>")

#+nil
(p:parse #'heading "*** TODO [#A] Fix the code [1/2] :bug:
CLOSED: [2021-04-28 Wed 15:10] DEADLINE: <2021-04-29 Thu> SCHEDULED: <2021-04-28 Wed>
<2022-01-01>
:PROPERTIES:
:Yes: Fun
:END:
")

#+nil
(p:parse #'heading "** org-mode :org:
:PROPERTIES:
:GREAT:    PANCAKES
:END:
:LOGBOOK:
CLOCK: [2025-10-08 Mi 04:50]
CLOCK: [2025-10-07 Di 07:08]--[2025-10-07 Di 07:57] =>  0:49
CLOCK: [2025-10-06 Mo 07:01]--[2025-10-06 Mo 07:53] =>  0:52
:END:

Content")

(defun bullets-of-heading (offset)
  "Parser: Just skips over the *."
  (multiple-value-bind (res next) (funcall (p:consume1 (lambda (c) (char= c #\*))) offset)
    (cond ((p:failure? res) (p:fail offset))
          (t (values (- next offset) next)))))

#+nil
(p:parse #'bullets-of-heading "*** Hello")

(defun todo (offset)
  "Parser: A single capital word. The word must be at least two letters long
in order to avoid parsing the normal word A as a TODO-like token."
  (multiple-value-bind (res next)
      (funcall (p:take-while1 (lambda (c) (and (not (char= c #\space))
                                               (char<= #\A c #\Z))))
               offset)
    (cond ((p:failure? res) (p:fail next))
          ((= 1 (length res)) (p:fail offset))
          (t (values (make-todo :text res) next)))))

#+nil
(p:parse #'todo "TODO")

(defun priority (offset)
  (p:fmap (lambda (s) (make-priority :text s))
          (funcall (p:between +bracket-open+
                              (*> +octothorp+
                                  (p:take-while1 (lambda (c) (not (char= c #\])))))
                              +bracket-close+)
                   offset)))

#+nil
(p:parse #'priority "[#A]")

(defun text-of-heading (offset)
  (p:fmap (lambda (list) (coerce list 'vector))
          (funcall (p:sep-end1 (*> +consume-space+
                                   (p:not (p:alt #'progress #'tags)))
                               #'words)
                   offset)))

#+nil
(p:parse #'text-of-heading "Hello there")
#+nil
(p:parse #'text-of-heading "Fix the code [1/2] :bug:")

(defun progress (offset)
  (funcall (p:alt #'percentage #'ratio) offset))

(defun percentage (offset)
  "Parser: A box like [37%]."
  (p:fmap (lambda (n) (make-percentage :number n))
          (funcall (p:between +bracket-open+
                              (<* #'p:unsigned +percent+)
                              +bracket-close+)
                   offset)))

#+nil
(p:parse #'percentage "[37%]")

(defun ratio (offset)
  "Parser: A box like [1/2]."
  (funcall (p:between +bracket-open+
                      (p:ap (lambda (num denom) (make-ratio :numerator num :denominator denom))
                            #'p:unsigned
                            (*> +slash+ #'p:unsigned))
                      +bracket-close+)
           offset))

#+nil
(p:parse #'ratio "[1/2]")

(defun tags (offset)
  "Parser: Tags like :foo:bar:baz:"
  (funcall (p:ap #'list->vector
                 (p:between +colon+
                            (p:sep1 (*> +colon+
                                        (p:not +space+)
                                        (p:not +newline+)
                                        (p:not #'p:eof))
                                    (p:take-while1 (lambda (c) (not (or (char= c #\:)
                                                                        (char= c #\newline))))))
                            +colon+))
           offset))

#+nil
(p:parse #'tags ":foo:")
#+nil
(p:parse #'tags ":foo:bar:baz:")
#+nil
(p:parse #'tags ":)")

(defun properties (offset)
  "Parser: A PROPERTIES drawer."
  (funcall (*> +properties+
               +consume-between-a-line+
               (<* (p:sep-end +consume-between-a-line+
                              (p:pair (p:between +colon+
                                                 (p:take-while1 (lambda (c) (not (char= c #\:))))
                                                 +colon+)
                                      (*> +consume-space+
                                          (p:take-while1 (lambda (c) (not (char= c #\newline)))))))
                   +end+))
           offset))

#+nil
(p:parse #'properties ":PROPERTIES:
:Yes: Fun
:Thing: Value of it
:END:
")

(defun logbook (offset)
  "Parser: A LOGBOOK drawer."
  (funcall (p:ap #'list->vector
                 (*> +logbook+
                     +consume-between-a-line+
                     (<* (p:sep-end +consume-between-a-line+
                                    (*> (p:not +end+) #'logbook-item))
                         +end+)))
           offset))

#+nil
(p:parse #'logbook ":LOGBOOK:
CLOCK: [2025-10-09 Do 06:33]
CLOCK: [2025-10-08 Mi 04:50]--[2025-10-08 Mi 06:20] =>  1:30
CLOCK: [2025-10-07 Di 07:08]--[2025-10-07 Di 07:57] =>  0:49
:END:")

(defun logbook-item (offset)
  "Parser: A single item of a LOGBOOK drawer."
  (funcall (p:ap (lambda (start end total) (make-logbook-item :start start :end end :total total))
                 (*> +clock+ +consume-space+ #'bracketed-timestamp)
                 ;; FIXME: 2025-10-10 Technically it's naughty that these two
                 ;; are `opt' separately.
                 (p:opt (*> +dashes+ #'bracketed-timestamp))
                 (p:opt (*> +consume-space+ +arrow+ +consume-space+ #'total-time)))
           offset))

#+nil
(p:parse #'logbook-item "CLOCK: [2025-10-09 Do 06:33]")

#+nil
(p:parse #'logbook-item "CLOCK: [2025-10-08 Mi 04:50]--[2025-10-08 Mi 06:20] =>  1:30")

(defun total-time (offset)
  "Parser: The total time of a logbook item."
  (funcall (p:ap (lambda (hours mins) (make-total :hours hours :minutes mins))
                 #'p:unsigned
                 (*> +colon+
                     (p:opt +zero+)
                     (valid (lambda (n) (< n 60)) #'p:unsigned)))
           offset))

#+nil
(p:parse #'total-time "0:01")
#+nil
(p:parse #'total-time "0:60")

;; --- Text Markup --- ;;

(defun line (offset)
  "Parser: As many `words' as possible without going over a line break. Does not
consume any newline characters."
  (funcall (p:sep-end1 +consume-space+ #'words) offset))

#+nil
(p:parse #'line "Hello what a *fine* day!")
#+nil
(p:parse #'line "This is not*bold*.")
#+nil
(p:parse #'line "Markup at the *end*.")
#+nil
(p:parse #'line "This should only parse the first line
and not the second.")

(defun markup (offset)
  "Parser: Some non-plain markup object."
  (funcall (p:alt #'bold #'italic #'highlight #'verbatim #'underline #'strike #'image #'link #'punct)
           offset))

(defun words (offset)
  (funcall (p:alt #'markup #'plain) offset))

(defun words-in-cell (offset)
  "Parser: Like `words' but certain markup is banned or altered."
  (funcall (p:alt #'markup #'plain-in-cell) offset))

(defun words-in-link (offset)
  "Parser: Like `words' but certain markup is banned or altered."
  (funcall (p:alt #'markup #'plain-in-link) offset))

;; FIXME: 2025-09-04 There are certainly bugs here involving line breaks. Markup
;; can stretch over line breaks, but only one at a time. A paragraph break (two
;; newlines) stops the markup. Will definitely need unit tests for that.

(defun bold (offset)
  (p:fmap (lambda (s) (make-bold :text s))
          (funcall (p:between +star+
                              (p:take-while1 (lambda (c) (not (char= c #\*))))
                              +star+)
                   offset)))

#+nil
(p:parse #'bold "*hello*")

(defun italic (offset)
  (p:fmap (lambda (s) (make-italic :text s))
          (funcall (p:between +slash+
                              (p:take-while1 (lambda (c) (not (char= c #\/))))
                              +slash+)
                   offset)))

#+nil
(p:parse #'italic "/hello/")

(defun highlight (offset)
  (p:fmap (lambda (s) (make-highlight :text s))
          (funcall (p:between +tilde+
                              (p:take-while1 (lambda (c) (not (char= c #\~))))
                              +tilde+)
                   offset)))

#+nil
(p:parse #'highlight "~hello~")

(defun verbatim (offset)
  (p:fmap (lambda (s) (make-verbatim :text s))
          (funcall (p:between +equal+
                              (p:take-while1 (lambda (c) (not (char= c #\=))))
                              +equal+)
                   offset)))

#+nil
(p:parse #'verbatim "=hello=")

(defun underline (offset)
  (p:fmap (lambda (s) (make-underline :text s))
          (funcall (p:between +under+
                              (p:take-while1 (lambda (c) (not (char= c #\_))))
                              +under+)
                   offset)))

#+nil
(p:parse #'underline "_hello_")

(defun strike (offset)
  (p:fmap (lambda (s) (make-strike :text s))
          (funcall (p:between +plus+
                              (p:take-while1 (lambda (c) (not (char= c #\+))))
                              +plus+)
                   offset)))

#+nil
(p:parse #'strike "+hello+")

(defun plain (offset)
  "Parser: A single, unadorned word."
  (funcall (p:take-while1 (lambda (c) (not (or (char= c #\space)
                                               (char= c #\newline)))))
           offset))

#+nil
(p:parse #'plain "hello there")

(defun plain-in-cell (offset)
  "Parser: Like `plain', but constrained to the conditions of a table cell."
  (funcall (p:take-while1 (lambda (c) (not (or (char= c #\space)
                                               (char= c #\|)))))
           offset))

(defun plain-in-link (offset)
  "Parser: Like `plain', but constrained to the conditions of a link description."
  (funcall (p:take-while1 (lambda (c) (not (or (char= c #\space)
                                               (char= c #\])))))
           offset))

(defun punct (offset)
  "Parser: A single character of punctuation."
  (p:fmap (lambda (c) (make-punct :char c))
          (funcall (p:any-if (lambda (c) (or (char= c #\.)
                                             (char= c #\,)
                                             (char= c #\!)
                                             (char= c #\?)
                                             (char= c #\()
                                             (char= c #\))
                                             (char= c #\:)
                                             (char= c #\;)
                                             (char= c #\')
                                             (char= c #\"))))
                   offset)))

#+nil
(p:parse #'punct ",hello")

(defun link (offset)
  (funcall (p:ap (lambda (attrs url text)
                   (make-link :attrs attrs
                              :url (make-url :text url)
                              :text text))
                 (p:opt #'attrs)
                 (*> +bracket-open+ +between-brackets+)
                 (<* (p:opt (*> +bracket-open+
                                (<* #'link-description +bracket-close+)))
                     +bracket-close+))
           offset))

#+nil
(p:parse #'link "[[https://www.fosskers.ca][*Amazing* Site]]")
#+nil
(p:parse #'link "[[https://www.fosskers.ca]]")
#+nil
(p:parse #'link "#+ATTR_HTML: :title foo
[[https://www.fosskers.ca]]")

(defun link-description (offset)
  "Like `line', but limited to a link description."
  (funcall (p:ap #'list->vector
                 (p:sep-end +consume-space+ (*> (p:not +bracket-close+)
                                                #'words-in-link)))
           offset))

(defun image (offset)
  (funcall (p:ap (lambda (caption attrs name url)
                   (make-image :caption caption
                               :attrs attrs
                               :name name
                               :url url))
                 (p:opt (<* #'caption +newline+))
                 (p:opt #'attrs)
                 (p:opt (<* #'name +newline+))
                 (p:between +bracket-open+
                            (p:between +bracket-open+
                                       #'url-of-image
                                       +bracket-close+)
                            +bracket-close+))
           offset))

#+nil
(p:parse #'image "[[/path/to/img.jpeg]]")
#+nil
(p:parse #'image "#+CAPTION: Hello
#+ATTR_HTML: :title foo
#+NAME: foo
[[/path/to/img.jpeg]]")

;; https://developer.mozilla.org/en-US/docs/Web/Media/Guides/Formats/Image_types
(defun url-of-image (offset)
  (multiple-value-bind (res next)
      (funcall (p:take-while1 (lambda (c) (not (char= c #\])))) offset)
    (if (and (p:ok? res)
             (or (string-ends-with? res ".png")
                 (string-ends-with? res ".jpg")
                 (string-ends-with? res ".jpeg")
                 (string-ends-with? res ".svg")
                 (string-ends-with? res ".webp")
                 (string-ends-with? res ".gif")))
        (values (make-url :text res) next)
        (p:fail offset))))

(defun name (offset)
  "Parser: A #+NAME line."
  (funcall (*> +name+ +take1-til-end+) offset))

(defun attrs (offset)
  (funcall (p:ap (lambda (alist)
                   (make-attrs :html  (cdr (assoc :html  alist))
                               :org   (cdr (assoc :org   alist))
                               :latex (cdr (assoc :latex alist))))
                 (p:sep-end1 +newline+ #'attr-pair))
           offset))

#+nil
(p:parse #'attrs "#+ATTR_HTML: :title foo
#+ATTR_ORG: :center true")

(defun attr-pair (offset)
  (funcall (p:ap #'cons
                 (*> (p:alt (p:string "#+ATTR_")
                            (p:string "#+attr_"))
                     (p:alt (<$ :html  (p:alt (p:string "HTML") (p:string "html")))
                            (<$ :org   (p:alt (p:string "ORG") (p:string "org")))
                            (<$ :latex (p:alt (p:string "LATEX") (p:string "latex")))))
                 (*> +colon+ +space+ +consume-space+ +take1-til-end+))
           offset))

#+nil
(p:parse #'attr-pair "#+ATTR_HTML: :title foo")

(defun drawer (offset)
  "Parser: Any kind of drawer."
  (funcall (p:ap (lambda (label content)
                   (make-drawer :label label :content (list->vector content)))
                 (<* #'drawer-labl +consume-junk+)
                 (<* (p:sep-end +consume-junk+
                                (*> (p:not +end+) #'block-in-drawer))
                     +consume-junk+
                     +end+))
           offset))

#+nil
(p:parse #'drawer ":MYDRAWER:
:END:")

#+nil
(p:parse #'drawer ":MYDRAWER:
Great content.
This line too.
:END:")

#+nil
(p:parse #'drawer ":MYDRAWER:
Great content.

[[https://www.fosskers.ca]]

More!
:END:")

#+nil
(p:parse #'file "Still outside the drawer
:DRAWERNAME:
This is inside the drawer.
:END:
After the drawer.")

;; NOTE: 2025-10-08 Given a stunted name in order to avoid clashing with the
;; `drawer' struct accessor of the true name.
(defun drawer-labl (offset)
  (funcall (p:between +colon+
                      (p:take-while1 (lambda (c) (not (char= c #\colon))))
                      +colon+)
           offset))

#+nil
(p:parse #'drawer-labl ":MYDRAWER:
Content
:END:")

(defun footnote (offset)
  "Parser: A full, standalone fullnote."
  (funcall (p:ap (lambda (label content)
                   (make-footnote :label label
                                  :content (coerce (apply #'append content) 'vector)))
                 (p:between +footnote-start+
                            (p:take-while1 (lambda (c) (and (not (char= c #\space))
                                                            (not (char= c #\newline))
                                                            (not (char= c #\])))))
                            +bracket-close+)
                 (*> +consume-space+
                     (p:sep-end1 +newline+
                                 (*> (p:not #'heading)
                                     (p:not #'footnote)
                                     #'line))))

           offset))

#+nil
(p:parse #'footnote "[fn:50] See also Jung's /Symbole der Wandlung/.")

#+nil
(p:parse #'footnote "[fn:50] See also Jung's /Symbole der Wandlung/,
a book I am referencing but have not read myself.

This last line should not be part of the footnote.")
