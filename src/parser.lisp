(in-package :org-mode)

;; --- Static Parsers --- ;;

(defparameter +h+     (p:char #\h))
(defparameter +d+     (p:char #\d))
(defparameter +w+     (p:char #\w))
(defparameter +m+     (p:char #\m))
(defparameter +y+     (p:char #\y))
(defparameter +colon+ (p:char #\:))
(defparameter +dash+  (p:char #\-))
(defparameter +equal+ (p:char #\=))
(defparameter +plus+  (p:char #\+))
(defparameter +slash+ (p:char #\/))
(defparameter +star+  (p:char #\*))
(defparameter +tilde+ (p:char #\~))
(defparameter +under+ (p:char #\_))
(defparameter +zero+  (p:char #\0))
(defparameter +space+ (p:char #\space))
(defparameter +newline+ (p:char #\newline))
(defparameter +scheduled+ (p:string "SCHEDULED:"))
(defparameter +deadline+  (p:string "DEADLINE:"))
(defparameter +closed+    (p:string "CLOSED:"))
(defparameter +properties+ (p:string ":PROPERTIES:"))
(defparameter +end+        (p:string ":END:"))
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
(defparameter +consume-space+ (p:consume (lambda (c) (char= c #\space))))
(defparameter +consume-between-a-line+ (*> +consume-space+ +newline+ +consume-space+))
(defparameter +consume-junk+ (p:consume #'p:space?))
(defparameter +between-brackets+ (p:between +bracket-open+
                                            (p:take-while1 (lambda (c) (not (char= c #\]))))
                                            +bracket-close+))

;; --- Documents and Sections --- ;;

(defun document (offset)
  "Paser: Many blocks and any subsections of deeper depth."
  (p:fmap (lambda (list) (make-document :blocks (coerce (car list) 'vector)
                                        :sections (coerce (cadr list) 'vector)))
          (funcall (<*> (p:sep-end +newline+ #'block)
                        (p:sep-end +newline+ #'section))
                   offset)))

(defun section (offset)
  "Parser: A heading and any subsequent content."
  (p:fmap (lambda (list) (make-section :heading (car list) :document (cadr list)))
          (funcall (<*> #'heading
                        (*> +consume-junk+ #'document))
                   offset)))

#+nil
(p:parse #'section "* Grand Plans

Eloquent thoughts.")

;; --- Blocks --- ;;

;; TODO: 2025-09-01 Start here. Parse a block in general. Handle lists and
;; tables later. Then do the recursive `section' and `document' parsing, which
;; you can take care of with a custom lambda cache for both that stores lambdas
;; in vectors, whose indices correspond to the depth of the subsection we're
;; trying to parse. The first time we reach a new depth, allocate a new lambda
;; and add it to the vector. Subsequent attempts will reuse the allocated one.
(defun block (offset)
  (funcall (p:alt #'quote #'example #'code #'paragraph) offset))

(defun paragraph (offset)
  "A single body of text which runs until a double-newline or a header is
encountered."
  (p:fmap (lambda (lists) (make-paragraph :words (coerce (apply #'append lists) 'vector)))
          (funcall (p:sep-end1 +newline+
                               (*> (p:not #'heading) #'line))
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
  "Parser: An example block."
  (p:fmap (lambda (list)
            (destructuring-bind (lang vars code) list
              (make-code :lang lang :vars vars :text (coerce code 'vector))))
          (funcall (p:between (*> +code-open+ +consume-space+)
                              (<*> (p:take-while (lambda (c) (not (or (char= c #\space)
                                                                      (char= c #\newline)))))
                                   (p:opt (*> +consume-space+ #'variables))
                                   (*> +newline+
                                       (p:sep-end +newline+
                                                  (*> (p:not +code-close+)
                                                      (p:take-while (lambda (c) (not (char= c #\newline))))))))
                              +code-close+)
                   offset)))

#+nil
(p:parse #'code "#+begin_src lisp :results verbatim :exports both
(+ 1 1)

(+ 1 1)
#+end_src")

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
               (p:between +bracket-open+
                          #'timestamp
                          +bracket-close+))
           offset))

#+nil
(p:parse #'closed "CLOSED: [2021-04-30 Fri 12:34]")

(defun timestamp (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (day dow time repeat delay) list
              (make-timestamp :day day :day-of-week dow :time time :repeat repeat :delay delay)))
          (funcall (<*> #'d:local-date
                        (*> +consume-space+ (p:take-while #'p:ascii-letter?))
                        (p:opt (*> +consume-space+ #'d:simple-local-time))
                        (p:opt (*> +consume-space+ #'repeat))
                        (p:opt (*> +consume-space+ #'delay)))
                   offset)))

#+nil
(p:parse #'timestamp "2021-04-28 Wed 13:00 .+1w -1d")

#+nil
(p:parse #'timestamp "2025-08-31 So")

(defun repeat (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (mode value interval) list
              (make-repeat :mode mode :value value :interval interval)))
          (funcall (<*> (p:alt (<$ :from-today (p:string ".+"))
                               (<$ :jump (p:string "++"))
                               (<$ :single +plus+))
                        #'p:unsigned
                        #'interval)
                   offset)))

#+nil
(p:parse #'repeat ".+1w")

(defun delay (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (mode value interval) list
              (make-delay :mode mode :value value :interval interval)))
          (funcall (<*> (p:alt (<$ :one (p:string "--"))
                               (<$ :all +dash+))
                        #'p:unsigned
                        #'interval)
                   offset)))

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

(defun heading (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (todo priority text progress tags tss ts ps) list
              (make-heading :todo todo
                            :priority priority
                            :text text
                            :progress progress
                            :tags (or tags (vector))
                            :closed (getf tss :closed)
                            :deadline (getf tss :deadline)
                            :scheduled (getf tss :scheduled)
                            :timestamp ts
                            :properties ps)))
          (funcall (<*> (*> #'bullets-of-heading
                            +consume-space+
                            (p:opt (<* #'todo (p:sneak #\space))))
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
                                   #'properties)))
                   offset)))

#+nil
(p:parse #'heading "Not a heading!")

#+nil
(p:parse #'heading "* Simplest")

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

(defun bullets-of-heading (offset)
  "Parser: Just skips over the *."
  (funcall (*> (p:sneak #\*) (p:consume (lambda (c) (char= c #\*)))) offset))

#+nil
(p:parse #'bullets-of-heading "*** Hello")

(defun todo (offset)
  "Parser: A single capital word."
  (p:fmap (lambda (s) (make-todo :text s))
          (funcall (p:take-while1 (lambda (c) (and (not (char= c #\space))
                                                   (char<= #\A c #\Z))))
                   offset)))

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
  (p:fmap (lambda (list) (make-ratio :numerator (car list)
                                     :denominator (cadr list)))
          (funcall (p:between +bracket-open+
                              (<*> #'p:unsigned
                                   (*> +slash+ #'p:unsigned))
                              +bracket-close+)
                   offset)))

#+nil
(p:parse #'ratio "[1/2]")

(defun tags (offset)
  "Parser: Tags like :foo:bar:baz:"
  ;; FIXME: 2025-08-31 Probably needs to be a (vector string).
  (p:fmap (lambda (list) (coerce list 'vector))
          (funcall (*> +colon+
                       (p:sep-end1 +colon+ (p:take-while1 (lambda (c) (not (or (char= c #\:)
                                                                               (char= c #\newline)))))))
                   offset)))

#+nil
(p:parse #'tags ":foo:bar:baz:")

(defun properties (offset)
  "Parser: A PROPERTIES drawer."
  (funcall (*> +properties+
               +consume-between-a-line+
               (<* (p:sep-end1 +consume-between-a-line+
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

;; --- Text Markup --- ;;

(defun line (offset)
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

(defun words (offset)
  (funcall (p:alt #'bold #'italic #'highlight #'verbatim #'underline #'strike #'image #'link #'punct #'plain)
           offset))

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
  (p:fmap (lambda (s) (make-plain :text s))
          (funcall (p:take-while1 (lambda (c) (not (or (char= c #\space)
                                                       (char= c #\newline)))))
                   offset)))

#+nil
(p:parse #'plain "hello there")

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
  (p:fmap (lambda (list) (make-link :url (make-url :text (car list))
                                    :text (cadr list)))
          (funcall (p:between +bracket-open+
                              (<*> +between-brackets+
                                   (p:opt +between-brackets+))
                              +bracket-close+)
                   offset)))

#+nil
(p:parse #'link "[[https://www.fosskers.ca][Site]]")
#+nil
(p:parse #'link "[[https://www.fosskers.ca]]")

(defun image (offset)
  (funcall (p:between +bracket-open+
                      (p:between +bracket-open+
                                 #'url-of-image
                                 +bracket-close+)
                      +bracket-close+)
           offset))

(defun url-of-image (offset)
  (multiple-value-bind (res next)
      (funcall (p:take-while1 (lambda (c) (not (char= c #\])))) offset)
    (if (and (p:ok? res)
             (or (string-ends-with? res ".jpg")
                 (string-ends-with? res ".jpeg")
                 (string-ends-with? res ".png")))
        (values (make-image :url (make-url :text res)) next)
        (p:fail offset))))

#+nil
(p:parse #'image "[[/path/to/img.jpeg]]")
