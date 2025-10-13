(in-package :org-mode)

(defstruct file
  "The contents of a complete `.org' with metadata."
  (metadata nil :type metadata)
  (document nil :type document))

(defstruct metadata
  "All extra content at the top of a file."
  (comments   nil :type (vector comment))
  (properties nil :type list)
  (metadata   nil :type hash-table))

(defstruct document
  "A recursive org document. These are zero or more blocks of markup, followed
by zero or more subsections."
  (blocks   nil :type (vector block))
  (sections nil :type (vector section)))

(defstruct comment
  "As-is lines that nonetheless must be parsed."
  (text nil :type (vector string)))

(defstruct attrs
  "ATTR pragmas that can attach to various structures."
  (html  nil :type (or null string))
  (org   nil :type (or null string))
  (latex nil :type (or null string)))

(deftype block ()
  "Look Haskell, Lisp can do ADTs too!"
  '(or comment quote example center code result listing table drawer footnote paragraph))

(defstruct quote
  "A quote block."
  (text nil :type (vector paragraph)))

(defstruct example
  "An example block."
  (text nil :type (vector string)))

(defstruct center
  "A center block, which centers text when exported."
  (text nil :type (vector string)))

(defstruct code
  "A src code block."
  (name nil :type (or null string))
  (lang nil :type string)
  ;; An association list.
  (vars nil :type list)
  (text nil :type (vector string)))

(defstruct result
  "The result of executing a `src' block."
  (name nil :type (or null string))
  (text nil :type (or (vector string) example)))

(defstruct listing
  "Various kinds of bullet lists."
  (type  nil :type (member
                    :bulleted :plussed :numbered :numpar :letter-small
                    :letter-big :letter-par-small :letter-par-big))
  (items nil :type (vector list-item)))

(defstruct table
  "A group of cells forming a chart."
  (caption nil :type (or null caption))
  (attrs   nil :type (or null attrs))
  (plot    nil :type (or null string))
  (name    nil :type (or null string))
  (rows    nil :type (vector row))
  ;; A TBLFM (table formula).
  (form    nil :type (or null string)))

(defstruct clock-table
  "An auto-generated clocktable object."
  (settings nil :type string)
  (table    nil :type table))

(defstruct caption
  "Additional commentary that can appear before a number of structures, such
as tables and image links."
  (short nil :type (or null string))
  (long  nil :type string))

(defstruct paragraph
  "An ordinary body of text."
  (words nil :type (vector words)))

(defstruct drawer
  "A block of some hidable contents."
  (label   nil :type string)
  (content nil :type (vector (and block (not drawer)))))

(defstruct footnote
  "Some reference to external material.

Example:

[fn:50] See also Jung's /Symbole der Wandlung/."
  (label   nil :type string)
  (content nil :type (vector words)))

(deftype footnote-ref ()
  '(or footnote-simple-ref footnote-inline-ref))

(defstruct footnote-simple-ref
  "A reference to a full footnote, typically defined somewhere else."
  (label nil :type string))

(defstruct footnote-inline-ref
  (label   nil :type (or null string))
  (content nil :type (vector words)))

(defstruct horizontal-line
  "A simple horizontal line to break up some content.")

(defstruct section
  "A section or subsection, marked by a heading line and followed recursively by other documents."
  (heading  nil :type heading)
  (document nil :type document))

;; --- Headings --- ;;

(defstruct heading
  "The top line of a `section' with associated metadata."
  (depth      nil :type fixnum)
  (todo       nil :type (or null todo))
  (priority   nil :type (or null priority))
  (text       nil :type (vector words))
  (progress   nil :type (or null progress))
  (tags       nil :type (vector string))
  (closed     nil :type (or null timestamp))
  (deadline   nil :type (or null timestamp))
  (scheduled  nil :type (or null timestamp))
  ;; A timestamp for general events that are neither a DEADLINE nor SCHEDULED.
  (timestamp  nil :type (or null timestamp))
  ;; An assoc-list.
  (properties nil :type list)
  (logbook    nil :type (vector logbook-item)))

(defstruct logbook-item
  "An entry in a LOGBOOK drawer."
  (start nil :type timestamp)
  (end   nil :type (or null timestamp))
  (total nil :type (or null total)))

(defstruct total
  "The total time of a logbook item."
  (hours   nil :type fixnum)
  (minutes nil :type fixnum))

(defstruct todo
  "A marker like TODO or DONE. These are customizable by the user, so we can't
prescribe what they should be. We at least expect them to be one word, and all
caps."
  (text nil :type string))

(defstruct priority
  "A priority value, usually associated with a TODO marking, as in:

*** TODO [#A] Eat lunch
*** TODO [#B] Cure cancer"
  (text nil :type string))

(deftype progress ()
  "Completion progress of a checklist within a section."
  '(or percentage ratio))

(defstruct percentage
  "A box like [37%]."
  (number nil :type fixnum))

(defstruct ratio
  "A box like [1/2]."
  (numerator   nil :type fixnum)
  (denominator nil :type fixnum))

(defstruct list-item
  "A line in a listing. Can contain sublists."
  (status   nil :type (or null (member :open :progress :done)))
  (words    nil :type (vector words))
  (progress nil :type (or null progress))
  (sublist  nil :type (or null listing)))

(deftype row ()
  '(or (member :hline) (vector column)))

(deftype column ()
  '(vector words))

;; --- Timestamps --- ;;

(defstruct timestamp
  "An org-mode timestamp. Must contain at least a year-month-day and the day of the week."
  (day         nil :type d:local-date)
  (day-of-week nil :type string)
  (time        nil :type (or null d:local-time))
  (repeat      nil :type (or null repeat))
  (delay       nil :type (or null delay)))

(defstruct repeat
  "Repetition of a timestamp."
  (mode     nil :type repeat-mode)
  (value    nil :type fixnum)
  (interval nil :type interval))

(deftype repeat-mode ()
  "The nature of the repetition."
  '(member :single :jump :from-today))

(deftype interval ()
  "The timestamp repetition unit."
  '(member :hour :day :week :month :year))

(defstruct delay
  "Delay the appearance of a timestamp in the agenda."
  (mode     nil :type delay-mode)
  (value    nil :type fixnum)
  (interval nil :type interval))

(deftype delay-mode ()
  "When a repeater is also present, should the delay be for the first value or all
of them?"
  '(member :one :all))

;; --- Text Markup --- ;;

(deftype words ()
  "The fundamental unit of Org text content. Plain units are split word-by-word."
  '(or bold italic highlight underline verbatim strike link image punct plain))

(defstruct link
  (attrs nil :type (or null attrs))
  (url   nil :type url)
  (text  nil :type (or null (vector words))))

(defstruct image
  (caption nil :type (or null caption))
  (attrs   nil :type (or null attrs))
  (name    nil :type (or null string))
  (url     nil :type url))

(defstruct url
  "The url portion of some link-like type."
  (text nil :type string))

(defstruct bold
  "Text surrounded by *."
  (text nil :type string))

(defstruct italic
  "Text surrounded by /."
  (text nil :type string))

(defstruct highlight
  "TODO: 2025-08-30 I forget!"
  (text nil :type string))

(defstruct underline
  "Text surrounded by _."
  (text nil :type string))

(defstruct verbatim
  "TODO: 2025-08-30 I forget!"
  (text nil :type string))

(defstruct strike
  "Text surrounded by +."
  (text nil :type string))

(defstruct punct
  (char nil :type character))

(deftype plain ()
  "A single word."
  'string)

;; --- Generics --- ;;

(defgeneric text (obj)
  (:documentation "The plain text contents of some otherwise wrapped thing."))

(defmethod text ((link link)) (link-text link))
(defmethod text ((url url)) (url-text url))
(defmethod text ((bold bold)) (bold-text bold))
(defmethod text ((italic italic)) (italic-text italic))
(defmethod text ((highlight highlight)) (highlight-text highlight))
(defmethod text ((underline underline)) (underline-text underline))
(defmethod text ((verbatim verbatim)) (verbatim-text verbatim))
(defmethod text ((strike strike)) (strike-text strike))
(defmethod text ((todo todo)) (todo-text todo))
(defmethod text ((priority priority)) (priority-text priority))
(defmethod text ((comment comment)) (comment-text comment))
(defmethod text ((code code)) (code-text code))
(defmethod text ((result result)) (result-text result))
(defmethod text ((string string)) string)
;; FIXME: 2025-09-24 This returns a character; probably naughty.
(defmethod text ((punct punct)) (punct-char punct))

;; --- Utilities --- ;;

(defun draw-doc-tree (doc)
  "Draw the basic structure of a parsed `document' tree."
  (labels ((recur (d depth)
             (loop :for sec :across (document-sections d)
                   :do (progn (format t "~,,v,'-@a " depth "")
                              (draw-words (heading-text (section-heading sec)))
                              (format t "~%")
                              (recur (section-document sec) (1+ depth))))))
    (recur doc 1)
    (format t "~%")))

#+nil
(format nil "~,,v,'-@a" 3 #\a)

(defun draw-words (words &optional (stream t))
  "Rudimentary rendering of the text of some `words'."
  (loop :for word :across words
        :do (format stream "~a " (text word))))
