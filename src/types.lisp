(in-package :org-mode)

(defstruct file
  "The contents of a complete `.org' with metadata."
  (metadata nil :type hash-table)
  (document nil :type document))

(defstruct document
  "A recursive org document. These are zero or more blocks of markup, followed
by zero or more subsections."
  (blocks   nil :type (vector block))
  (sections nil :type (vector section)))

(deftype block ()
  "Look Haskell, Lisp can do ADTs too!"
  '(or quote example code listing table paragraph))

(defstruct quote
  "A quote block."
  (text nil :type string))

(defstruct example
  "An example block."
  (text nil :type string))

(defstruct code
  "A code block."
  (lang nil :type language)
  (text nil :type string))

(defstruct listing
  "Various kinds of bullet lists."
  (type  nil :type (member :bulleted :plussed :numbered))
  (items nil :type (vector item)))

(defstruct table
  "A group of cells forming a chart."
  (rows nil :type (vector row)))

(defstruct paragraph
  "An ordinary body of text."
  (words nil :type (vector words)))

(defstruct section
  "A section or subsection, marked by a heading line and followed recursively by other documents."
  (heading  nil :type heading)
  (document nil :type document))

(defstruct heading
  "The top line of a `section' with associated metadata."
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
  (properties nil :type hash-table))

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

(defstruct item
  "A line in a listing. Can contain sublists."
  (words   nil :type (vector words))
  (sublist nil :type (or null listing)))

(deftype row ()
  '(or (member :break) (vector column)))

(deftype column ()
  '(or (member :empty) (vector words)))

;; --- Timestamps --- ;;

(defstruct timestamp
  "An org-mode timestamp. Must contain at least a year-month-day and the day of the week."
  (day         nil :type day)
  (day-of-week nil :type day-of-week)
  (time        nil :type (or null time))
  (repeat      nil :type (or null repeater))
  (delay       nil :type (or null delay)))

(defstruct day
  (year  nil :type fixnum)
  (month nil :type fixnum)
  (day   nil :type fixnum))

(deftype day-of-week ()
  '(member :monday :tuesday :wednesday :thursday :friday :saturday :sunday))

;; TODO: 2025-08-30 Implement! Probably these can come near-to-last.
(deftype time ())
(deftype repeater ())
(deftype delay ())

;; --- Text Markup --- ;;

(deftype words ()
  "The fundamental unit of Org text content. Plain units are split word-by-word."
  '(or bold italic highlight underline verbatim strike link image punct plain))

(defstruct link
  (url  nil :type url)
  (text nil :type (or null string)))

(defstruct image
  (url nil :type url))

(defstruct url
  "The url portion of some link-like type."
  (text nil :type string))

(defstruct language
  "The programming language that some source code block was written in."
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

;; FIXME: 2025-08-31 Maybe get rid of this wrapping to avoid allocation?
(defstruct plain
  "A single word."
  (text nil :type string))
