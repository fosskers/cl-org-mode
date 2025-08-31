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
(defparameter +percent+ (p:char #\%))
(defparameter +octothorp+ (p:char #\#))
(defparameter +bracket-open+  (p:char #\[))
(defparameter +bracket-close+ (p:char #\]))
(defparameter +consume-space+ (p:consume (lambda (c) (char= c #\space))))
(defparameter +between-brackets+ (p:between +bracket-open+
                                            (p:take-while1 (lambda (c) (not (char= c #\]))))
                                            +bracket-close+))

;; --- Timestamps --- ;;

(defun timestamp (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (day dow time repeat delay) list
              (make-timestamp :day day :day-of-week dow :time time :repeat repeat :delay delay)))
          (funcall (<*> #'d:local-date
                        (*> +consume-space+
                            (p:take-while (lambda (c) (not (or (char= c #\space)
                                                               (char= c #\newline))))))
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
            (destructuring-bind (todo priority text progress tags) list
              (make-heading :todo todo
                            :priority priority
                            :text text
                            :progress progress
                            :tags (or tags (vector))
                            :properties (make-hash-table))))
          (funcall (<*> (*> #'bullets-of-heading
                            +consume-space+
                            (p:opt (<* #'todo (p:sneak #\space))))
                        (*> +consume-space+ (p:opt #'priority))
                        (*> +consume-space+ #'text-of-heading)
                        (*> +consume-space+ (p:opt #'progress))
                        (*> +consume-space+ (p:opt #'tags)))
                   offset)))

#+nil
(p:parse #'heading "* Simplest")

#+nil
(p:parse #'heading "*** TODO [#A] Fix the code [1/2] :bug:")

(defun bullets-of-heading (offset)
  "Parser: Just skips over the *."
  (funcall (p:consume (lambda (c) (char= c #\*))) offset))

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
                       (p:sep-end1 +colon+ (p:take-while1 (lambda (c) (not (char= c #\:))))))
                   offset)))

#+nil
(p:parse #'tags ":foo:bar:baz:")

;; --- Text Markup --- ;;

(defun line (offset)
  (funcall (p:sep-end1 +consume-space+ #'words) offset))

#+nil
(p:parse #'line "Hello what a *fine* day!")
#+nil
(p:parse #'line "This is not*bold*.")
#+nil
(p:parse #'line "Markup at the *end*.")

(defun words (offset)
  (funcall (p:alt #'bold #'italic #'highlight #'verbatim #'underline #'strike #'image #'link #'punct #'plain)
           offset))

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
