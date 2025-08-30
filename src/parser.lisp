(in-package :org-mode)

;; --- Static Parsers --- ;;

(defparameter +star+  (p:char #\*))
(defparameter +slash+ (p:char #\/))
(defparameter +tilde+ (p:char #\~))
(defparameter +equal+ (p:char #\=))
(defparameter +under+ (p:char #\_))
(defparameter +plus+  (p:char #\+))
(defparameter +bracket-open+  (p:char #\[))
(defparameter +bracket-close+ (p:char #\]))
(defparameter +between-brackets+ (p:between +bracket-open+
                                            (p:take-while1 (lambda (c) (not (char= c #\]))))
                                            +bracket-close+))

;; --- Timestamps --- ;;

;; --- Text Markup --- ;;

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

;; TODO: 2025-08-30 Image and link.

(defun link (offset)
  (p:fmap (lambda (list) (make-link :url (make-url :text (car list))
                                    :text (cadr list)))
          (funcall (p:between +bracket-open+
                              (p:<*> +between-brackets+
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
                                 (lambda (off)
                                   (multiple-value-bind (res next)
                                       (funcall (p:take-while1 (lambda (c) (not (char= c #\])))) off)
                                     (if (and (p:ok? res)
                                              (or (string-ends-with? res ".jpg")
                                                  (string-ends-with? res ".jpeg")
                                                  (string-ends-with? res ".png")))
                                         (values (make-image :url (make-url :text res)) next)
                                         (p:fail off))))
                                 +bracket-close+)
                      +bracket-close+)
           offset))

#+nil
(p:parse #'image "[[/path/to/img.jpeg]]")
