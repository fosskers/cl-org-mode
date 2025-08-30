(defpackage org-mode/tests
  (:use :cl :parachute)
  (:local-nicknames (#:o #:org-mode)
                    (#:p #:parcom)))

(in-package :org-mode/tests)

;; (define-test reduction)
;; (is equal '() (t:transduce #'t:pass #'t:cons '())))
