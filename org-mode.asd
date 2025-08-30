(defsystem "org-mode"
    :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/cl-org-mode"
  :depends-on (:parcom)
  :serial t
  :components ((:module "src" :components ((:file "package")
                                           (:file "types")
                                           (:file "parser"))))
  :description "An Emacs org-mode parser."
  :in-order-to ((test-op (test-op :org-mode/tests))))

(defsystem "org-mode/tests"
    :depends-on (:org-mode :parachute :parcom)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :org-mode/tests)))
