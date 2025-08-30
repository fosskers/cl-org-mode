(defsystem "org-mode"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/cl-org-mode"
  :depends-on (:parcom)
  :serial t
  :components ((:module "src" :components ((:file "package"))))
  :description "An Emacs org-mode parser.")
