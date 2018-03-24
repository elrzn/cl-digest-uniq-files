;;;; cl-digest-uniq-files.asd

(asdf:defsystem #:cl-digest-uniq-files
  :description "Describe cl-digest-uniq-files here"
  :author "Eric Lorenzana"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "cl-digest-uniq-files")))
