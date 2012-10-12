(cl:defpackage :silcro-system
  (:use :cl :asdf))
(cl:in-package :silcro-system)

(defsystem :silcro
  :version "0.1"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :description "A web framework"
  :components ((:file "package")
               (:file "silcro" :depends-on ("package"))
               (:file "cookie" :depends-on ("package"))
               (:file "mime-types" :depends-on ("package")))
  :depends-on (:stampede
               :ironclad
               :cl-fad
               :anaphora
               :alexandria))
