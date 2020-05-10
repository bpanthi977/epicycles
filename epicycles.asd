;;;; epicycles.asd

(asdf:defsystem #:epicycles
  :description "Epicycles"
  :author "Bibek Panthi <bpanthi977@gmail.com>" 
  :license  "GNU GPLv2"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl #:opticl)
  :components ((:file "package")
               (:file "epicycles")))
