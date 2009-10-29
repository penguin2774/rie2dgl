;;;
;;; An ASDF class for all the translated shared/ libraries.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defsystem rie2dgl
  :depends-on (cl-opengl cl-glut cl-glu lispbuilder-sdl lispbuilder-sdl-image)
  :description "A 2d opengl graphics library."
  :version "0.0"
  :author "Nathanael Cunningham <penguin2774@gmail.com>"

  :components
  ((:file "package")
   (:file "utility")
   (:module "backend" :components ((:file "backend")))
   (:file "flags")
   (:file "math3d")
   (:file "texture-db")
   (:file "image" )
   (:file "animation")
   (:file "sprite"))
;   (:module "tests" :components ((:file "sprite"))))
  :serial t)