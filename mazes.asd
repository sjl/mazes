(asdf:defsystem #:mazes
  :name "mazes"
  :description "Working through the Mazes for Programmers book."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:defstar
               #:sketch
               #:sb-cga
               #:snakes
               #:cl-strings
               #:cl-arrows)

  :serial t
  :components
  ((:file "quickutils") ; quickutils package ordering crap
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "fps")
                 (:file "grid")
                 (:file "generation")
                 (:file "demo")))))
