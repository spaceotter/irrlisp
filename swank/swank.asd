(defsystem :swank
  :description "A hack to include swank completely in the compiled C program."
  :version "2.26.1"
  :pathname "../slime"
  :depends-on (sockets)
  :serial t
  :components ((:file "swank-loader")
               (:file "packages")
               (:module "swankdir"
                :serial t
                :pathname "swank"
                :components ((:file "backend")
                             (:file "ecl")
                             (:file "gray")
                             (:file "match")
                             (:file "rpc")))
               (:file "swank")
               (:module "contrib"
                :serial t
                :components ((:file "swank-util")
                             (:file "swank-repl")
                             (:file "swank-c-p-c")
                             (:file "swank-arglists")
                             (:file "swank-fuzzy")
                             (:file "swank-fancy-inspector")
                             (:file "swank-presentations")
                             (:file "swank-presentation-streams")
                             (:file "swank-asdf")
                             (:file "swank-package-fu")
                             (:file "swank-hyperdoc")
                             (:file "swank-mrepl")
                             (:file "swank-trace-dialog")
                             (:file "swank-macrostep")
                             (:file "swank-indentation")
                             ))))
