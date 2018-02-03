(asdf:defsystem #:sdf
    :description "Common Lisp port of msdfgen by Viktor Chlumsky"
    :author "Kevin Ednalino <kcednalino@gmail.com>"
    :license "Apache License 2.0"
    :depends-on (#:iterate
                 #:cl-freetype2
                 #:3d-vectors)
    :serial t
    :components ((:file "src/package")
                 (:file "src/sdf")
                 (:file "src/font")
                 (:file "src/shape")
                 (:file "src/contour")
                 (:file "src/signed-distance")
                 (:file "src/msdf")
                 (:file "src/winding-spanner")
                 (:file "src/linear-segment")
                 (:file "src/quadratic-segment")
                 (:file "src/cubic-segment")
		 (:file "src/vector")
		 (:file "src/logic")
		 (:file "src/solvers")
		 (:file "src/bitmap")))
