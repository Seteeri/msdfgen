(in-package :sdf)

(defparameter *debug-outline-decompose* nil)
(defparameter *debug-bounds* nil)

(defparameter *debug-conic-signed-distance* nil)
(defparameter *debug-conic-signed-distance-solve* nil)
(defparameter *debug-linear-signed-distance* nil)

(defparameter *debug-edge-coloring-simple* nil)
(defparameter *debug-split-in-thirds* nil)
(defparameter *debug-advance-to* nil)
(defparameter *debug-collect-crossings* nil)
(defparameter *debug-generate-msdf* nil)
(defparameter *debug-correct-msdf-error* nil)

(defclass ft-context ()
  ((pos
    :accessor pos
    :initarg :pos
    :initform (vec2 0.0 0.0)
    :documentation "Point2")
   (shape
    :accessor shape
    :initarg :shape
    :initform nil
    :documentation "Shape*")
   (contour
    :accessor contour
    :initarg :contour
    :initform nil
    :documentation "Contour*")))

;; Note:
;; Point2 = Vector2
(declaim (inline make-f2-vec2))
(defun make-ft-vec2 (x y)
  (vec2 (/ x 64) (/ y 64)))


(defun main ()

  ;; (format t "~a~%" (freetype2:check-font-file "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf"))
  
  (let ((face (freetype2:new-face "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf")))
    ;; check face if null
  
    ;; printable asii printable characters range including extended
    (iter (for code from 32 to 255)
    ;; (let ((code 164))
	  
	  (let* ((width 32)
		 (height 32)
		 (shape (load-glyph face code))
		 (bitmap (make-array (* width height) :fill-pointer 0)))
	    
	    (normalize (contours shape))
	    
	    (edge-coloring-simple shape 3.0)
	    
	    (iter (for i from 0 below (* width height))
		  (vector-push (make-array 3 :fill-pointer 0) bitmap))
	    
	    (generate-msdf bitmap
			   shape
			   4.0 ; range
			   (vec2 1.0 1.0) ; scale
			   (vec2 4.0 4.0)) ; translation

	    ;; Write RGB float output
	    (when t
	      (with-open-file (out #p"/home/user/font-gen/sdf/output.txt"
				   :direction :output
				   :if-does-not-exist :create
				   :if-exists :supersede)
		(iter (for y from (- height 1) downto 0)
		      (iter (for x from 0 below width)
			    (let ((px (get-pixel bitmap x y width)))
			      (write-line (format nil "(~a, ~a) ~5$ ~5$ ~5$" x y (aref px 0) (aref px 1) (aref px 2))
					  out))))))
	    
	    (write-rgb-buffer-to-ppm-file (format nil "/home/user/font-gen/sdf/~a.ppm" code)
					  bitmap width height)
	    (format t "Wrote ~a~%" (format nil "/home/user/font-gen/sdf/~a.ppm" code)))))
  
  (sb-ext:exit))

(defun write-rgb-buffer-to-ppm-file (filename bitmap width height)
  (with-open-file (stream filename 
			  :element-type '(unsigned-byte 8)
			  :direction :output 
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (let* ((header (format nil "P6~A~D ~D~A255~A" 
			   #\newline
			   width height #\newline
			   #\newline)))
      
      (loop 
	 :for char :across header 
	 :do (write-byte (char-code char) stream)) #| Assumes char-codes match ASCII |#

      (iter (for y from (- height 1) downto 0) ; height-1
	    (iter (for x from 0 below width) ; width
		  (let* ((px (get-pixel bitmap x y width)))
		    (write-byte (clamp (truncate (* (aref px 0) #x100)) 0 #xff) stream)
		    (write-byte (clamp (truncate (* (aref px 1) #x100)) 0 #xff) stream)
		    (write-byte (clamp (truncate (* (aref px 2) #x100)) 0 #xff) stream)))))))


(defun load-glyph (face unicode)
  
  (freetype2:load-char face
		       unicode
		       1) ; FT_LOAD_NO_SCALE
  ;; check chr

  (let* ((shape (make-instance 'shape))
	 (context (make-instance 'ft-context :shape shape)))

    ;; make function for this
    (setf (fill-pointer (contours shape)) 0)
    (adjust-array (contours shape) 0)

    (setf (inverse-y-axis shape) nil)

    ;; do in let*
    ;; (setf (shape context) shape)
    (defparameter *ft-context* context)
    
    (let* ((glyph (freetype2:get-glyph face))
	   (outline (freetype2-types:ft-outlineglyph-outline glyph)))
      
      ;; pass context to callback
      (freetype2:do-outline-decompose outline
	(op p p2 p3)
	(handle-outline-decompose op p p2 p3)))

    shape))


(defun handle-outline-decompose (op p p2 p3)
  
  (cond
    ((eq op :moveto)
     (progn
       (setf (contour *ft-context*) (add-contour (shape *ft-context*)))
       (let ((to (make-ft-vec2 (freetype2-types:ft-vector-x p)
			       (freetype2-types:ft-vector-y p))))
	 (when *debug-outline-decompose*
	   (format t "[moveto] ~a:~%" to)
	   (format t "         ~a -> ~a~%" (pos *ft-context*) to))
	 (setf (pos *ft-context*) (vcopy2 to)))))
    ((eq op :lineto)
     (progn
       (let* ((to (make-ft-vec2 (freetype2-types:ft-vector-x p)
				(freetype2-types:ft-vector-y p)))
	      (edge (make-instance 'linear-segment
				   :p0 (pos *ft-context*)
				   :p1 to)))
	 (when *debug-outline-decompose*
	   (format t "[lineto] ~a:~%" edge)
	   (format t "         ~a~%" to)
	   (format t "         ~a | ~a~%" (pos *ft-context*) to))
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) (vcopy2 to)))))
    ((eq op :conicto)
     (progn
       (let* ((control (make-ft-vec2 (freetype2-types:ft-vector-x p)
				     (freetype2-types:ft-vector-y p)))
	      (to (make-ft-vec2 (freetype2-types:ft-vector-x p2)
				(freetype2-types:ft-vector-y p2)))
	      (edge (make-instance 'quadratic-segment
				   :p0 (pos *ft-context*)
				   :p1 control
				   :p2 to)))
	 (when *debug-outline-decompose*
	   (format t "[conicto] ~a:~%" edge)
	   (format t "          ~a | ~a, ~a~%" (pos *ft-context*) control to))
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) (vcopy2 to)))))
    ((eq op :cubicto)
     (progn
       (error "TODO: cubicto")
       (let* ((control-1 (make-ft-vec2 (freetype2-types:ft-vector-x p)
				       (freetype2-types:ft-vector-y p)))
	      (control-2 (make-ft-vec2 (freetype2-types:ft-vector-x p2)
				       (freetype2-types:ft-vector-y p2)))
	      (to (make-ft-vec2 (freetype2-types:ft-vector-x p3)
				(freetype2-types:ft-vector-y p3)))
	      (edge (make-instance 'cubic-segment
				   :p0 (pos *ft-context*)
				   :p1 control-1
				   :p2 control-2
				   :p3 to)))
	 (when *debug-outline-decompose*
	   (format t "[cubicto] ~a:~%" edge)
	   (format t "          ~a | ~a, ~a, ~a~%" (pos *ft-context*) control-1 control-2 to))
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) (vcopy2 to)))))))
