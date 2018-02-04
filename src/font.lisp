(in-package :sdf)

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
