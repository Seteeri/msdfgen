(in-package :sdf)

(defclass linear-segment ()
  ((points
    :accessor points
    :initarg :points
    :initform (make-array 2 :fill-pointer 0)
    :documentation "")
   (color
    :accessor color
    :initarg :color
    :initform +white+
    :documentation "")
   (is-degenerate
    :accessor is-degenerate
    :initarg :is-degenerate
    :initform nil
    :documentation "")))

(defmethod initialize-instance :after ((edge linear-segment) &key p0 p1)
  (with-slots (points) edge
    (setf (aref points 0) p0)
    (setf (aref points 1) p1)))

(defmethod split-in-thirds ((edge linear-segment))
  (with-slots (points color) edge
    ;;(assert (not (eq color nil)))
    (let* ((p0 (aref points 0))
	   (p1 (aref points 1))
	   (e0 (make-instance 'linear-segment
			      :points (make-array 2 :initial-contents (list p0
									    (point edge (/ 1 3))))
			      :color color))
	   (e1 (make-instance 'linear-segment
			      :points (make-array 2 :initial-contents (list (point edge (/ 1 3))
									    (point edge (/ 2 3))))
			      :color color))
	   (e2 (make-instance 'linear-segment
			      :points (make-array 2 :initial-contents (list (point edge (/ 2 3))
									    p1))
			      :color color)))

      (when *debug-split-in-thirds*
	(format t "[l:s-i-t] ~a: ~a~%" e0 (points e0))
	(format t "[l:s-i-t] ~a: ~a~%" e1 (points e1))
	(format t "[l:s-i-t] ~a: ~a~%" e2 (points e2)))
      
      (values e0 e1 e2))))

(defmethod point ((edge linear-segment) w)
  (let* ((points (points edge))
	 (p0 (aref points 0))
	 (p1 (aref points 1)))
    ;; (format t "[point] ~a, ~a~%" p0 p1)
    (mix p0 p1 w)))

(defmethod move-end-point ((edge linear-segment) to)
  (setf (aref (points edge) 1) to))

(defmethod direction ((edge linear-segment) param)
  (let ((points (points edge)))
    ;; (format t "[l:direction] ~a~%" points)
    (v- (aref points 1) (aref points 0))))

(defmethod bounds ((edge linear-segment) left bottom right top)
  (let ((points (points edge)))
    (when *debug-bounds*
      (format t "    [bounds:linear] iterating~%"))
    (multiple-value-bind (l2 b2 r2 t2) (point-bounds (aref points 0) left bottom right top)
      (multiple-value-bind (l3 b3 r3 t3) (point-bounds (aref points 1) l2 b2 r2 t2)
	(values l3 b3 r3 t3)))))

;; int LinearSegment::crossings(const Point2 &r, CrossingCallback* cb) const {
;;     return crossLine(r, p[0], p[1], cb);
;; }
(defmethod cross-points ((edge linear-segment) r cb)
  (let ((points (points edge)))
    (cross-line r (aref points 0) (aref points 1) cb)))

;; TODO: Have cross-* return a symbol rather than a number
;;
;; Check how many times a ray from point R extending to the +X direction intersects
;; the given segment:
;; 0 = no intersection or co-linear
;; +1 = intersection increasing in the Y axis
;; -1 = intersection decreasing in the Y axis
(defun cross-line (r p0 p1 cb)

  ;; (format t "[cross-line] ~a, ~a, ~a~%"
  ;; 	  (< (vy2 r) (min (vy2 p0) (vy2 p1)))
  ;; 	  (>= (vy2 r) (max (vy2 p0) (vy2 p1)))
  ;; 	  (>= (vx2 r) (max (vx2 p0) (vx2 p1))))
  ;; (format t "[cross-line] pts: ~a, ~a~%"
  ;; 	  ;r
  ;; 	  p0
  ;; 	  p1)
  
  (when (< (vy2 r) (min (vy2 p0) (vy2 p1)))
    ;; (format t "[cross-line] 1~%")
    (return-from cross-line 0))
  (when (>= (vy2 r) (max (vy2 p0) (vy2 p1)))
    ;; (format t "[cross-line] 2~%")
    (return-from cross-line 0))
  (when (>= (vx2 r) (max (vx2 p0) (vx2 p1)))
    ;; (format t "[cross-line] 3~%")
    (return-from cross-line 0))
  
  (let ((x-intercept (+ (vx2 p0)
			(/ (* (- (vy2 r) (vy2 p0))
			      (- (vx2 p1) (vx2 p0)))
			   (- (vy2 p1)
			      (vy2 p0))))))
    (when (< (vx2 r) x-intercept)
      (let ((w (if (< (vy2 p0) (vy2 p1)) 1 -1)))
	(when cb
	  ;; (format t "[cross-line] cb~%")
	  (funcall cb (vec2 x-intercept (vy2 r)) w))
	;; (format t "[cross-line] 4~%")
	(return-from cross-line w))))

  ;; (format t "[cross-line] 5~%")
  0)

(defmethod signed-distance ((edge linear-segment) origin)
  (let* ((points (points edge))
	 (aq (v- origin (aref points 0)))
	 (ab (v- (aref points 1) (aref points 0)))
	 (param (/ (v. aq ab) (v. ab ab)))
	 (eq (v- (aref points (if (> param 0.5) 1 0)) origin))
	 (end-point-distance (vlength eq)))

    (when *debug-linear-signed-distance*
      (format t "    [lin:sd] p0 = ~a~%" (aref points 0))
      (format t "    [lin:sd] p1 = ~a~%" (aref points 1))
      (format t "    [lin:sd] aq = ~a~%" aq)
      (format t "    [lin:sd] ab = ~a~%" ab)
      (format t "    [lin:sd] param = ~a~%" param)
      (format t "    [lin:sd] eq = ~a~%" eq)
      (format t "    [lin:sd] end-point-distance = ~a~%" end-point-distance))

    (when (and (> param 0)
	       (< param 1))
      (let ((ortho-distance (v. (get-orthonormal ab nil) aq)))
	(when (< (abs ortho-distance) end-point-distance)
	  (return-from signed-distance (values (make-instance 'signed-distance :distance ortho-distance :dot 0)
					       param)))))
    (values (make-instance 'signed-distance
			   :distance (* (non-zero-sign (cross-product aq ab)) end-point-distance)
			   :dot (abs (v. (vunit ab) (vunit eq))))
	    param)))

