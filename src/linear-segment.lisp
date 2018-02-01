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

;; void LinearSegment::splitInThirds(EdgeSegment *&part1, EdgeSegment *&part2, EdgeSegment *&part3) const {
;;     part1 = new LinearSegment(p[0], point(1/3.), color);
;;     part2 = new LinearSegment(point(1/3.), point(2/3.), color);
;;     part3 = new LinearSegment(point(2/3.), p[1], color);
;; }
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
    (mix-point p0 p1 w)))

(defmethod move-end-point ((edge linear-segment) to)
  (setf (aref (points edge) 1) to))

;; Vector2 LinearSegment::direction(double param) const {
;;     return p[1]-p[0];
;; }
(defmethod direction ((edge linear-segment) param)
  (let ((points (points edge)))
    ;; (format t "[l:direction] ~a~%" points)
    (v- (aref points 1) (aref points 0))))

;; void LinearSegment::bounds(double &l, double &b, double &r, double &t) const {
;;     pointBounds(p[0], l, b, r, t);
;;     pointBounds(p[1], l, b, r, t);
;; }
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

;; /// Check how many times a ray from point R extending to the +X direction intersects
;; /// the given segment:
;; ///  0 = no intersection or co-linear
;; /// +1 = intersection increasing in the Y axis
;; /// -1 = intersection decreasing in the Y axis
;; static int crossLine(const Point2& r, const Point2& p0, const Point2& p1, EdgeSegment::CrossingCallback* cb) {
;;     if (r.y < min(p0.y, p1.y))
;;         return 0;
;;     if (r.y >= max(p0.y, p1.y))
;;         return 0;
;;     if (r.x >= max(p0.x, p1.x))
;;         return 0;
;;     // Intersect the line at r.y and see if the intersection is before or after the origin.
;;     double xintercept = (p0.x + (r.y - p0.y) * (p1.x - p0.x) / (p1.y - p0.y)); // a + ((b * c) / d)
;;     if (r.x < xintercept) {
;;         int w = (p0.y < p1.y) ? 1 : -1;
;;         if( cb != NULL ) {
;;             cb->intersection(Point2(xintercept, r.y), w);
;;         }
;;         return w;
;;     }
;;     return 0;
;; }
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
	 (param (/ (dot-product aq ab) (dot-product ab ab)))
	 (eq (v- (aref points (if (> param 0.5) 1 0)) origin))
	 (end-point-distance (vlength eq)))
    (when (and (> param 0)
	       (< param 1))
      (let ((ortho-distance (dot-product (get-orthonormal ab nil) aq)))
	(when (< (abs ortho-distance) end-point-distance)
	  (return-from signed-distance (values (make-instance 'signed-distance :distance ortho-distance :dot 0)
					       param)))))
    (values (make-instance 'signed-distance
			   :distance (* (non-zero-sign (cross-product aq ab)) end-point-distance)
			   :dot (abs (dot-product (vunit ab) (vunit eq))))
	    param)))

