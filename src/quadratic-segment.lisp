(in-package :sdf)

(defclass quadratic-segment ()
  ((points
    :accessor points
    :initarg :points
    :initform (make-array 3 :fill-pointer 0)
    :documentation "")
   (color
    :accessor color
    :initarg :color
    :initform nil
    :documentation "")
   (is-degenerate
    :accessor is-degenerate
    :initarg :is-degenerate
    :initform nil
    :documentation "")))

;; void QuadraticSegment::splitInThirds(EdgeSegment *&part1, EdgeSegment *&part2, EdgeSegment *&part3) const {
;;     part1 = new QuadraticSegment(p[0], mix(p[0], p[1], 1/3.), point(1/3.), color);
;;     part2 = new QuadraticSegment(point(1/3.), mix(mix(p[0], p[1], 5/9.), mix(p[1], p[2], 4/9.), .5), point(2/3.), color);
;;     part3 = new QuadraticSegment(point(2/3.), mix(p[1], p[2], 2/3.), p[2], color);
;; }
(defmethod split-in-thirds ((edge quadratic-segment))
  (with-slots (points color) edge
    (let ((p0 (aref points 0))
      (p1 (aref points 1))
      (p2 (aref points 2)))
      (values (make-instance 'quadratic-segment
                 p0
                 (mix-point p0 p1 p2)
                 (point edge (/ 1 3))
                 color)
          (make-instance 'quadratic-segment
                 (point edge (/ 1 3))
                 (mix-point (mix-point p0 p1 (/ 5 9))
                    (mix-point p1 p2 (/ 4 9)))
                 (point edge (/ 2 3))
                 color)
          (make-instance 'quadratic-segment
                 (point edge (/ 2 3))
                 (mix-point p1 p2 (/ 2 3))
                 p2
                 color)))))

(defmethod point ((edge quadratic-segment) w)
  (let* ((points (points edge))
     (p0 (aref points 0))
     (p1 (aref points 1)))
    (mix-point (mix-point p0 p1 w)
           (mix-point p1 p2 w)
           w)))

(defmethod move-end-point ((edge quadratic-segment) to)
  (let* ((points (points edge))
     (p0 (aref points 0))
     (p1 (aref points 1))
     (p2 (aref points 2))
     (orig-EDir (- p2 p1))
     (orig-P1 p1))
    ;; TODO: use vec2
    (incf (aref p 1) (* (/ (cross-product (- p2 p1)
                      (- to p2))
               (cross-product (- p2 p1)
                      (- p0 p1)))
            (- p0 p1)))
    (setf (aref points 2) to)
    (when (< (dot-product orig-EDir (- p2 p1)) 0)
      (setf (aref points 1) orig-P1))))

;; Vector2 QuadraticSegment::direction(double param) const {
;;     return mix(p[1]-p[0], p[2]-p[1], param);
;; }
(defmethod direction ((edge quadratic-segment) param)
  (let ((points (points edge)))
    (mix-point (v- (aref points 1) (aref points 0))
           (v- (aref points 2) (aref points 1))
           param)))
           
;; void QuadraticSegment::bounds(double &l, double &b, double &r, double &t) const {
;;     pointBounds(p[0], l, b, r, t);
;;     pointBounds(p[2], l, b, r, t);
;;     Vector2 bot = (p[1]-p[0])-(p[2]-p[1]);
;;     if (bot.x) {
;;         double param = (p[1].x-p[0].x)/bot.x;
;;         if (param > 0 && param < 1)
;;             pointBounds(point(param), l, b, r, t);
;;     }
;;     if (bot.y) {
;;         double param = (p[1].y-p[0].y)/bot.y;
;;         if (param > 0 && param < 1)
;;             pointBounds(point(param), l, b, r, t);
;;     }
;; }

(defmethod bounds ((edge quadratic-segment) left bottom right top)
  (let ((points (points edge)))
    (multiple-value-bind (left-1 bottom-1 right-1 top-1) (point-bounds (aref points 0) left bottom right top)
      (multiple-value-bind (left-2 bottom-2 right-2 top-2) (point-bounds (aref points 2) left-1 bottom-1 right-1 top-1)
    (let ((bot (v- (v- (aref points 1) (aref points 0))
               (v- (aref points 2) (aref points 1)))))
      (when (/= (vx2 bot) 0.0)
        (let ((param (/ (- (vx2 (aref points 1))
                   (vx2 (aref points 1)))
                (vx2 bot))))
          (when (and (> param 0)
             (< param 1))
        (multiple-value-bind (left-3 bottom-3 right-3 top-3) (point-bounds (point edge param) left-2 bottom-2 right-2 top-2)
          (setf left left-3)
          (setf bottom bottom-3)
          (setf right right-3)
          (setf top top-3)))))      
      (when (/= (vy2 bot) 0.0)
        (let ((param (/ (- (vy2 (aref points 1))
                   (vy2 (aref points 1)))
                (vy2 bot))))
          (when (and (> param 0)
             (< param 1))
        (multiple-value-bind (left-3 bottom-3 right-3 top-3) (point-bounds (point edge param) left-2 bottom-2 right-2 top-2)
          (setf left left-3)
          (setf bottom bottom-3)
          (setf right right-3)
          (setf top top-3)))))))))
  (values left bottom right top))

;; /// Check how many times a ray from point R extending to the +X direction intersects
;; /// the given segment:
;; ///  0 = no intersection or co-linear
;; /// +1 = for each intersection increasing in the Y axis
;; /// -1 = for each intersection decreasing in the Y axis
(defun cross-quad (r p0 c0 p1 depth cb)

  (when (or (< (vy2 r) (min (vy2 p0) (min (vy2 c0) (vy2 p1))))
	    (> (vy2 r) (max (vy2 p0) (max (vy2 c0) (vy2 p1))))
	    (>= (vx2 r) (max (vx2 p0) (max (vx2 c0) (vx2 p1)))))
    (return-from cross-quad 0))
  
  ;; Recursively subdivide the curve to find the intersection point(s). If we haven't
  ;; converged on a solution by a given depth, just treat it as a linear segment
  ;; and call the approximation good enough.
  (when (> depth 30)
    (return-from cross-quad (cross-line r p0 p1 cb)))

  (incf depth)

  (let ((mc0 (v* (v+ p0 c0) 0.5))
	(mc1 (v* (v+ c0 p1) 0.5))
	(mid (v* (v+ mc0 mc1) 0.5)))

    (+ (cross-quad r p0 mc0 mid depth cb) (cross-quad r mid mc1 p1 depth cb))))

(defmethod cross-points ((edge quadratic-segment) r cb)
  (let ((points (points edge)))
    (cross-quad r (aref points 0) (aref points 1) (aref points 2) 0 cb)))

(defmethod signed-distance ((edge quadratic-segment) origin)
  (let* ((points (points edge))
	 (qa (v- (aref points 0) origin))
	 (ab (v- (aref points 1) (aref points 0)))
	 (br (v- (v+ (aref points 0) (aref points 2)) (aref points 1) (aref points 1)))
	 (a (dot-product br br))
	 (b (* 3 (dot-product ab br)))
	 (c (* 2 (dot-product qa ab)))
	 (d (dot-product qa ab))
	 (solutions (solve-cubic t a b c d))
	 (min-distance (* (non-zero-sign (cross-product ab qa)) (vlength qa)))
	 (param (- (/ (dot-product qa ab) (dot-product ab ab)))))

    (let ((distance (* (non-zero-sign (cross-product (v- (aref points 2) (aref points 1)) (v- (aref points 2) origin)))
		       (vlength (v- (aref points 2) origin)))))
      (when (< (abs distance) (abs min-distance))
	(setf min-distance distance)
	(setf param (/ (dot-product (v- origin (aref points 1)) (v- (aref points 2) (aref points 1)))
		       (dot-product (v- (aref points 2) (aref points 1)) (v- (aref points 2) (aref points 1)))))))

    (let ((e (make-array 3 :initial-contents '(0.0 0.0 0.0))))
      (iter (for i from 0 below solutions)
	    (when (and (> (aref e i) 0)
		       (< (aref e i) 1))
	      (let* ((endpoint (v+ (aref points 0) (v* 2 (aref e i) ab) (v* (aref e i) (aref e i) br)))
		     (distance (* (non-zero-sign (cross-product (v- (aref points 2) (aref points 0)) (v- endpoint origin)))
				  (vlength (v- endpoint origin)))))
		(when (<= (abs distance) (abs min-distance))
		  (setf min-distance distance)
		  (setf param (aref e i)))))))

    (when (and (>= param 0) (<= param 1))
      (return-from signed-distance (values (make-instance 'signed-distance
							  :distance min-distance
							  :dot 0)
					   param)))
    (if (< param 0.5)
	(return-from signed-distance (values (make-instance 'signed-distance
							    :distance min-distance
							    :dot (abs (dot-product (vunit ab) (vunit qa))))
					     param))
	(return-from signed-distance (values (make-instance 'signed-distance
							    :distance min-distance
							    :dot (abs (dot-product (vunit (v- (aref points 2) (aref points 1)))
										   (vunit (v- (aref points 2) origin)))))
					     param)))))
