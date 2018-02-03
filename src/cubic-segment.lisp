(in-package :sdf)

;; Parameters for iterative search of closest point on a cubic Bezier curve. Increase for higher precision.
(defconstant +msdfgen-cubic-search-starts+ 4)
(defconstant +msdfgen-cubic-search-steps+ 4)

(defclass cubic-segment ()
  ((points
    :accessor points
    :initarg :points
    :initform (make-array 4 :fill-pointer 0)
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

(defmethod initialize-instance :after ((edge cubic-segment) &key p0 p1 p2 p3)
  (with-slots (points) edge
    (setf (aref points 0) p0)
    (setf (aref points 1) p1)
    (setf (aref points 2) p2)
    (setf (aref points 3) p3)))

;; void CubicSegment::splitInThirds(EdgeSegment *&part1, EdgeSegment *&part2, EdgeSegment *&part3) const {
;;
;; CubicSegment(p[0],
;;              p[0] == p[1] ? p[0] : mix(p[0], p[1], 1/3.),
;;              mix(mix(p[0], p[1], 1/3.),
;;                  mix(p[1], p[2], 1/3.),
;;                  1/3.),
;;              point(1/3.),
;;              color);
;;
;; CubicSegment(point(1/3.),
;;              mix(mix(mix(p[0], p[1], 1/3.),
;;                      mix(p[1], p[2], 1/3.),
;;                      1/3.),
;;                  mix(mix(p[1], p[2], 1/3.),
;;                      mix(p[2], p[3], 1/3.),
;;                      1/3.),
;;                  2/3.),
;;              mix(mix(mix(p[0], p[1], 2/3.),
;;                      mix(p[1], p[2], 2/3.),
;;                      2/3.),
;;                  mix(mix(p[1], p[2], 2/3.),
;;                      mix(p[2], p[3], 2/3.),
;;                      2/3.),
;;                  1/3.),
;;              point(2/3.),
;;              color);
;;
;; CubicSegment(point(2/3.),
;;              mix(mix(p[1], p[2], 2/3.),
;;                  mix(p[2], p[3], 2/3.),
;;                  2/3.),
;;              p[2] == p[3] ? p[3] : mix(p[2], p[3], 2/3.),
;;              p[3],
;;              color);
;; }
(defmethod split-in-thirds ((edge cubic-segment))
  (error "TODO: cubic-segment:split-in-thirds")
  (with-slots (points color) edge
    (let ((p0 (aref points 0))
	  (p1 (aref points 1))
	  (p2 (aref points 2))
	  (p3 (aref points 3)))
      (values (make-instance 'cubic-segment
			     p0
			     (if (= p0 p1)
				 p0
				 (mix p0 p1 (/ 1 3)))
			     (mix (mix p0 p1 (/ 1 3))
				  (mix p1 p2 (/ 1 3))
				  (/ 1 3))
			     (point edge (/ 1 3))
			     color)
	      (make-instance 'cubic-segment
			     (point edge (/1 3))           
			     (mix (mix (mix p0 p1 (/ 1 3))
				       (mix p1 p2 (/ 1 3))
				       (/ 1 3))
				  (mix (mix p1 p2 (/ 1 3))
				       (mix p2 p3 (/ 1 3))
				       (/ 1 3))
				  (/ 2 3))
			     (mix (mix (mix p0 p1 (/ 2 3))
				       (mix p1 p2 (/ 2 3))
				       (/ 2 3))
				  (mix (mix p1 p2 (/ 2 3))
				       (mix p2 p3 (/ 2 3))
				       (/ 2 3))
				  (/ 1 3))
			     (point edge (/ 2 3))
			     color)
	      (make-instance 'cubic-segment
			     (point edge (/ 2 3))
			     (mix (mix p1 p2 (/ 2 3))
				  (mix p2 p3 (/ 2 3))
				  (/ 2 3))
			     (if (= p2 p3)
				 p3
				 (mix p2 p3 (/ 2 3)))
			     p3
			     color)))))

(defmethod point ((edge cubic-segment) w)
  (let* ((points (points edge))
	 (p0 (aref points 0))
	 (p1 (aref points 1))
	 (p12 (mix p1 p2 w)))
    (mix (mix (mix p0 p1 w)
	      p12
	      w)
	 (mix p12
	      (mix p2 p3 w)
	      w)
	 w)))

;; TODO: Check this
(defmethod move-end-point-cubic ((edge cubic-segment) to)    
  (let ((points (points edge)))
    (setf (aref points 2) (v+ (aref points 2)
			      (v- to (aref points 3))))
    (setf (aref points 3)
	  to)))

(defmethod direction ((edge cubic-segment) param)
  (let* ((points (points edge))
	 (tangent (mix (mix (v- (aref points 1) (aref points 0))
			    (v- (aref points 2) (aref points 1))
			    param)
		       (mix (v- (aref points 2) (aref points 1))
			    (v- (aref points 3) (aref points 2))
			    param)
		       param)))
    ;; when not zero vectors
    (when (v-not tangent)
      (when (= param 0) (return-from direction (v- (aref points 2) (aref points 0))))
      (when (= param 1) (return-from direction (v- (aref points 3) (aref points 1)))))

    tangent))


;; void CubicSegment::bounds(double &l, double &b, double &r, double &t) const {
;;     pointBounds(p[0], l, b, r, t);
;;     pointBounds(p[3], l, b, r, t);
;;     Vector2 a0 = p[1]-p[0];
;;     Vector2 a1 = 2*(p[2]-p[1]-a0);
;;     Vector2 a2 = p[3]-3*p[2] + 3*p[1]-p[0];
;;     double params[2];
;;     int solutions;
;;     solutions = solveQuadratic(params, a2.x, a1.x, a0.x);
;;     for (int i = 0; i < solutions; ++i)
;;         if (params[i] > 0 && params[i] < 1)
;;             pointBounds(point(params[i]), l, b, r, t);
;;     solutions = solveQuadratic(params, a2.y, a1.y, a0.y);
;;     for (int i = 0; i < solutions; ++i)
;;         if (params[i] > 0 && params[i] < 1)
;;             pointBounds(point(params[i]), l, b, r, t);
;; }

(defmethod bounds ((edge cubic-segment) left bottom right top)
  (let ((points (points edge)))
    ;; simplify this
    (multiple-value-bind (l2 b2 r2 t2) (point-bounds (aref points 0) left bottom right top)
      (multiple-value-bind (l3 b3 r3 t3) (point-bounds (aref points 3) l2 b2 r2 t2)
	;; check these
	(let* ((a0 (v- (aref points 1) (aref points 0)))
	       (a1 (v* (v- (aref points 2) (aref points 1) a0) 2))
	       (a2 (v+ (v- (v* (aref points 2) 3) (aref points 3))
		       (v- (v* (aref points 1) 3) (aref points 0)))))
	  
	  (multiple-value-bind (x solutions) (solve-quadratic (vec2 (vx2 a2) (vx2 a1) (vx2 a0)))
	    (iter (for i from 0 below solutions)
		  (when (and (> (aref x i) 0) (< (aref x i) 1))
		    (multiple-value-bind (l4 b4 r4 t4) (point-bounds (point edge (aref x i)) l3 b3 r3 t3)
		      
		      (multiple-value-bind (x solutions) (solve-quadratic (vec2 (vy2 a2) (vy2 a1) (vy2 a0)))
			(iter (for i from 0 below solutions)
			      (when (and (> (aref x i) 0) (< (aref x i) 1))
				(multiple-value-bind (l5 b5 r5 t5) (point-bounds (point edge (aref x i)) l4 b4 r4 t4)
				  (values l5 b5 r5 t5))))))))))))))

;; Check how many times a ray from point R extending to the +X direction intersects
;; the given segment:
;; 0 = no intersection or co-linear
;; +1 = for each intersection increasing in the Y axis
;; -1 = for each intersection decreasing in the Y axis
(defun cross-cubic (r p0 c0 c1 p1 depth cb)
  
  (when (or (< (vy2 r) (min (vy2 p0) (min (vy2 c0) (min (vy2 c1) (vy2 p1)))))
	    (> (vy2 r) (max (vy2 p0) (max (vy2 c0) (max (vy2 c1) (vy2 p1)))))
	    (>= (vx2 r) (max (vx2 p0) (max (vx2 c0) (max (vx2 c1) (vx2 p1))))))
    (return-from cross-cubic 0))
  
  ;; Recursively subdivide the curve to find the intersection point(s). If we haven't
  ;; converged on a solution by a given depth, just treat it as a linear segment
  ;; and call the approximation good enough.
  (when (> (first depth 30))
    (return-from cross-cubic (cross-line r p0 p1 cb)))

  ;; original function is pass by value, not reference
  ;; however points are pass by reference
  (incf (first depth))

  (let* ((mid (v* (v+ c0 c1) 0.5))
	 (c00 (v* (v+ p0 c0) 0.5))
	 (c11 (v* (v+ c1 p1) 0.5))
	 (c01 (v* (v+ c00 mid) 0.5))
	 (c10 (v* (v+ c11 mid) 0.5)))
    (setf mid (v* (v+ c01 c10) 0.5))

    ;; recursive
    (+ (cross-cubic r p0 c00 c01 mid depth cb) (cross-cubic r mid c10 c11 p1 depth cb))))

(defmethod cross-points ((edge cubic-segment) r cb)
  (let ((points (points edge)))
    (cross-cubic r (aref points 0) (aref points 1) (aref points 2) (aref points 3) '(0) cb)))

(defmethod signed-distance ((edge cubic-segment) origin)
  (error "TODO: cubic-segment:signed-distance")
  (let* ((points (points edge))
	 (qa (v- (aref points 0) origin))
	 (ab (v- (aref points 1) (aref points 0)))
	 (br (v- (aref points 2) (aref points 1) ab))
	 (as (v- (v- (aref points 3) (aref points 2))
		 (v- (aref points 2) (aref points 1))
		 br))
	 (ep-dir (direction edge 0))
	 (min-distance (* (non-zero-sign (cross-product ep-dir qa))
			  (vlength qa)))
	 (param (- (/ (v. qa ep-dir) (v. ep-dir ep-dir)))))

    (setf ep-dir (direction edge 1))
    
    (let ((distance (* (non-zero-sign (cross-product ep-dir (v- (aref points 3) origin)))
		       (vunit (v- (aref points 3) origin)))))
      (when (< (abs distance) (abs min-distance))
	(setf min-distance distance)
	(setf param (/ (v. (v- (v+ origin ep-dir) (aref points 3)))
		       (v. ep-dir ep-dir)))))
    
    ;; Iterative minimum distance search
    (iter (for i from 0 to +msdfgen-cubic-search-starts+)
	  (for u = (/ i +msdfgen-cubic-search-starts+))
	  (iter (for step upfrom 0)
		(let ((qpt (v- (point edge u) origin))
		      (distance (* (non-zero-sign (cross-product (direction edge u) qpt))
				   (vlength qpt))))
		  (when (< (abs distance) (min-distance))
		    (setf min-distance distance)
		    (setf param u))
		  (when (= step +msdfgen-cubic-search-steps)
		    (finish))
		  ;; Improve t
		  (let ((d1 (v+ (v* 3 as u u)
				(v* 6 br u)
				(v* 3 ab)))
			(d2 (v+ (v* 6 as u)
				(v* 6 br))))
		    (setf u (- (/ (v. qpt d1)
				  (v+ (v. d1 d1)
				      (v. qpt d2)))))
		    (when (or (< u 0) (> u 1))
		      (finish)))))))

  (when (and (>= param 0) (<= param 1))
    (return-from signed-distance (values (make-instance 'signed-distance
							:distance min-distance
							:dot 0)
					 param)))
  (if (< param 0.5)
      (return-from signed-distance (values (make-instance 'signed-distance
							  :distance min-distance
							  :dot (abs (v. (vunit (direction edge 0))
									(vunit qa))))
					   param))
      (return-from signed-distance (values (make-instance 'signed-distance
							  :distance min-distance
							  :dot (abs (v. (vunit (direction edge 1))
									(vunit (v- (aref points 3) origin)))))
					   param))))
