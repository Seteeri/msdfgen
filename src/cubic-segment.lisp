(in-package :sdf)

(defclass cubic-segment ()
  ((points
    :accessor points
    :initarg :points
    :initform (make-array 4 :fill-pointer 0)
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
  (with-slots (points color) edge
    (let ((p0 (aref points 0))
	  (p1 (aref points 1))
	  (p2 (aref points 2))
	  (p3 (aref points 3)))
      (values (make-instance 'cubic-segment
			     p0
			     (if (= p0 p1)
				 p0
				 (mix-point p0 p1 (/ 1 3)))
			     (mix-point (mix-point p0 p1 (/ 1 3))
					(mix-point p1 p2 (/ 1 3))
					(/ 1 3))
			     (point edge (/ 1 3))
			     color)
	      (make-instance 'cubic-segment
			     (point edge (/1 3))           
			     (mix-point (mix-point (mix-point p0 p1 (/ 1 3))
						   (mix-point p1 p2 (/ 1 3))
						   (/ 1 3))
					(mix-point (mix-point p1 p2 (/ 1 3))
						   (mix-point p2 p3 (/ 1 3))
						   (/ 1 3))
					(/ 2 3))
			     (mix-point (mix-point (mix-point p0 p1 (/ 2 3))
						   (mix-point p1 p2 (/ 2 3))
						   (/ 2 3))
					(mix-point (mix-point p1 p2 (/ 2 3))
						   (mix-point p2 p3 (/ 2 3))
						   (/ 2 3))
					(/ 1 3))
			     (point edge (/ 2 3))
			     color)
	      (make-instance 'cubic-segment
			     (point edge (/ 2 3))
			     (mix-point (mix-point p1 p2 (/ 2 3))
					(mix-point p2 p3 (/ 2 3))
					(/ 2 3))
			     (if (= p2 p3)
				 p3
				 (mix-point p2 p3 (/ 2 3)))
			     p3
			     color)))))

(defmethod point ((edge cubic-segment) w)
  (let* ((points (points edge))
	 (p0 (aref points 0))
	 (p1 (aref points 1))
	 (p12 (mix-point p1 p2 w)))
    (mix-point (mix-point (mix-point p0 p1 w)
			  p12
			  w)
	       (mix-point p12
			  (mix-point p2 p3 w)
			  w)
	       w)))

(defmethod move-end-point-cubic ((edge cubic-segment) to)    
  (let ((points (points edge)))
    (incf (aref points 2)
	  (- to (aref points 3)))
    (setf (aref points 3)
	  to)))

;; Vector2 CubicSegment::direction(double param) const {
;;     Vector2 tangent = (mix (mix p[1]-p[0] p[2]-p[1] param)
;;                            (mix p[2]-p[1] p[3]-p[2] param)
;;                            param)
;;     if (!tangent) {
;;         if (param == 0) return p[2]-p[0];
;;         if (param == 1) return p[3]-p[1];
;;     }
;;     return tangent;
;; }
(defmethod direction ((edge cubic-segment) param)
  (let* ((points (points edge))
	 (tangent (mix-point (mix-point (v- (aref points 1) (aref points 0))
					(v- (aref points 2) (aref points 1))
					param)
			     (mix-point (v- (aref points 2) (aref points 1))
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


;; /// Check how many times a ray from point R extending to the +X direction intersects
;; /// the given segment:
;; ///  0 = no intersection or co-linear
;; /// +1 = for each intersection increasing in the Y axis
;; /// -1 = for each intersection decreasing in the Y axis
;; static int crossCubic(const Point2& r, const Point2& p0, const Point2& c0, const Point2& c1, const Point2& p1, int depth, EdgeSegment::CrossingCallback* cb) {
;;     if (r.y < min(p0.y, min(c0.y, min(c1.y, p1.y))))
;;         return 0;
;;     if (r.y > max(p0.y, max(c0.y, max(c1.y, p1.y))))
;;         return 0;
;;     if (r.x >= max(p0.x, max(c0.x, max(c1.x, p1.x))))
;;         return 0;

;;     // Recursively subdivide the curve to find the intersection point(s). If we haven't
;;     // converged on a solution by a given depth, just treat it as a linear segment
;;     // and call the approximation good enough.
;;     if( depth > 30 )
;;         return crossLine(r, p0, p1, cb);

;;     depth++;

;;     Point2 mid = (c0 + c1) * 0.5;
;;     Point2 c00 = (p0 + c0) * 0.5;
;;     Point2 c11 = (c1 + p1) * 0.5;
;;     Point2 c01 = (c00 + mid) * 0.5;
;;     Point2 c10 = (c11 + mid) * 0.5;

;;     mid = (c01 + c10) * 0.5;

;;     return crossCubic(r, p0, c00, c01, mid, depth, cb) + crossCubic(r, mid, c10, c11, p1, depth, cb);
;; }

(defmethod crossings2 ((edge cubic-segment) r cb)
  (let ((points (points edge)))
    (cross-cubic r (aref points 0) (aref points 1) (aref points 2) (aref points 3) 0 cb)))

;; SignedDistance CubicSegment::signedDistance(Point2 origin, double &param) const {
;;     Vector2 qa = p[0]-origin;
;;     Vector2 ab = p[1]-p[0];
;;     Vector2 br = p[2]-p[1]-ab;
;;     Vector2 as = (p[3]-p[2])-(p[2]-p[1])-br;

;;     Vector2 epDir = direction(0);
;;     double minDistance = nonZeroSign(crossProduct(epDir, qa))*qa.length(); // distance from A
;;     param = -dotProduct(qa, epDir)/dotProduct(epDir, epDir);
;;     {
;;         epDir = direction(1);
;;         double distance = nonZeroSign(crossProduct(epDir, p[3]-origin))*(p[3]-origin).length(); // distance from B
;;         if (fabs(distance) < fabs(minDistance)) {
;;             minDistance = distance;
;;             param = dotProduct(origin+epDir-p[3], epDir)/dotProduct(epDir, epDir);
;;         }
;;     }
;;     // Iterative minimum distance search
;;     for (int i = 0; i <= MSDFGEN_CUBIC_SEARCH_STARTS; ++i) {
;;         double t = (double) i/MSDFGEN_CUBIC_SEARCH_STARTS;
;;         for (int step = 0;; ++step) {
;;             Vector2 qpt = point(t)-origin;
;;             double distance = nonZeroSign(crossProduct(direction(t), qpt))*qpt.length();
;;             if (fabs(distance) < fabs(minDistance)) {
;;                 minDistance = distance;
;;                 param = t;
;;             }
;;             if (step == MSDFGEN_CUBIC_SEARCH_STEPS)
;;                 break;
;;             // Improve t
;;             Vector2 d1 = 3*as*t*t+6*br*t+3*ab;
;;             Vector2 d2 = 6*as*t+6*br;
;;             t -= dotProduct(qpt, d1)/(dotProduct(d1, d1)+dotProduct(qpt, d2));
;;             if (t < 0 || t > 1)
;;                 break;
;;         }
;;     }

;;     if (param >= 0 && param <= 1)
;;         return SignedDistance(minDistance, 0);
;;     if (param < .5)
;;         return SignedDistance(minDistance, fabs(dotProduct(direction(0).normalize(), qa.normalize())));
;;     else
;;         return SignedDistance(minDistance, fabs(dotProduct(direction(1).normalize(), (p[3]-origin).normalize())));
;; }
