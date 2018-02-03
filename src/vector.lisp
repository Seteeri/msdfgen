(in-package :sdf)

;; static void pointBounds(Point2 p, double &l, double &b, double &r, double &t) {
;;     if (p.x < l) l = p.x;
;;     if (p.y < b) b = p.y;
;;     if (p.x > r) r = p.x;
;;     if (p.y > t) t = p.y;
;; }
(defun point-bounds (point left bottom right top)
  (when *debug-bounds*
    (format t "       [point-bounds] ~a, ~a, ~a, ~a, ~a~%" point left bottom right top))
  (values (if (< (vx2 point) left) (vx2 point) left)
	  (if (< (vy2 point) bottom) (vy2 point) bottom)
	  (if (> (vx2 point) right) (vx2 point) right)
	  (if (> (vy2 point) top) (vy2 point) top)))

;; Returns whether zero vector
(declaim (inline v-not))
(defun v-not (v)
  (and (= (vx2 v) 0.0)
       (= (vy2 v) 0.0)))

;; Not available from 3d-vectors so must implement here
(declaim (inline cross-product))
(defun cross-product (a b)
  (- (* (vx2 a)
	(vy2 b))
     (* (vy2 a)
	(vx2 b))))

(defun get-orthonormal (point &optional (polarity t) (allow-zero nil))
  (let ((len (vlength point)))
    (if (= len 0)
	(if polarity
	    (vec2 0 (if (not allow-zero) 1 0))
	    (vec2 0 (if (not allow zero) -1 0)))
	(if polarity
	    (vec2 (/ (- (vy2 point)) len) (/ (vx2 point) len))
	    (vec2 (/ (vy2 point) len) (/ (- (vx2 point)) len))))))
