(in-package :sdf)

(defun solve-quadratic (a b c)
  (when (< (abs a) 1E-14)
    (when (< (abs b) 1E-14)
      (when (= c 0)
	(return-from solve-quadratic (values -1
					     ())))
      (return-from solve-quadratic (values 0
					   ())))
    (return-from solve-quadratic (values 1
					 (list (- (/ c b))))))
  
  (let ((dscr (- (* b b)
		 (* 4 a c))))
    (cond ((> dscr 0)
	   (setf dscr (sqrt dscr))
	   (return-from solve-quadratic (values 2
						(list (/ (+ (- b) dscr) (* 2 a))
						      (/ (- (- b) dscr) (* 2 a))))))
	  ((= dscr 0)
	   (return-from solve-quadratic (values 1
						(list (/ (- b) (* 2 a))))))
	  (t
	   (return-from solve-quadratic (values 0
						()))))))

(defun solve-cubic-normed (a b c)
  (let* ((a2 (* a a))
	 (q (/ (- a2 (* 3 b)) 9))
	 (r (/ (+ (* a (- (* 2 a2) (* 9 b))) (* 27 c)) 54))
	 (r2 (* r r))
	 (q3 (* q q q)))
    (if (< r2 q3)
	(let ((tt (/ r (sqrt q3)))) ; rename tt...
	  (when (< tt -1) (setf tt -1))
	  (when (> tt 1) (setf tt 1))
	  (setf tt (acos tt))
	  (setf a (/ a 3))
	  (setf q (* -2 (sqrt q)))
	  (values 3
		  (list (- (* q (cos (/ tt 3))) a)
			(- (* q (cos (/ (+ tt (* 2 PI)) 3))) a)
			(- (* q (cos (/ (- tt (* 2 PI)) 3))) a))))
	(let ((aa (- (expt (+ (abs r) (sqrt (- r2 q3))) (/ 1 3))))
	      (bb 0))
	  (when (< r 0) (setf aa (- aa)))
	  (setf bb (if (= aa 0) 0 (/ q aa)))
	  (setf a (/ a 3))
	  (let ((x0 (- (+ aa bb) a))
		(x1 (- (* (- 0.5) (+ aa bb)) a))
		(x2 (* 0.5 (sqrt 3.0) (- aa bb))))
	    (if (< (abs x2) 1E-14)
		(values 2 (list x0 x1))
		(values 1 (list x0))))))))

;; int solveCubic(double x[3], double a, double b, double c, double d) {
;;     if (fabs(a) < 1e-14)
;;         return solveQuadratic(x, b, c, d);
;;     return solveCubicNormed(x, b/a, c/a, d/a);
;; }
;; quadratic signed-distance calls this
(defun solve-cubic (a b c d)
  (if (< (abs a) 1E-14)
      (solve-quadratic b c d) ; x[2]
      (solve-cubic-normed (/ b a) (/ c a) (/ d a)))) ; x[3]
