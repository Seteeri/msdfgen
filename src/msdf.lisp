(in-package :sdf)

(defclass multi-distance ()
  ((r
    :accessor r
    :initarg :r
    :initform nil
    :documentation "")
   (g
    :accessor g
    :initarg :g
    :initform nil
    :documentation "")
   (b
    :accessor b
    :initarg :b
    :initform nil
    :documentation "")
   (med
    :accessor med
    :initarg :med
    :initform nil
    :documentation "")))

(defun pixel-clash (a b threshold)
  (let ((a-in (>= (+ (if (> (aref a 0) 0.5) 1 0)
		     (if (> (aref a 1) 0.5) 1 0)
		     (if (> (aref a 2) 0.5) 1 0))
		  2))
	(b-in (>= (+ (if (> (aref b 0) 0.5) 1 0)
		     (if (> (aref b 1) 0.5) 1 0)
		     (if (> (aref b 2) 0.5) 1 0))
		  2)))

    (when (not (eq a-in b-in))
      (return-from pixel-clash))

    ;; If the change is 0 <-> 1 or 2 <-> 3 channels and not 1 <-> 1 or 2 <-> 2, it is not a clash
    (when (or (and (> (aref a 0) 0.5) (> (aref a 1) 0.5) (> (aref a 2) 0.5))
	      (and (< (aref a 0) 0.5) (< (aref a 1) 0.5) (< (aref a 2) 0.5))
	      (and (> (aref b 0) 0.5) (> (aref b 1) 0.5) (> (aref b 2) 0.5))
	      (and (< (aref b 0) 0.5) (< (aref b 1) 0.5) (< (aref b 2) 0.5)))
      (return-from pixel-clash))

    
    ;; If the change is 0 <-> 1 or 2 <-> 3 channels and not 1 <-> 1 or 2 <-> 2, it is not a clash
    ;;     if (   (a.r > .5f && a.g > .5f && a.b > .5f)
    ;;         || (a.r < .5f && a.g < .5f && a.b < .5f)
    ;;         || (b.r > .5f && b.g > .5f && b.b > .5f)
    ;;         || (b.r < .5f && b.g < .5f && b.b < .5f))
    ;;         return false;
    (when (or (and (> (aref a 0) 0.5) (> (aref a 1) 0.5) (> (aref a 2) 0.5))
	      (and (< (aref a 0) 0.5) (< (aref a 1) 0.5) (< (aref a 2) 0.5))
	      (and (> (aref b 0) 0.5) (> (aref b 1) 0.5) (> (aref b 2) 0.5))
	      (and (< (aref b 0) 0.5) (< (aref b 1) 0.5) (< (aref b 2) 0.5)))
      (return-from pixel-clash))
    
    ;; Find which color is which: _a, _b = the changing channels, _c = the remaining one
    ;;     float aa, ab, ba, bb, ac, bc;
    (let ((aa 0.0)
	  (ab 0.0)
	  (ba 0.0)
	  (bb 0.0)
	  (ac 0.0)
	  (bc 0.0))
      
      (cond ((and (not (eq (> (aref a 0) 0.5) (> (aref b 0) 0.5)))
		  (not (eq (< (aref a 0) 0.5) (< (aref b 0) 0.5))))
	     (setf aa (aref a 0))
	     (setf bb (aref b 0))
	     (cond ((and (not (eq (> (aref a 1) 0.5) (> (aref b 1) 0.5)))
			 (not (eq (< (aref a 1) 0.5) (< (aref b 1) 0.5))))
		    (setf ab (aref a 1))
		    (setf bb (aref b 1))
		    (setf ac (aref a 2))
		    (setf bc (aref b 2)))
		   ((and (not (eq (> (aref a 2) 0.5) (> (aref b 2) 0.5)))
			 (not (eq (< (aref a 2) 0.5) (< (aref b 2) 0.5))))
		    (setf ab (aref a 2))
		    (setf bb (aref b 2))
		    (setf ac (aref a 1))
		    (setf bc (aref b 1)))
		   (t
		    (error "this should never happen")
		    (return-from pixel-clash))))
	    ((and (not (eq (> (aref a 1) 0.5) (> (aref b 1) 0.5)))
		  (not (eq (< (aref a 1) 0.5) (< (aref b 1) 0.5)))
		  (not (eq (> (aref a 2) 0.5) (> (aref b 2) 0.5)))
		  (not (eq (< (aref a 2) 0.5) (< (aref b 2) 0.5))))
	     (setf aa (aref a 1))
	     (setf ba (aref b 1))
	     (setf ab (aref a 2))
	     (setf bb (aref b 2))
	     (setf ac (aref a 0))
	     (setf bc (aref b 0)))
	    (t
	     (return-from pixel-clash)))

      (return-from pixel-clash (and (>= (abs (- aa ba)) threshold)
				    (>= (abs (- ab bb)) threshold)
				    (>= (abs (- ac 0.5)) (abs (- bc 0.5))))))))

;; void msdfErrorCorrection(Bitmap<FloatRGB> &output, const Vector2 &threshold) {
;;     std::vector<std::pair<int, int> > clashes;
;;     int w = output.width(), h = output.height();
;;     for (int y = 0; y < h; ++y)
;;         for (int x = 0; x < w; ++x) {
;;             if (   (x > 0 && pixelClash(output(x, y), output(x-1, y), threshold.x))
;;                 || (x < w-1 && pixelClash(output(x, y), output(x+1, y), threshold.x))
;;                 || (y > 0 && pixelClash(output(x, y), output(x, y-1), threshold.y))
;;                 || (y < h-1 && pixelClash(output(x, y), output(x, y+1), threshold.y)))
;;                 clashes.push_back(std::make_pair(x, y));
;;         }
;;     for (std::vector<std::pair<int, int> >::const_iterator clash = clashes.begin(); clash != clashes.end(); ++clash) {
;;         FloatRGB &pixel = output(clash->first, clash->second);
;;         float med = median(pixel.r, pixel.g, pixel.b);
;;         pixel.r = med, pixel.g = med, pixel.b = med;
;;     }
;; }
(defun correct-msdf-error (output threshold)
  (let ((clashes (make-array 0 :fill-pointer 0 :adjustable t))
	(w 32)
	(h 32))
    (iter (for y from 0 below h)
	  (iter (for x from 0 below w)
		(when (or (and (> x 0) (pixel-clash (get-pixel output x y w) (get-pixel output (- x 1) y w) (vx2 threshold)))
			  (and (< x (- w 1)) (pixel-clash (get-pixel output x y w) (get-pixel output (+ x 1) y w) (vx2 threshold)))
			  (and (> y 0) (pixel-clash (get-pixel output x y w) (get-pixel output x (- y 1) w) (vy2 threshold)))
			  (and (< y (- h 1)) (pixel-clash (get-pixel output x y w) (get-pixel output x (+ y 1) w) (vy2 threshold))))
		  (vector-push-extend (make-array 2 :initial-contents (list x y))
				      clashes))))
    (format t "[correct-msdf-error] clashes length: ~a~%" (length clashes))
    (iter (for clash in-vector clashes)
	  (let* ((pixel (get-pixel output (aref clash 0) (aref clash 1) w))
		 (med (median (aref pixel 0) (aref pixel 1) (aref pixel 2))))
	    (setf (aref pixel 0) med)
	    (setf (aref pixel 1) med)
	    (setf (aref pixel 2) med)))))

(defun get-pixel (output x y w)
  (aref output (+ (* y w) x)))

(defclass edge-point ()
  ((min-distance
    :accessor min-distance
    :initarg :min-distance
    :initform (make-instance 'signed-distance)
    :documentation "SignedDistance")
   (near-edge
    :accessor near-edge
    :initarg :near-edge
    :initform nil
    :documentation "EdgeHolder *")
   (near-param
    :accessor near-param
    :initarg :near-param
    :initform 0.0
    :documentation "double")))

(defun generate-msdf (output shape range scale translate &optional (edge-threshold 1.00000001d0))
  (let* ((contour-count (length (contours shape)))
	 (w 32) ;(w (width output))
	 (h 32) ;(h (height output))
	 (spanner (make-instance 'winding-spanner))
	 (contour-sd (make-array contour-count :fill-pointer 0 :adjustable t)))

    (iter (for y from 0 below h)
	  (let ((row (if (inverse-y-axis shape)
			 (- h y 1)
			 y)))

	    (when *debug-generate-msdf*
	      (format t "[generate-msdf] call bounds~%"))

	    (multiple-value-bind (bound-left bound-bottom bound-right bound-top) (bounds shape 0.0 0.0 0.0 0.0)
	      (when *debug-generate-msdf*
		(format t "~%[generate-msdf] collect-crossings - y, row: ~a, ~a~%" y row))
	      (collect-crossings spanner
				 shape
				 (vec2 (- bound-left 0.5)
				       (- (/ (+ y 0.5) (vy2 scale)) (vy2 translate)))))
	    (when *debug-generate-msdf*
	      (format t "[generate-msdf] call bounds done~%"))
	    
	    (iter (for x from 0 below w)
		  (for foobar = (if *debug-generate-msdf* (format t "[generate-msdf] x: ~a~%" x) nil))
		  (for p = (v- (v/ (vec2 (+ x 0.5) (+ y 0.5))
				   scale)
			       translate))
		  (for sr = (make-instance 'edge-point))
		  (for sg = (make-instance 'edge-point))
		  (for sb = (make-instance 'edge-point))
		  (for real-sign = (advance-to spanner (vx2 p)))

		  (when *debug-generate-msdf*
		    (format t "[generate-msdf] real-sign: ~a, p: ~4$, ~4$~%" real-sign (vx2 p) (vy2 p)))
		  
		  (iter (for contour in-vector (contours shape))
			(for r = (make-instance 'edge-point))
			(for g = (make-instance 'edge-point))
			(for b = (make-instance 'edge-point))
			(for ii = 0)

			(when *debug-generate-msdf*
			  (format t " [generate-msdf] length edges: ~a~%" (length (edges contour))))
			
			(iter (for edge in-vector (edges contour))
			      ;; signed-distance - in edges
			      ;; fix nvunit -> vunit

			      (when *debug-generate-msdf*
				(format t " [generate-msdf] -----------------~%"))
			      
			      (multiple-value-bind (distance param) (signed-distance edge p)

				(when *debug-generate-msdf*
				  (format t "  [generateMSDF][e#~a] ~a~%" ii edge)
				  (format t "  [generateMSDF][e#~a] dist: ~4$, dot: ~4$~%"
					  ii
					  (distance distance)
					  (dot distance))
				  (format t "                       param: ~4$~%"
					  param))

				(when *debug-generate-msdf*
				  (when (= ii 10)
				    (format t "  [generateMSDF][e#~a] color edge: ~a, logand green: ~a~%"
					    ii (color edge) (logand (color edge) +green+))))
				
				(when (and (not (= (logand (color edge) +red+) 0))
					   (sd< distance (min-distance r)))
				  (setf (min-distance r) distance)
				  (setf (near-edge r) edge)
				  (setf (near-param r) param))
				(when (and (not (= (logand (color edge) +green+) 0))
					   (sd< distance (min-distance g)))
				  (setf (min-distance g) distance)
				  (setf (near-edge g) edge)
				  (setf (near-param g) param))
				(when (and (not (= (logand (color edge) +blue+) 0))
					   (sd< distance (min-distance b)))
				  (setf (min-distance b) distance)
				  (setf (near-edge b) edge)
				  (setf (near-param b) param))

				(when *debug-generate-msdf*
				  (format t "  [generateMSDF][e#~a] r dist, dot: ~4$, ~4$~%"
					  ii
					  (if (< (distance (min-distance r)) 0.0)
					      nil
					      (distance (min-distance r)))
					  (dot (min-distance r)))
				  (format t "  [generateMSDF][e#~a] g dist, dot: ~4$, ~4$~%"
					  ii
					  (if (< (distance (min-distance g)) 0.0)
					      nil
					      (distance (min-distance g)))
					  (dot (min-distance g)))
				  (format t "  [generateMSDF][e#~a] b dist, dot: ~4$, ~4$~%"
					  ii
					  (if (< (distance (min-distance b)) 0.0)
					      nil
					      (distance (min-distance b)))
					  (dot (min-distance b))))

				;; 10
				(when *debug-generate-msdf*
				  (when (= ii -1) (sb-ext:exit)))
				(incf ii)

				t))

			(when (sd< (min-distance r)
				   (min-distance sr))
			  (setf sr r))
			(when (sd< (min-distance g)
				   (min-distance sg))
			  (setf sg g))
			(when (sd< (min-distance b)
				   (min-distance sb))
			  (setf sb b)))

		  (when t
		    (when (and (= x 16) (= row 8))
		      (sb-ext:exit)))
		  
		  (when (near-edge sr)
		    (distance-to-pseudo-distance (near-edge sr) (min-distance sr) p (near-param sr)))
		  (when (near-edge sg)
		    (distance-to-pseudo-distance (near-edge sg) (min-distance sg) p (near-param sg)))
		  (when (near-edge sb)
		    (distance-to-pseudo-distance (near-edge sb) (min-distance sb) p (near-param sb)))
		  
		  (let* ((dr (distance (min-distance sr)))
			 (dg (distance (min-distance sg)))
			 (db (distance (min-distance sb)))
			 (med (median dr dg db))
			 (med-sign (if (> (float-sign med) 0d0) 1 -1)))

		    ;; Note: Use signbit() not sign() here because we need to know -0 case.
		    ;; int medSign = signbit(med) ? -1 : 1;

		    (when *debug-generate-msdf*
		      (format t "[generateMSDF] d*: ~$, ~$, ~$~%" dr dg db)
		      (format t "[generateMSDF] med, med-sign: ~$, ~a~%" med med-sign))
		    
		    (when (/= med-sign real-sign)
		      (setf dr (- dr))
		      (setf dg (- dg))
		      (setf db (- db)))

		    (let ((r (+ (/ dr range) 0.5d0))
			  (g (+ (/ dg range) 0.5d0))
			  (b (+ (/ db range) 0.5d0))
			  (px (aref output (+ (* row w) x)))) ; content[y*w+x]
		      (vector-push r px)
		      (vector-push g px)
		      (vector-push b px)
		      
		      ;; (format t "[generate-msdf] (~a, ~a): ~a, ~a, ~a~%" x row r g b)
		      ;; (sb-ext:exit)
		      
		      t))))))
  
  (when (> edge-threshold 0)
    (correct-msdf-error output (v/ (v* scale range) edge-threshold))))


(defun distance-to-pseudo-distance (edge distance origin param)
  ;; have this return new distance and dot...
  (cond ((< param 0)
	 (let* ((dir (vunit (direction edge 0)))
		(aq (v- origin (point edge 0)))
		(ts (dot-product aq dir)))
	   (when (< ts 0)
	     (let ((pseudo-distance (cross-product aq dir)))
	       (when (<= (abs pseudo-distance) (abs (distance distance)))
		 (setf (distance distance) pseudo-distance)
		 (setf (dot distance) 0))))))
	((> param 1)
	 (let* ((dir (vunit (direction edge 1)))
		(bq (v- origin (point edge 1)))
		(ts (dot-product bq dir)))
	   (when (> ts 0)
	     (let ((pseudo-distance (cross-product bq dir)))
	       (when (<= (abs pseudo-distance) (abs (distance distance)))
		 (setf (distance distance) pseudo-distance)
		 (setf (dot distance) 0))))))))
