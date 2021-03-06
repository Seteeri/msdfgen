(in-package :sdf)

;; TODO
;; - refactor/cleanup
;; - implement renderSDF

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


(defun main ()

  ;; (format t "~a~%" (freetype2:check-font-file "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf"))
  
  (let ((face (freetype2:new-face "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf")))
    ;; check face if null
    
    ;; printable asii printable characters range including extended
    (iter (for code from 32 to 255)
    ;; (let ((code 38))
	  
	  (let* ((shape (load-glyph face code))
		 (width 32)
		 (height 32)
		 (bitmap (make-array (* width height) :fill-pointer 0))
		 (scale (vec2 1.0 1.0))
		 (translation (vec2 4.0 4.0))
		 (edge-threshold 1.00000001d0)
		 (range 4.0d0))
	    
	    (normalize (contours shape))
	    
	    (edge-coloring-simple shape 3.0)
	    
	    (iter (for i from 0 below (* width height))
		  (vector-push (make-array 3 :fill-pointer 0) bitmap))
	    
	    (generate-msdf bitmap
			   width height
			   shape
			   scale
			   translation
			   range)

	    (correct-msdf-error bitmap
				width height
				(v/ edge-threshold (v* scale range)))

	    (when nil
	      (write-raw-buffer-to-file #p"/home/user/font-gen/sdf/output.txt" bitmap width height)
	      (format t "Wrote /home/user/font-gen/sdf/output.txt"))
	    
	    (write-rgb-buffer-to-ppm-file (format nil "/home/user/font-gen/sdf/~a.ppm" code)
					  bitmap width height)
	    (format t "Wrote ~a~%" (format nil "/home/user/font-gen/sdf/~a.ppm" code)))))

  ;; (compare-ppm-files)
  
  (sb-ext:exit))

(defun write-raw-buffer-to-file (filepath bitmap width height)
  (with-open-file (out filepath
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede)
    (iter (for y from (- height 1) downto 0)
	  (iter (for x from 0 below width)
		(for px = (get-pixel bitmap x y width))
		(write-line (format nil "(~a, ~a) ~5$ ~5$ ~5$" x y (aref px 0) (aref px 1) (aref px 2))
			    out)))))


(defun pixel-clash (a b threshold)

  (when *debug-correct-msdf-error*
    (format t "a: ~a~%" a)
    (format t "b: ~a~%" b)
    (format t "...~%"))

  (let ((a-in (>= (+ (if (> (aref a +px-red+) 0.5) 1 +px-red+)
		     (if (> (aref a +px-green+) 0.5) 1 +px-red+)
		     (if (> (aref a +px-blue+) 0.5) 1 +px-red+))
		  +px-blue+))
	(b-in (>= (+ (if (> (aref b +px-red+) 0.5) 1 +px-red+)
		     (if (> (aref b +px-green+) 0.5) 1 +px-red+)
		     (if (> (aref b +px-blue+) 0.5) 1 +px-red+))
		  +px-blue+))
	(pr t))

    (when (not (eq a-in b-in))
      (return-from pixel-clash))
    
    ;; If the change is 0 <-> 1 or 2 <-> 3 channels and not 1 <-> 1 or 2 <-> 2, it is not a clash
    (when (or (and (> (aref a +px-red+) 0.5) (> (aref a +px-green+) 0.5) (> (aref a +px-blue+) 0.5))
	      (and (< (aref a +px-red+) 0.5) (< (aref a +px-green+) 0.5) (< (aref a +px-blue+) 0.5))
	      (and (> (aref b +px-red+) 0.5) (> (aref b +px-green+) 0.5) (> (aref b +px-blue+) 0.5))
	      (and (< (aref b +px-red+) 0.5) (< (aref b +px-green+) 0.5) (< (aref b +px-blue+) 0.5)))
      (return-from pixel-clash))

    ;; If the change is 0 <-> 1 or 2 <-> 3 channels and not 1 <-> 1 or 2 <-> 2, it is not a clash
    (when (or (and (> (aref a +px-red+) 0.5) (> (aref a +px-green+) 0.5) (> (aref a +px-blue+) 0.5))
	      (and (< (aref a +px-red+) 0.5) (< (aref a +px-green+) 0.5) (< (aref a +px-blue+) 0.5))
	      (and (> (aref b +px-red+) 0.5) (> (aref b +px-green+) 0.5) (> (aref b +px-blue+) 0.5))
	      (and (< (aref b +px-red+) 0.5) (< (aref b +px-green+) 0.5) (< (aref b +px-blue+) 0.5)))
      (return-from pixel-clash))
    
    ;; Find which color is which: _a, _b = the changing channels, _c = the remaining one
    ;;     float aa, ab, ba, bb, ac, bc;
    (let ((aa 0.0)
	  (ab 0.0)
	  (ba 0.0)
	  (bb 0.0)
	  (ac 0.0)
	  (bc 0.0))
      
      (cond ((and (not (eq (> (aref a +px-red+) 0.5) (> (aref b +px-red+) 0.5)))
		  (not (eq (< (aref a +px-red+) 0.5) (< (aref b +px-red+) 0.5))))
	     (setf aa (aref a +px-red+))
	     (setf ba (aref b +px-red+))

	     (when *debug-correct-msdf-error*
	       (format t "    [p-c] threshold = ~a~%" threshold)
	       (format t "    [p-c] aa = ~a~%" aa)
	       (format t "    [p-c] ba = ~a~%" ba)
	       (format t "    [p-c] ab = ~a~%" ab)
	       (format t "    [p-c] bb = ~a~%" bb)
	       (format t "    [p-c] ac = ~a~%" ac)
	       (format t "    [p-c] bc = ~a~%" bc))
	     
	     (cond ((and (not (eq (> (aref a +px-green+) 0.5) (> (aref b +px-green+) 0.5)))
			 (not (eq (< (aref a +px-green+) 0.5) (< (aref b +px-green+) 0.5))))
		    (setf ab (aref a +px-green+))
		    (setf bb (aref b +px-green+))
		    (setf ac (aref a +px-blue+))
		    (setf bc (aref b +px-blue+)))
		   ((and (not (eq (> (aref a +px-blue+) 0.5) (> (aref b +px-blue+) 0.5)))
			 (not (eq (< (aref a +px-blue+) 0.5) (< (aref b +px-blue+) 0.5))))
		    (setf ab (aref a +px-blue+))
		    (setf bb (aref b +px-blue+))
		    (setf ac (aref a +px-green+))
		    (setf bc (aref b +px-green+)))
		   (t
		    (warn "pixel-clash: this should never happen")
		    (return-from pixel-clash))))
	    ((and (not (eq (> (aref a +px-green+) 0.5) (> (aref b +px-green+) 0.5)))
		  (not (eq (< (aref a +px-green+) 0.5) (< (aref b +px-green+) 0.5)))
		  (not (eq (> (aref a +px-blue+) 0.5) (> (aref b +px-blue+) 0.5)))
		  (not (eq (< (aref a +px-blue+) 0.5) (< (aref b +px-blue+) 0.5))))
	     (setf aa (aref a +px-green+))
	     (setf ba (aref b +px-green+))
	     (setf ab (aref a +px-blue+))
	     (setf bb (aref b +px-blue+))
	     (setf ac (aref a +px-red+))
	     (setf bc (aref b +px-red+)))
	    (t
	     (return-from pixel-clash)))

      (when *debug-correct-msdf-error*
	(format t "    [p-c] threshold = ~a~%" threshold)
	(format t "    [p-c] aa = ~a~%" aa)
	(format t "    [p-c] ba = ~a~%" ba)
	(format t "    [p-c] ab = ~a~%" ab)
	(format t "    [p-c] bb = ~a~%" bb)
	(format t "    [p-c] ac = ~a~%" ac)
	(format t "    [p-c] bc = ~a~%" bc))
      
      (let ((r (and (>= (abs (- aa ba)) threshold)
		    (>= (abs (- ab bb)) threshold)
		    (>= (abs (- ac 0.5)) (abs (- bc 0.5))))))
	(when *debug-correct-msdf-error*
	  (when pr (format t "5. ~a, ~a, r=~a~%" a-in b-in r)))
	(return-from pixel-clash r)))))

(defun correct-msdf-error (output w h threshold)
  (let ((clashes (make-array 0 :fill-pointer 0 :adjustable t)))
    (iter (for y from 0 below h)
	  (iter (for x from 0 below w)
		(for pixel = (get-pixel output x y w))
		
		(when (and *debug-correct-msdf-error* (= x 5) (= y 6))
		  (format t "[c-m-e] (~a, ~a) ----------------------~%" x y)
		  (format t "(5,6): ~a~%~%" (get-pixel output 5 6 w))
		  
		  (format t "(~a,~a): ~a~%" (- x 1) y (get-pixel output (- x 1) y w))

		  (pixel-clash pixel
			       (get-pixel output (- x 1) y w)
			       (vx2 threshold))		  
		  
		  (format t "(~a,~a): ~a~%" (+ x 1) y (get-pixel output (+ x 1) y w))

		  (pixel-clash pixel
			       (get-pixel output (+ x 1) y w)
			       (vx2 threshold))

		  (format t "(~a,~a): ~a~%" x (- y 1) (get-pixel output x (- y 1) w))

		  (pixel-clash pixel
			       (get-pixel output x (- y 1) w) ; 5, (- 6 1) = 5,5 ; 6,6
			       (vy2 threshold))

		  (format t "(~a,~a): ~a~%" x (+ y 1) (get-pixel output x (+ y 1) w))
		  
		  (pixel-clash pixel
			       (get-pixel output x (+ y 1) w)
			       (vy2 threshold)))
		
		(when (or (and (> x 0)
			       (pixel-clash pixel
					    (get-pixel output (- x 1) y w)
					    (vx2 threshold)))
			  (and (< x (- w 1))
			       (pixel-clash pixel
					    (get-pixel output (+ x 1) y w) ; (6, 6)
					    (vx2 threshold)))
			  (and (> y 0)
			       (pixel-clash pixel
					    (get-pixel output x (- y 1) w)
					    (vy2 threshold)))
			  (and (< y (- h 1))
			       (pixel-clash pixel
					    (get-pixel output x (+ y 1) w)
					    (vy2 threshold))))
		  (when *debug-correct-msdf-error*
		    (format t "[c-m-e] push: ~a~%" (list x y)))
		  (vector-push-extend (make-array 2 :initial-contents (list x y))
				      clashes))
		(when (and *debug-correct-msdf-error* nil)
		  (when (and (= x 12) (= y 11)) (sb-ext:exit)))))
    
    (format t "[correct-msdf-error] clashes length: ~a~%" (length clashes))

    (iter (for clash in-vector clashes)
	  (let* ((pixel (get-pixel output (aref clash 0) (aref clash 1) w))
		 (med (median (aref pixel 0) (aref pixel 1) (aref pixel 2))))
	    ;; (format t "[correct-msdf-error] (~a, ~a) ~6$~%" (aref clash 0) (aref clash 1) med)
	    (setf (aref pixel 0) med)
	    (setf (aref pixel 1) med)
	    (setf (aref pixel 2) med)))))

(defun generate-msdf (bitmap w h shape scale translate range)
  (let* ((contour-count (length (contours shape)))
	 (spanner (make-instance 'winding-spanner))
	 (contour-sd (make-array contour-count :fill-pointer 0 :adjustable t)))

    (iter (for y from 0 below h)
	  (let ((row (if (inverse-y-axis shape)
			 (- h y 1)
			 y)))

	    (when *debug-generate-msdf*
	      (format t "[generate-msdf] call bounds~%"))

	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    
	    (multiple-value-bind (bound-left bound-bottom bound-right bound-top) (bounds shape 0.0 0.0 0.0 0.0)
	      (when *debug-generate-msdf*
		(format t "~%[generate-msdf] collect-crossings: y, row: ~a, ~a~%" y row))
	      (collect-crossings spanner
				 shape
				 (vec2 (- bound-left 0.5)
				       (- (/ (+ y 0.5) (vy2 scale)) (vy2 translate)))))
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	    (when *debug-generate-msdf*
	      (format t "[generate-msdf] call bounds done~%"))
	    
	    (iter (for x from 0 below w)
		  (for foobar = (if *debug-generate-msdf* (format t "[generate-msdf] (x,y) = (~a,~a)~%" x row) nil))
		  (for p = (v- (v/ (vec2 (+ x 0.5d0) (+ y 0.5d0))
				   scale)
			       translate))
		  (for sr = (make-instance 'edge-point))
		  (for sg = (make-instance 'edge-point))
		  (for sb = (make-instance 'edge-point))
		  (for real-sign = (advance-to spanner (vx2 p)))

		  (when *debug-generate-msdf*
		    (format t "[generate-msdf] real-sign: ~a, p: ~4$, ~4$~%" real-sign (vx2 p) (vy2 p)))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  
		  (iter (for contour in-vector (contours shape))
			(for r = (make-instance 'edge-point))
			(for g = (make-instance 'edge-point))
			(for b = (make-instance 'edge-point))
			(for ii = 0)

			(when *debug-generate-msdf*
			  (format t " [generate-msdf] length edges: ~a~%" (length (edges contour))))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  
			
			(iter (for edge in-vector (edges contour))
			      ;; signed-distance - in edges
			      ;; fix nvunit -> vunit

			      (when *debug-generate-msdf*
				(format t " [generate-msdf] -----------------~%"))
			      
			      (multiple-value-bind (distance param) (signed-distance edge p)

				(when *debug-generate-msdf*
				  ;; (format t "  [generateMSDF][e#~a] ~a~%" ii edge)
				  (format t "  [generateMSDF][e#~a] dist: ~4$, dot: ~4$~%"
				  	  ii
				  	  (distance distance)
				  	  (dot distance))
				  (format t "                       param: ~4$~%"
				  	  param)
				  )

				;; (when *debug-generate-msdf*
				;;   (format t " [generateMSDF][e#~a] b| color edge: ~a, logand: ~a~%"
				;;   	  ii (color edge) (logand (color edge) +green+))
				;;   (format t " [generateMSDF][e#~a] g| color edge: ~a, logand: ~a~%"
				;; 	  ii (color edge) (logand (color edge) +blue+)))

				(when (and *debug-generate-msdf* t)
				  (format t "  [generateMSDF][e#~a] r dist, dot: ~6$, ~6$~%"
					  ii
					  (distance (min-distance r))
					  (dot (min-distance r)))
				  (format t "  [generateMSDF][e#~a] g dist, dot: ~6$, ~6$~%"
					  ii
					  (distance (min-distance g))
					  (dot (min-distance g)))
				  (format t "  [generateMSDF][e#~a] b dist, dot: ~6$, ~6$~%~%"
					  ii
					  (distance (min-distance b))
					  (dot (min-distance b))))

				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
				(when (and (not (= (logand (color edge) +red+) 0))
					   (sd< distance (min-distance r)))
				  (when (and *debug-generate-msdf* t)
				    (format t "  [generateMSDF][e#~a] r set~%" ii))
				  (setf (min-distance r) distance)
				  (setf (near-edge r) edge)
				  (setf (near-param r) param))

				(when (and (not (= (logand (color edge) +green+) 0))
					   (sd< distance (min-distance g)))
				  (when (and *debug-generate-msdf* t)
				    (format t "  [generateMSDF][e#~a] g set~%" ii))
				  (setf (min-distance g) distance)
				  (setf (near-edge g) edge)
				  (setf (near-param g) param))
				
				(when (and (not (= (logand (color edge) +blue+) 0))
					   (sd< distance (min-distance b)))
				  (when (and *debug-generate-msdf* t)
				    (format t "  [generateMSDF][e#~a] b set~%" ii))
				  (setf (min-distance b) distance)
				  (setf (near-edge b) edge)
				  (setf (near-param b) param))

				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
				(when (and *debug-generate-msdf* t)
				  (format t "~%  [generateMSDF][e#~a] r dist, dot: ~6$, ~6$~%"
					  ii
					  (distance (min-distance r))
					  (dot (min-distance r)))
				  (format t "  [generateMSDF][e#~a] g dist, dot: ~6$, ~6$~%"
					  ii
					  (distance (min-distance g))
					  (dot (min-distance g)))
				  (format t "  [generateMSDF][e#~a] b dist, dot: ~6$, ~6$~%~%"
					  ii
					  (distance (min-distance b))
					  (dot (min-distance b))))

				(when *debug-generate-msdf*
				  (when (and (= x 7) (= row 4) (= ii 15) (eq nil t))
				    (sb-ext:exit)))

				(incf ii)

				t))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			
			(when (sd< (min-distance r)
				   (min-distance sr))
			  (setf sr r))
			(when (sd< (min-distance g)
				   (min-distance sg))
			  (setf sg g))
			(when (sd< (min-distance b)
				   (min-distance sb))
			  (setf sb b)))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  
		  (when (near-edge sr)
		    (distance-to-pseudo-distance (near-edge sr) (min-distance sr) p (near-param sr)))
		  (when (near-edge sg)
		    (distance-to-pseudo-distance (near-edge sg) (min-distance sg) p (near-param sg)))
		  (when (near-edge sb)
		    (distance-to-pseudo-distance (near-edge sb) (min-distance sb) p (near-param sb)))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  
		  (when *debug-generate-msdf*
		    (format t "  [generateMSDF][(~a,~a)] sr dist, dot: ~6$, ~6$~%"
			    x row
			    (distance (min-distance sr))
			    (dot (min-distance sr)))
		    (format t "  [generateMSDF][(~a,~a)] sg dist, dot: ~6$, ~6$~%"
			    x row
			    (distance (min-distance sg))
			    (dot (min-distance sg)))
		    (format t "  [generateMSDF][(~a,~a)] sb dist, dot: ~6$, ~6$~%"
			    x row
			    (distance (min-distance sb))
			    (dot (min-distance sb))))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  
		  (let* ((dr (distance (min-distance sr)))
			 (dg (distance (min-distance sg)))
			 (db (distance (min-distance sb))))

		    (when *debug-generate-msdf*
		      (format t "float-sign: ~a, real-sign: ~a, median: ~6$~%"
			      (float-sign (median dr dg db))
			      real-sign
			      (median dr dg db)))
		    
		    ;; Note: Use signbit() not sign() here because we need to know -0 case.
		    ;; int medSign = signbit(med) ? -1 : 1;
		    (when (/= (if (> (float-sign (median dr dg db)) 0)
				  1
				  -1) ; med-sign
			      real-sign)
		      (setf dr (- dr))
		      (setf dg (- dg))
		      (setf db (- db)))
		    
		    (let ((r (+ (/ dr range) 0.5d0))
			  (g (+ (/ dg range) 0.5d0))
			  (b (+ (/ db range) 0.5d0))
			  (px (get-pixel bitmap x row w)))

		      ;; (when *debug-generate-msdf*
		      ;; 	(format t "[generateMSDF] d*: ~4$, ~4$, ~4$~%" dr dg db)
		      ;; 	(format t "[generateMSDF] med, med-sign: ~4$, ~a~%" med med-sign))

		      ;; (format t "~6$~%"  (/ dr range))
		      
		      (vector-push r px)
		      (vector-push g px)
		      (vector-push b px)

		      (when (or *debug-generate-msdf* nil)
			(format t "[generate-msdf] (~a, ~a): ~6$, ~6$, ~6$~%" x row r g b))

		      
		      (when nil
		      	(when (and (= x 14) (= row 5))
		      	  (sb-ext:exit)))
		      
		      t)))))))


(defun distance-to-pseudo-distance (edge distance origin param)
  ;; have this return new distance and dot...
  (cond ((< param 0)
	 (let* ((dir (vunit (direction edge 0)))
		(aq (v- origin (point edge 0)))
		(ts (v. aq dir)))
	   (when (< ts 0)
	     (let ((pseudo-distance (cross-product aq dir)))
	       (when (<= (abs pseudo-distance) (abs (distance distance)))
		 (setf (distance distance) pseudo-distance)
		 (setf (dot distance) 0))))))
	((> param 1)
	 (let* ((dir (vunit (direction edge 1)))
		(bq (v- origin (point edge 1)))
		(ts (v. bq dir)))
	   (when (> ts 0)
	     (let ((pseudo-distance (cross-product bq dir)))
	       (when (<= (abs pseudo-distance) (abs (distance distance)))
		 (setf (distance distance) pseudo-distance)
		 (setf (dot distance) 0))))))))


