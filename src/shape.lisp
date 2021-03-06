(in-package :sdf)

(defclass shape ()
  ((contours
    :accessor contours
    :initarg :contours
    :initform (make-array 0 :fill-pointer 0 :adjustable t)
    :documentation "")
   (inverse-y-axis
    :accessor inverse-y-axis
    :initarg :inverse-y-axis
    :initform nil
    :documentation "")
   (fill-rule
    :accessor fill-rule
    :initarg :fill-rule
    :initform +fill-rule-non-zero+
    :documentation "")))

;; Fill rules compatible with SVG: https://www.w3.org/TR/SVG/painting.html#FillRuleProperty
(defconstant +fill-rule-none+ 0) ; legacy
(defconstant +fill-rule-non-zero+ 1)
(defconstant +fill-rule-even-odd+ 2)

(defmethod add-contour ((shape shape))
  (let ((contour (make-instance 'contour)))
    (vector-push-extend contour
			(contours shape))
    contour))

;; Edge color specifies which color channels an edge belongs to.
(defconstant +black+ 0)
(defconstant +red+ 1)
(defconstant +green+ 2)
(defconstant +yellow+ 3)
(defconstant +blue+ 4)
(defconstant +magenta+ 5)
(defconstant +cyan+ 6)
(defconstant +white+ 7)
;; number to symbol
(defun index-edge-color (i)
  (aref #(+black+ +red+ +green+ +yellow+ +blue+ +magenta+ +cyan+ +white+) i))

(defun edge-coloring-simple (shape angle-threshold &optional (seed 0))
  
  (let ((cross-threshold (sin angle-threshold)))

    (when *debug-edge-coloring-simple*
      (format t "[edge-coloring-simple] contours: ~a~%" (length (contours shape))))
    
    (iter (for contour in-vector (contours shape))

	  (when *debug-edge-coloring-simple*
	    (format t "[edge-coloring-simple] ======================~%")
	    (format t "[edge-coloring-simple] contour: ~a~%" contour))
	  
	  ;; clear corners for each iteration
	  (let ((corners (make-array 0 :fill-pointer 0 :adjustable t))
		(edges (edges contour)))

	    (when *debug-edge-coloring-simple*
	      (format t "[edge-coloring-simple] length of edges: ~a~%" (length edges)))
	    
	    (when (/= (length edges) 0)
	      (let ((prev-direction (direction (aref edges (- (length edges) 1))
					       1)) ; last edge direction
		    (index 0))
		
		(when *debug-edge-coloring-simple*
		  (format t "[edge-coloring-simple] prev-direction: ~a~%" prev-direction)
		  (format t "[edge-coloring-simple] -------------------~%"))
		
		(iter (for edge in-vector edges)
		      (when *debug-edge-coloring-simple*
			(format t "[edge-coloring-simple] index: ~a~%" index)
			(format t "                       arg1: ~a~%" (vunit prev-direction))
			(format t "                       arg2: ~a~%" (vunit (direction edge 0))))
		      (when (is-corner (vunit prev-direction)
				       (vunit (direction edge 0))
				       cross-threshold)
			(when *debug-edge-coloring-simple*
			  (format t "                       vector-push-extend: ~a~%" index))
			(vector-push-extend index corners))
		      (when *debug-edge-coloring-simple*
			(format t "[edge-coloring-simple] -------------------~%" prev-direction))
		      (setf prev-direction (direction edge 1))
		      (incf index))))

	    ;; (sb-ext:exit)
	    
	    (when *debug-edge-coloring-simple*
	      (format t "[edge-coloring-simple] length of corners: ~a~%" (length corners)))

	    ;; smooth contours
	    (cond ((= (length corners) 0)
		   (iter (for edge in-vector edges)
			 (setf (color edge) +white+)))
		  
		  ;; teardrop
		  ((= (length corners) 1)
		   (let ((colors (list +white+
				       +white+
				       +black+))
			 (corner (aref corners 0)))
		     (setf (first colors) (switch-color (first colors) seed))
		     (setf (third colors) (first colors))
		     (setf (third colors) (switch-color (third colors) seed))

		     (when *debug-edge-coloring-simple*
		       (format t "[edge-coloring-simple] length edges: ~a~%" (length edges)))

		     ;; contour->edges[(corner+i)%m]->color = (colors+1)[int(3+2.875*i/(m-1)-1.4375+.5)-3];
		     ;; ((3+(2.875*i/(m-1)))-1.4375)+.5
		     ;; (+ (- (+ 3 (/ (* 2.875 i) (- m 1))) 1.4375) 0.5)
		     ;; (- (floor t) 3)
		     ;; (- (floor (+ (- (+ 3 (/ (* 2.875 i) (- m 1))) 1.4375) 0.5)) 3)
		     (cond ((>= (length edges) 3)
			    (let ((m (length edges)))
			      (iter (for i from 0 below m) ; ++i
				    (for n = (+ 1 (- (floor (+ (- (+ 3 (/ (* 2.875 i) (- m 1))) 1.4375) 0.5)) 3)))
				    ;; (format t "[edgeColoringSimple] ~a: ~a, ~a, ~a~%"
				    ;; 	    i
				    ;; 	    (mod (+ corner i) m)
				    ;; 	    (- (floor (+ (- (+ 3 (/ (* 2.875 i) (- m 1))) 1.4375) 0.5)) 3)
				    ;; 	    n)
				    (setf (color (aref edges (mod (+ corner i) m)))
					  (nth n colors)))))
			   ;; (iter (for i from 0 below m)
			   ;; 	    (for ed = (aref edges i))
			   ;; 	    (format t "~a, ~a~%" ed (color ed)))
			   ((>= (length edges) 1)
			    (let ((parts (make-array 7 :initial-contents (list nil nil nil nil nil nil nil))))
			      (multiple-value-bind (e0-0 e0-1 e0-2) (split-in-thirds (aref edges 0))
				;; (format t "~a~%" parts)
				;; (format t "~a, ~a, ~a~%" e0-0 e0-1 e0-2)
				(setf (aref parts (+ 0 (* 3 corner))) e0-0)
				(setf (aref parts (+ 1 (* 3 corner))) e0-1)
				(setf (aref parts (+ 2 (* 3 corner))) e0-2)
				
				(if (>= (length edges) 2)
				    (multiple-value-bind (e1-0 e1-1 e1-2) (split-in-thirds (aref edges 1))
				      ;; (format t "~a~%" parts)
				      ;; (format t "~a, ~a, ~a~%" e1-0 e0-1 e1-2)
				      (setf (aref parts (- 3 (* 3 corner))) e1-0)
				      (setf (aref parts (- 4 (* 3 corner))) e1-1)
				      (setf (aref parts (- 5 (* 3 corner))) e1-2)
				      
				      (setf (color (aref parts 0)) (nth 0 colors))
				      (setf (color (aref parts 1)) (nth 0 colors))
				      
				      (setf (color (aref parts 2)) (nth 1 colors))
				      (setf (color (aref parts 3)) (nth 1 colors))

				      (setf (color (aref parts 4)) (nth 2 colors))
				      (setf (color (aref parts 5)) (nth 2 colors)))
				    (progn
				      (setf (color (aref parts 0)) (nth 0 colors))
				      (setf (color (aref parts 1)) (nth 1 colors))
				      (setf (color (aref parts 2)) (nth 2 colors))))
				
				;; double check this...
				(setf (fill-pointer edges) 0)
				(adjust-array edges 0)
				;; (iter (for i from 0 below 7)
				;; 	 (when (aref parts i)
				;; 	   (vector-push-extend (make-instance 'edge-holder (aref parts i))
				;; 			       edges))))))))
				;; don't need an edgeholder class (dynamic container) in lisp
				(iter (for i from 0 below 7)
				      (when (aref parts i)
					(vector-push-extend (aref parts i)
							    edges)))))))))
		  
		  ;; multiple corners
		  (t
		   (let* ((corner-count (length corners))
			  (spline 0)
			  (start (aref corners 0))
			  (m (length edges))
			  (color (switch-color +white+ seed))
			  (initial-color color))
		     
		     (iter (for i from 0 below m)
			   (let ((index (mod (+ start i) m)))

			     ;; (format t "[edge-coloring-simple] index: ~a, spline: ~a~%" index spline)
			     
			     (when (and (< (+ spline 1) corner-count)
					(= (aref corners (+ spline 1)) index))

			       (incf spline)
			       
			       ;; check this
			       ;; (format t "[edge-coloring-simple] switch color: ~a, ~a~%" color (* 1 initial-color))
			       (multiple-value-bind (new-color new-seed) (switch-color color
										       seed
										       (* (if (= spline (- corner-count 1)) 1 0)
											  initial-color))
				 (setf color new-color)
				 (setf seed new-seed)))

			     ;; (format t "[edge-coloring-simple] ~a. final color: ~a~%~%" i color)
			     (setf (color (aref edges index)) color))))))))))

(defun is-corner (a-dir b-dir cross-threshold)
  (or (< (v. a-dir b-dir) 0)
      (> (abs (cross-product a-dir b-dir)) cross-threshold)))

(defun switch-color (color seed &optional (banned +black+))
  (let ((combined (logand color banned)))
    ;; (format t "  Input: color: ~a, banned: ~a, combo: ~a~%" color banned combined)
    (when (or (= combined +red+)
	      (= combined +green+)
	      (= combined +blue+))
      ;; (format t "  Output: 1. ~a~%" (logxor combined +white+))
      (return-from switch-color (values (logxor combined +white+)
					seed)))
    (when (or (= color +black+)
	      (= color +white+))
      (let ((start (list +cyan+ +magenta+ +yellow+)))
	;; (format t "  Output: 2. ~a~%" (nth (mod seed 3) start))
	(return-from switch-color (values (nth (mod seed 3) start)
					  (/ seed 3)))))
    (let ((shifted (ash color (+ 1 (logand seed 1)))))
      ;; (format t "  Output: 3. shifted: ~a, color: ~a, seed: ~a~%"
      ;;          shifted
      ;;          (logand (logior shifted (ash shifted (- 3))) +white+)
      ;;          (ash seed (- 1)))
      (values (logand (logior shifted (ash shifted (- 3))) +white+)
	      (ash seed (- 1))))))

(defmethod bounds ((shape shape) left bottom right top)
  (iter (for contour in-vector (contours shape))
	(when *debug-bounds*
	  (format t "[bounds:shape] iterating~%"))
	(multiple-value-bind (left-1 bottom-1 right-1 top-1) (bounds contour left bottom right top)
	  (setf left left-1)
	  (setf bottom bottom-1)
	  (setf right right-1)
	  (setf top top-1)))
  (values left bottom right top))
