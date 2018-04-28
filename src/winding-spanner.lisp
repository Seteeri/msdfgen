(in-package :sdf)

;; A utility structure for holding winding spans for a single horizontal scanline.
;; First initialize a row by calling collect(), then use advance() to walk the row
;; and determine "inside"-ness as you go.
(defclass winding-spanner ()
  ((crossings
    :accessor crossings
    :initarg :crossings
    :initform (make-array 0 :fill-pointer 0 :adjustable t)
    :documentation "")
   (fill-rule
    :accessor fill-rule
    :initarg :fill-rule
    :initform nil
    :documentation "")
   (cur-w
    :accessor cur-w
    :initarg :cur-w
    :initform nil
    :documentation "")
   (cur-span
    :accessor cur-span
    :initarg :cur-span
    :initform 0
    :documentation "")))

(defun collect-crossings (ws shape point)
  
  (setf (fill-rule ws) (fill-rule shape))

  (let ((fill-rule (fill-rule ws)))
    
    (setf (fill-pointer (crossings ws)) 0)
    (adjust-array (crossings ws) 0)

    (when *debug-collect-crossings*
      (format t "[collect-crossings] contours: ~a~%" (length (contours shape))))
    
    (iter (for contour in-vector (contours shape))
	  (when *debug-collect-crossings*
	    (format t "[collect-crossings] ~a, edges: ~a~%" contour (length (edges contour))))
	  (iter (for edge in-vector (edges contour))
		(cross-points edge point (lambda (point winding)
					   ;; (format t "[collect-crossings:lambda] ~a, ~6$, ~a~%" edge (vx2 point) winding)
					   (vector-push-extend (list (vx2 point) winding)
							       (crossings ws))))))

    (when *debug-collect-crossings*
      (format t "[collect-crossings] crossings size: ~a~%" (length (crossings ws))))
    
    ;; Make sure we've collected them all in increasing x order.
    (sort (crossings ws) (lambda (a b)
			   (< (first a) (first b))))
    
    (setf (cur-w ws) (if (= fill-rule +fill-rule-even-odd+) 1 0))

    ;; keep track of iteration
    ;; if there are crossings set to first
    (if (> (length (crossings ws)) 0)
	(setf (cur-span ws) 0)
	(setf (cur-span ws) nil))))

(defun advance-to (ws x)
  ;; Scan to the provided X coordinate and use the winding rule to return the current sign as either:
  ;; -1 = pixel is "outside" the shape (i.e. not filled)
  ;; +1 = pixel is "inside" the shape (i.e. filled)
  ;; (Note: This is actually the inverse of the final distance field sign.)

  ;; while( curSpan != crossings.cend() && x > curSpan->first )
  ;; {
  ;;     curW += curSpan->second;
  ;;     ++curSpan;
  ;; }
  
  (with-slots (cur-w cur-span crossings) ws

    (when *debug-advance-to*
      (format t "[advance-to] x: ~6$, crossings length = ~a~%" x (length crossings))    
      (format t "[advance-to] a. cur-w = ~a, cur-span = ~a~%" cur-w cur-span))

    (when cur-span

      (when *debug-advance-to*
	(format t "[advance-to] a. cur-span = ~a~%"
		(aref crossings cur-span)))

      (iter (while (and (< cur-span (length crossings))
			(> x (first (aref crossings cur-span)))))
	    (for crossing = (aref crossings cur-span))
	    (when *debug-advance-to*
	      (format t "[advance-to] i. cur-w = ~a~%" cur-w))
	    (incf cur-w (second crossing))
	    (when *debug-advance-to*
	      (format t "[advance-to] i. incf cur-w = ~a~%" cur-w))
	    (incf cur-span)
	    (when *debug-advance-to*
	      (format t "[advance-to] i. incf cur-span = ~a~%" cur-span))))

    (when *debug-advance-to*
      (format t "[advance-to] b. cur-w = ~a~%" cur-w))

    ;; use (length crossings) == vector.end()
    (cond ((= (fill-rule ws) +fill-rule-non-zero+) 
	   (return-from advance-to (if (/= cur-w 0) 1 -1)))
	  
	  ((= (fill-rule ws) +fill-rule-even-odd+)
	   (return-from advance-to (if (== (mod cur-w 2) 0) 1 -1)))
	  
	  ((= (fill-rule ws) +fill-rule-none+)
	   (return-from advance-to (if (not (= cur-span (length crossings)))
				       (sign (second (aref crossings cur-span)))
				       0))))))


;; switch( fillRule ) {
;;     case FillRule::NonZero:
;;         return curW != 0 ? 1 : -1;
;;     case FillRule::EvenOdd:
;;         return curW % 2 == 0 ? 1 : -1;
;;     case FillRule::None:
;;         return curSpan != crossings.cend() ? sign(curSpan->second) : 0;
;; }

;; /// Returns 1 for positive values, -1 for negative values, and 0 for zero.
;; template <typename T>
;; inline int sign(T n) {
;;     return (T(0) < n)-(n < T(0));
;; }
(defun sign (n)
  (cond ((> n 0)
	 1)
	((< n 0)
	 -1)
	((= n 0)
	 0)))
