(in-package :sdf)

;; /// A utility structure for holding winding spans for a single horizontal scanline.
;; /// First initialize a row by calling collect(), then use advance() to walk the row
;; /// and determine "inside"-ness as you go.
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

    (format t "[collect-crossings] contours: ~a~%" (length (contours shape)))
    
    (iter (for contour in-vector (contours shape))
	  (format t "[collect-crossings] ~a, edges: ~a~%" contour (length (edges contour)))
	  (iter (for edge in-vector (edges contour))
		;; (format t "[collect-crossings] ~a~%" edge)
		(cross-points edge point (lambda (point winding)
					   (vector-push-extend (list (vx2 point) winding)
							       (crossings ws))))))

    (format t "[collect-crossings] crossings size: ~a~%" (length (crossings ws)))
    
    ;; Make sure we've collected them all in increasing x order.
    (sort (crossings ws) (lambda (a b)
			   (< (first a) (first b))))
    
    (setf (cur-w ws) (if (= fill-rule +fill-rule-even-odd+) 1 0))

    ;; keep track of iteration
    (setf (cur-span ws) 0)))

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
      (format t "[advance-to] x: ~a, crossings length = ~a~%" x (length crossings))    
      (format t "[advance-to] a. cur-w = ~a~%" cur-w))

    (iter (while (and (< cur-span (length crossings))
		      (> x (first (aref crossings cur-span)))))
	  (for crossing = (aref crossings cur-span))
	  (incf cur-w (second crossing))
	  (incf cur-span))

    (when *debug-advance-to*
      (format t "[advance-to] b. cur-w = ~a~%" cur-w))
    
    (cond ((= (fill-rule ws) +fill-rule-non-zero+)
	   (return-from advance-to (if (/= cur-w 0) 1 -1)))
	  ((= (fill-rule ws) +fill-rule-even-odd+)
	   (return-from advance-to (if (== (mod cur-w 2) 0) 1 -1)))
	  ((= (fill-rule ws) +fill-rule-none+)
	   (if cur-span
	       (return-from advance-to (if (not (eq cur-span
						    (aref crossings (- (length crossings) 1))))
					   (sign (second cur-span))
					   0))
	       (return-from advance-to 0))))))

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
