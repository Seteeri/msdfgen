(in-package :sdf)

;; /// A utility structure for holding winding spans for a single horizontal scanline.
;; /// First initialize a row by calling collect(), then use advance() to walk the row
;; /// and determine "inside"-ness as you go.
;; struct WindingSpanner: public EdgeSegment::CrossingCallback {
    
;;     std::vector<std::pair<double, int>> crossings;
    
;;     FillRule fillRule;
    
;;     WindingSpanner(): curW(0) {
;;         curSpan = crossings.cend();
;;     }
    
;;     void collect(const Shape& shape, const Point2& p) {
;;         fillRule = shape.fillRule;
;;         crossings.clear();
;;         for (std::vector<Contour>::const_iterator contour = shape.contours.cbegin(); contour != shape.contours.cend(); ++contour) {
;;             for (std::vector<EdgeHolder>::const_iterator e = contour->edges.cbegin(); e != contour->edges.cend(); ++e) {
;;                 (*e)->crossings(p, this);
;;             }
;;         }
        
;;         // Make sure we've collected them all in increasing x order.
;;         std::sort(crossings.begin(), crossings.end(), compareX);
        
;;         // And set up a traversal.
;;         if( fillRule == FillRule::EvenOdd )
;;             curW = 1;
;;         else
;;             curW = 0;
;;         curSpan = crossings.cbegin();
;;     }
    
;;     /// Scan to the provided X coordinate and use the winding rule to return the current sign as either:
;;     /// -1 = pixel is "outside" the shape (i.e. not filled)
;;     /// +1 = pixel is "inside" the shape (i.e. filled)
;;     /// (Note: This is actually the inverse of the final distance field sign.)
;;     int advanceTo(double x) {
;;         while( curSpan != crossings.cend() && x > curSpan->first ) {
;;             curW += curSpan->second;
;;             ++curSpan;
;;         }

;;         switch( fillRule ) {
;;             case FillRule::NonZero:
;;                 return curW != 0 ? 1 : -1;
;;             case FillRule::EvenOdd:
;;                 return curW % 2 == 0 ? 1 : -1;
;;             case FillRule::None:
;;                 return curSpan != crossings.cend() ? sign(curSpan->second) : 0;
;;         }
;;     }

;; private:

;;     int curW;
    
;;     std::vector<std::pair<double, int>>::const_iterator curSpan;
    
;;     void intersection(const Point2& p, int winding) {
;;         crossings.push_back(std::pair<double, int>(p.x, winding));
;;     }

;;     static bool compareX(const std::pair<double,int>& a, std::pair<double,int>& b) {
;;         return a.first < b.first;
;;     }
;; };
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
    
    (iter (for contour in-vector (contours shape))
      (iter (for edge in-vector (edges contour))
        (cross-points edge point (lambda (point winding)
				   ;; (format t "[crossings-callback] ~a,~a~%" (vx2 point) winding)
				   (vector-push-extend (list (vx2 point) winding)
						       (crossings ws))))))

    (format t "[crossings] crossings size: ~a~%" (length (crossings ws)))
    
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

  ;;     int advanceTo(double x) {
  ;;
  ;;         while( curSpan != crossings.cend() && x > curSpan->first ) {
  ;;             curW += curSpan->second;
  ;;             ++curSpan;
  ;;         }
  
  (with-slots (cur-w cur-span crossings) ws

    (format t "[advance-to] x: ~a, crossings length = ~a~%" x (length crossings))    
    (format t "[advance-to] a. cur-w = ~a~%" cur-w)

    (iter (while (and (< cur-span (length crossings))
              (> x (first (aref crossings cur-span)))))
      (for crossing = (aref crossings cur-span))
      (incf cur-w (second crossing))
      (incf cur-span))

    (format t "[advance-to] b. cur-w = ~a~%" cur-w)
    
    ;; when we leave, we need to save current position
    ;; (when (< cur-span (length crossings))
    ;;   (iter (for i from cur-span below (length crossings))
    ;;      (for crossing = (aref crossings i))
        
    ;;      (setf cur-span i)
    ;;      (when (<= (first crossing) x)
    ;;        (leave))

    ;;      (format t "[advance-to] incf ~a~%" (second crossing))
    ;;      (incf cur-w (second crossing))))
    ;; last item so set to length to end it
    ;; ...or use list?
    ;; (when (= cur-span (- (length crossings) 1))
    ;;   (setf cur-span (length crossings)))
    
    ;; (when cur-span
    ;;   (iter (for crossing in-vector crossings)
    ;;      (setf cur-span crossing)
    ;;      (when (<= x (first cur-span))
    ;;        (leave))
        
    ;;      (format t "[advance-to] incf ~a~%" (second cur-span))
    ;;      (incf cur-w (second cur-span))
        
    ;;      (format t "[advance-to] i. cur-w = ~a~%" cur-w)))

    ;; (format t "[advance-to] b. cur-w = ~a~%" cur-w)

    ;;         switch( fillRule ) {
    ;;             case FillRule::NonZero:
    ;;                 return curW != 0 ? 1 : -1;
    ;;             case FillRule::EvenOdd:
    ;;                 return curW % 2 == 0 ? 1 : -1;
    ;;             case FillRule::None:
    ;;                 return curSpan != crossings.cend() ? sign(curSpan->second) : 0;
    ;;         }
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
