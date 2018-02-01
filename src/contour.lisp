(in-package :sdf)

(defclass contour ()
  ((edges
    :accessor edges
    :initarg :edges
    :initform (make-array 0 :fill-pointer 0 :adjustable t)
    :documentation "")))

(defun normalize (contours)

  ;; (format t "[normalize] contours: ~a~%" contours)
  
  (iter (for contour in-vector contours)
        (for edges = (edges contour))

        ;; (format t "[normalize,iter]: ~a, ~a~%" contour edges)
        
        (iter (for edge in-vector edges)
              (when (is-degenerate edge)
                t)) ; TODO: remove it
        
        (if (= (length edges) 1)
            (progn
              ;; Add 3 EdgeSegments to contour edges

              ;; (format t "[normalize,=1] ~a~%" edges)

              ;; Returns 3 edge-segment instances in a list
              ;; originally a method of EdgeSegment
              (multiple-value-bind (e0 e1 e2) (split-in-thirds (aref edges 0))
                (setf (fill-pointer edges) 0)
                (adjust-array edges 0)
                ;; Create edge-holder instances
                (vector-push-extend e0 edges)
                (vector-push-extend e1 edges)
                (vector-push-extend e2 edges))
            (progn

              ;; (format t "[normalize,/=1] ~a~%" edges)
              
              (iter (for i from 0 below (length edges))
                    ;; (format t "[normalize,/=1,for] ~a~%" i)
                    (let ((s1 (aref edges i))
                          (s2 (aref edges (mod (+ i 1) (length edges)))))
                      ;; point fn depends on type of s#
                      ;; (format t "[normalize,/=1,/=] ~a~%" s1)
                      (when (not (equalp (point s1 1) (point s2 0)))
                        (move-end-point s1 (point s2 0))))))))))



;; void Shape::normalize() {    
;;         if (contour->edges.size() == 1) {
;;             EdgeSegment *parts[3] = { };
;;             contour->edges[0]->splitInThirds(parts[0], parts[1], parts[2]);
;;             contour->edges.clear();
;;             contour->edges.push_back(EdgeHolder(parts[0]));
;;             contour->edges.push_back(EdgeHolder(parts[1]));
;;             contour->edges.push_back(EdgeHolder(parts[2]));
;;         }
;;         else {
            
;;             // Make sure that start points match end points exactly or we'll get artifacts.
;;             int n = contour->edges.size();
;;             for( int i = 0; i < n; i++ )
;;             {
;;                 EdgeSegment *s1 = contour->edges[i];
;;                 EdgeSegment *s2 = contour->edges[(i + 1) % n];
;;                 if( s1->point(1) != s2->point(0) )
;;                 {
;;                     s1->moveEndPoint(s2->point(0));
;;                 }
;;             }
;;         }
;;     }
;; }

;; void Contour::bounds(double &l, double &b, double &r, double &t) const {
;;     for (std::vector<EdgeHolder>::const_iterator edge = edges.begin(); edge != edges.end(); ++edge)
;;         (*edge)->bounds(l, b, r, t);
;; }
(defmethod bounds ((contour contour) left bottom right top)
  (iter (for edge in-vector (edges contour))
	(when *debug-bounds*
	  (format t "  [bounds:contour] iterating~%"))
	;; (format t  "~a~%" (edges contour))
	;; (format t "~a, ~a, ~a, ~a, ~a~%" edge left bottom right top)
        (multiple-value-bind (left-1 bottom-1 right-1 top-1) (bounds edge left bottom right top)
          (setf left left-1)
          (setf bottom bottom-1)
          (setf right right-1)
          (setf top top-1)))
  (values left bottom right top))
          
