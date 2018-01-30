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

;; /// Fill rules compatible with SVG: https://www.w3.org/TR/SVG/painting.html#FillRuleProperty
;; enum FillRule {
;;     None = 0, // Legacy
;;     NonZero = 1,
;;     EvenOdd = 2,
;; };
(defconstant +fill-rule-none+ 0)
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

    (format t "[edge-coloring-simple] contours: ~a~%" (length (contours shape)))
    
    (iter (for contour in-vector (contours shape))

      (format t "[edge-coloring-simple] contour: ~a~%" contour)
      
      ;; clear corners for each iteration
      (let ((corners (make-array 0 :fill-pointer 0 :adjustable t))
        (edges (edges contour)))

        (format t "[edge-coloring-simple] length of edges: ~a~%" (length edges))
        (when (/= (length edges) 0)
          (let ((prev-direction (direction (aref edges (- (length edges) 1))
                           1)) ; last edge direction
            (index 0))
        ;; (format t "[edge-coloring-simple] prev-direction: ~a~%" prev-direction)
        (iter (for edge in-vector edges)
              ;; (format t "[edge-coloring-simple] index: ~a~%" index)
              ;; (format t "~%[edge-coloring-simple] arg1: ~a~%" (nvunit prev-direction))
              ;; (format t "[edge-coloring-simple] arg2: ~a~%" (nvunit (direction edge 0)))
              (when (is-corner (vunit prev-direction)
                       (vunit (direction edge 0))
                       cross-threshold)
            ;; (format t "[edge-coloring-simple] push: ~a~%" index)
            (vector-push-extend index corners))
              (setf prev-direction (direction edge 1))
              (incf index))))

        ;; (sb-ext:exit)
        
        ;; smooth contours
        (format t "[edge-coloring-simple] length of corners: ~a~%" (length corners))
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
             (when (>= (length edges) 3)
               (let ((m (length edges)))
             (iter (for i from 0 below m) ; ++i
                   ;; check this
                   (setf (color (aref edges (mod (+ corner i) m)))
                     (nth (round (+ 1 (- (+ (- (+ 3 (/ (* 2.875 i) (- m 1))) 1.4375) 0.5) 3))) colors)))))
             (when (>= (length edges) 1)
               (let ((parts (make-array 7)))
             (multiple-value-bind (e0-0 e0-1 e0-2) (split-in-thirds (aref edges 0))
               (setf (aref parts (+ 0 (* 3 corner))) e0-0)
               (setf (aref parts (+ 1 (* 3 corner))) e0-1)
               (setf (aref parts (+ 2 (* 3 corner))) e0-2)
               (if (>= (length edges) 2)
                   (multiple-value-bind (e1-0 e1-1 e1-2) (split-in-thirds (aref edges 1))
                 (setf (aref parts (- 3 (* 3 corner))) e1-0)
                 (setf (aref parts (- 4 (* 3 corner))) e1-1)
                 (setf (aref parts (- 5 (* 3 corner))) e1-2)

                 (setf (color (aref parts 0)) (aref colors 0))
                 (setf (color (aref parts 1)) (aref colors 0))
                 
                 (setf (color (aref parts 2)) (aref colors 1))
                 (setf (color (aref parts 3)) (aref colors 1))

                 (setf (color (aref parts 4)) (aref colors 2))
                 (setf (color (aref parts 5)) (aref colors 2)))
                   (progn
                 (setf (color (aref parts 0)) (aref colors 0))
                 (setf (color (aref parts 1)) (aref colors 1))
                 (setf (color (aref parts 2)) (aref colors 2))))
               
               ;; double check this...
               (setf (fill-pointer edges) 0)
               (adjust-array edges 0)

               (iter (for i from 0 below 7)
                 (when (aref parts i)
                   (vector-push-extend (make-instance 'edge-holder (aref parts i))
                               edges))))))))
          
          ;; multiple corners
          (t
           (let* ((corner-count (length corners))
              (spline 0)
              (start (aref corners 0))
              (m (length edges))
              (color (switch-color +white+ seed))
              (initial-color color))
                         
             ;; (format t "~%[edge-coloring-simple] m: ~a, start: ~a~%" m start)
             
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

;; void edgeColoringSimple(Shape &shape, double angleThreshold, unsigned long long seed) {
;;     double crossThreshold = sin(angleThreshold);
;;     std::vector<int> corners;
;;     for (std::vector<Contour>::iterator contour = shape.contours.begin(); contour != shape.contours.end(); ++contour) {
;;         // Identify corners
;;         corners.clear();
;;         if (!contour->edges.empty()) {
;;             Vector2 prevDirection = (*(contour->edges.end()-1))->direction(1);
;;             int index = 0;
;;             for (std::vector<EdgeHolder>::const_iterator edge = contour->edges.begin(); edge != contour->edges.end(); ++edge, ++index) {
;;                 if (isCorner(prevDirection.normalize(),
;;                      (*edge)->direction(0).normalize(),
;;                              crossThreshold))
;;                     corners.push_back(index);
;;                 prevDirection = (*edge)->direction(1);
;;             }
;;         }
;;
;;         // Smooth contour
;;         if (corners.empty())
;;             for (std::vector<EdgeHolder>::iterator edge = contour->edges.begin(); edge != contour->edges.end(); ++edge)
;;                 (*edge)->color = WHITE;
;;         // "Teardrop" case
;;         else if (corners.size() == 1) {
;;             EdgeColor colors[3] = { WHITE, WHITE };
;;             switchColor(colors[0], seed);
;;             switchColor(colors[2] = colors[0], seed);
;;             int corner = corners[0];
;;             if (contour->edges.size() >= 3) {
;;                 int m = contour->edges.size();
;;                 for (int i = 0; i < m; ++i)
;;                     contour->edges[(corner+i)%m]->color = (colors+1)[int(3+2.875*i/(m-1)-1.4375+.5)-3];
;;             } else if (contour->edges.size() >= 1) {
;;                 // Less than three edge segments for three colors => edges must be split
;;                 EdgeSegment *parts[7] = { };
;;                 contour->edges[0]->splitInThirds(parts[0+3*corner], parts[1+3*corner], parts[2+3*corner]);
;;                 if (contour->edges.size() >= 2) {
;;                     contour->edges[1]->splitInThirds(parts[3-3*corner], parts[4-3*corner], parts[5-3*corner]);
;;                     parts[0]->color = parts[1]->color = colors[0];
;;                     parts[2]->color = parts[3]->color = colors[1];
;;                     parts[4]->color = parts[5]->color = colors[2];
;;                 } else {
;;                     parts[0]->color = colors[0];
;;                     parts[1]->color = colors[1];
;;                     parts[2]->color = colors[2];
;;                 }
;;                 contour->edges.clear();
;;                 for (int i = 0; parts[i]; ++i)
;;                     contour->edges.push_back(EdgeHolder(parts[i]));
;;             }
;;         }
;;         // Multiple corners
;;         else {
;;             int cornerCount = corners.size();
;;             int spline = 0;
;;             int start = corners[0];
;;             int m = contour->edges.size();
;;             EdgeColor color = WHITE;
;;             switchColor(color, seed);
;;             EdgeColor initialColor = color;
;;             for (int i = 0; i < m; ++i) {
;;                 int index = (start+i)%m;
;;                 if (spline+1 < cornerCount && corners[spline+1] == index) {
;;                     ++spline;
;;                     switchColor(color, seed, EdgeColor((spline == cornerCount-1)*initialColor));
;;                 }
;;                 contour->edges[index]->color = color;
;;             }
;;         }
;;     }
;; }

;; }

;; static bool isCorner(const Vector2 &aDir, const Vector2 &bDir, double crossThreshold) {
;;     return dotProduct(aDir, bDir) <= 0 || fabs(crossProduct(aDir, bDir)) > crossThreshold;
;; }
(defun is-corner (a-dir b-dir cross-threshold)
  (or (< (dot-product a-dir b-dir) 0)
      (> (abs (cross-product a-dir b-dir)) cross-threshold)))

;; static void switchColor(EdgeColor &color, unsigned long long &seed, EdgeColor banned = BLACK) {
;;     EdgeColor combined = EdgeColor(color&banned);
;;     if (combined == RED || combined == GREEN || combined == BLUE) {
;;         color = EdgeColor(combined^WHITE);
;;         return;
;;     }
;;     if (color == BLACK || color == WHITE) {
;;         static const EdgeColor start[3] = { CYAN, MAGENTA, YELLOW };
;;         color = start[seed%3];
;;         seed /= 3;
;;         return;
;;     }
;;     int shifted = color<<(1+(seed&1));
;;     color = EdgeColor((shifted|shifted>>3)&WHITE);
;;     seed >>= 1;
;; }

;; return color and seed
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
    
;; void Shape::bounds(double &l, double &b, double &r, double &t) const {
;;     for (std::vector<Contour>::const_iterator contour = contours.begin(); contour != contours.end(); ++contour)
;;         contour->bounds(l, b, r, t);
;; }
(defmethod bounds ((shape shape) left bottom right top)
  (iter (for contour in-vector (contours shape))
    ;; (format t "[bounds:shape] iterating~%")
    (multiple-value-bind (left-1 bottom-1 right-1 top-1) (bounds contour left bottom right top)
      (setf left left-1)
      (setf bottom bottom-1)
      (setf right right-1)
      (setf top top-1)))
  (values left bottom right top))
