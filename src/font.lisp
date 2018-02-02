(in-package :sdf)

(defclass ft-context ()
  ((pos
    :accessor pos
    :initarg :pos
    :initform (vec2 0.0 0.0)
    :documentation "Point2")
   (shape
    :accessor shape
    :initarg :shape
    :initform nil
    :documentation "Shape*")
   (contour
    :accessor contour
    :initarg :contour
    :initform nil
    :documentation "Contour*")))

;; Note:
;; Point2 = Vector2
(defun make-ft-vec2 (x y)
  (vec2 (/ x 64) (/ y 64)))

(defparameter *debug-outline-decompose* t)
(defparameter *debug-bounds* t)
(defparameter *debug-conic-signed-distance* t)
(defparameter *debug-conic-signed-distance-solve* t)
(defparameter *debug-linear-signed-distance* t)

(defparameter *debug-edge-coloring-simple* t)
(defparameter *debug-split-in-thirds* t)
(defparameter *debug-advance-to* t)
(defparameter *debug-collect-crossings* t)    ;; (iter (for code from 32 to 120)
(defparameter *debug-generate-msdf* t)

(defun main ()

  ;; (format t "~a~%" (freetype2:check-font-file "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf"))
  
  (let ((face (freetype2:new-face "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf")))
    ;; check face if null
  
    ;; printable asii printable characters range including extended
    ;; (iter (for code from 32 to 120)
    (let ((code 107))
	  
	  (let* ((width 32)
		 (height 32)
		 (shape (load-glyph face code))
		 (bitmap (make-array (* width height) :fill-pointer 0)))
	    
	    (normalize (contours shape))
	    
	    (edge-coloring-simple shape 3.0)
	    
	    (iter (for i from 0 below (* width height))
		  (vector-push (make-array 3 :fill-pointer 0) bitmap))
	    
	    (generate-msdf bitmap
			   shape
			   4.0 ; range
			   (vec2 1.0 1.0) ; scale
			   (vec2 4.0 4.0)) ; translation

	    ;; Write RGB float output
	    (when t
	      (with-open-file (out #p"/home/user/font-gen/sdf/output.txt"
				   :direction :output
				   :if-does-not-exist :create
				   :if-exists :supersede)
		(iter (for y from (- height 1) downto 0)
		      (iter (for x from 0 below width)
			    (let ((px (get-pixel bitmap x y width)))
			      (write-line (format nil "(~a, ~a) ~5$ ~5$ ~5$" x y (aref px 0) (aref px 1) (aref px 2))
					  out))))))
	    
	    (write-rgb-buffer-to-ppm-file (format nil "/home/user/font-gen/sdf/~a.ppm" code)
					  bitmap width height)
	    (format t "Wrote ~a~%" (format nil "/home/user/font-gen/sdf/~a.ppm" code)))))
  
  (sb-ext:exit))

(defun write-rgb-buffer-to-ppm-file (filename bitmap width height)
  (with-open-file (stream filename 
			  :element-type '(unsigned-byte 8)
			  :direction :output 
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (let* ((header (format nil "P6~A~D ~D~A255~A" 
			   #\newline
			   width height #\newline
			   #\newline)))
      
      (loop 
	 :for char :across header 
	 :do (write-byte (char-code char) stream)) #| Assumes char-codes match ASCII |#

      (iter (for y from (- height 1) downto 0) ; height-1
	    (iter (for x from 0 below width) ; width
		  (let* ((px (get-pixel bitmap x y width)))
		    (write-byte (clamp (truncate (* (aref px 0) #x100)) 0 #xff) stream)
		    (write-byte (clamp (truncate (* (aref px 1) #x100)) 0 #xff) stream)
		    (write-byte (clamp (truncate (* (aref px 2) #x100)) 0 #xff) stream)))))))


(defun load-glyph (face unicode)
  
  (freetype2:load-char face
		       unicode
		       1) ; FT_LOAD_NO_SCALE
  ;; check chr

  (let* ((shape (make-instance 'shape))
	 (context (make-instance 'ft-context :shape shape)))

    ;; make function for this
    (setf (fill-pointer (contours shape)) 0)
    (adjust-array (contours shape) 0)

    (setf (inverse-y-axis shape) nil)

    ;; do in let*
    ;; (setf (shape context) shape)
    (defparameter *ft-context* context)
    
    (let* ((glyph (freetype2:get-glyph face))
	   (outline (freetype2-types:ft-outlineglyph-outline glyph)))
      
      ;; pass context to callback
      (freetype2:do-outline-decompose outline
	(op p p2 p3)
	(handle-outline-decompose op p p2 p3)))

    shape))


(defun handle-outline-decompose (op p p2 p3)
  
  (cond
    ((eq op :moveto)
     (progn
       (setf (contour *ft-context*) (add-contour (shape *ft-context*)))
       (let ((to (make-ft-vec2 (freetype2-types:ft-vector-x p)
			       (freetype2-types:ft-vector-y p))))
	 (when *debug-outline-decompose*
	   (format t "[moveto] ~a:~%" to)
	   (format t "         ~a -> ~a~%" (pos *ft-context*) to))
	 (setf (pos *ft-context*) (vcopy2 to)))))
    ((eq op :lineto)
     (progn
       (let* ((to (make-ft-vec2 (freetype2-types:ft-vector-x p)
				(freetype2-types:ft-vector-y p)))
	      (edge (make-instance 'linear-segment
				   :p0 (pos *ft-context*)
				   :p1 to)))
	 (when *debug-outline-decompose*
	   (format t "[lineto] ~a:~%" edge)
	   (format t "         ~a~%" to)
	   (format t "         ~a | ~a~%" (pos *ft-context*) to))
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) (vcopy2 to)))))
    ((eq op :conicto)
     (progn
       (let* ((control (make-ft-vec2 (freetype2-types:ft-vector-x p)
				     (freetype2-types:ft-vector-y p)))
	      (to (make-ft-vec2 (freetype2-types:ft-vector-x p2)
				(freetype2-types:ft-vector-y p2)))
	      (edge (make-instance 'quadratic-segment
				   :p0 (pos *ft-context*)
				   :p1 control
				   :p2 to)))
	 (when *debug-outline-decompose*
	   (format t "[conicto] ~a:~%" edge)
	   (format t "          ~a | ~a, ~a~%" (pos *ft-context*) control to)
	   ;; (format t "          ~a~%" (points edge))
	   )
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) (vcopy2 to)))))
    ((eq op :cubicto)
     (progn
       (error "TODO: cubicto")
       (let* ((control-1 (make-ft-vec2 (freetype2-types:ft-vector-x p)
				       (freetype2-types:ft-vector-y p)))
	      (control-2 (make-ft-vec2 (freetype2-types:ft-vector-x p2)
				       (freetype2-types:ft-vector-y p2)))
	      (to (make-ft-vec2 (freetype2-types:ft-vector-x p3)
				(freetype2-types:ft-vector-y p3)))
	      (edge (make-instance 'cubic-segment
				   :p0 (pos *ft-context*)
				   :p1 control-1
				   :p2 control-2
				   :p3 to)))
	 (when *debug-outline-decompose*
	   (format t "[cubicto] ~a:~%" edge)
	   (format t "          ~a | ~a, ~a, ~a~%" (pos *ft-context*) control-1 control-2 to))
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) (vcopy2 to)))))))


(declaim (inline clamp))
(defun clamp (number min max)
  "Clamps the NUMBER into [min, max] range. Returns MIN if NUMBER is lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (if (< number min)
      min
      (if (> number max)
	  max
	  number)))

;; /// Returns the middle out of three values
;; template <typename T>
;; inline T median(T a, T b, T c) {
;;     return max(min(a, b),
;;                min(max(a, b),
;;                    c));
;; }
(defun median (a b c)
  (max (min a b)
       (min (max a b) c)))


;; bool Vector2::operator!() const {
;;     return !x && !y;
;; }
;; returns whether zero vector
(defun v-not (v)
  (and (= (vx2 v) 0.0)
       (= (vy2 v) 0.0)))

;; double dotProduct(const Vector2 &a, const Vector2 &b) {
;;     return a.x*b.x+a.y*b.y;
;; }
(defun dot-product (a b)
  (+ (* (vx2 a)
	(vx2 b))
     (* (vy2 a)
	(vy2 b))))

;; double crossProduct(const Vector2 &a, const Vector2 &b) {
;;     return a.x*b.y-a.y*b.x;
;; }
(defun cross-product (a b)
  (- (* (vx2 a)
	(vy2 b))
     (* (vy2 a)
	(vx2 b))))

;; Vector2 getOrthonormal(bool polarity = true, bool allowZero = false) const;
;; Vector2 Vector2::getOrthonormal(bool polarity, bool allowZero) const {
;;     double len = length();
;;     if (len == 0)
;;         return polarity ? Vector2(0, !allowZero) : Vector2(0, -!allowZero);
;;     return polarity ? Vector2(-y/len, x/len) : Vector2(y/len, -x/len);
;; }
(defun get-orthonormal (point &optional (polarity t) (allow-zero nil))
  (let ((len (vlength point)))
    (when (= len 0)
      (return-from get-orthonormal (if polarity
				       (vec2 0 (if (not allow-zero) 1 0))
				       (vec2 0 (if (not allow zero) -1 0)))))
    (return-from get-orthonormal (if polarity
				     (vec2 (/ (- (vy2 point)) len) (/ (vx2 point) len))
				     (vec2 (/ (vy2 point) len) (/ (- (vx2 point)) len))))))

;; /// Returns 1 for non-negative values and -1 for negative values.
;; template <typename T>
;; inline int nonZeroSign(T n) {
;;     return 2*(n > T(0))-1;
;; }
(defun non-zero-sign (n)
  (- (* 2 (if (> n 0) 1 0)) 1))


;; template <typename T, typename S>
;; inline T mix(T a, T b, S weight) {
;;     return (1-weight)*a)+(weight*b)
;; }
(defun mix (a b weight)
  (+ (* (- 1 weight)
	a)
     (* weight
	b)))

;; (v+ (v* a (- 1 weight))
;;     (v* b weight)

;; (defun mix-point (a b weight)
;;   ;; scale vector then add
;;   (let ((as (- 1 weight)))
;;     (vec2 (+ (* as (vx2 a))
;; 	     (* weight (vx2 b)))
;; 	  (+ (* as (vy2 a))
;; 	     (* weight (vy2 b))))))
(defun mix-point (a b weight)
  (v+ (v* a (- 1 weight))
      (v* b weight)))
    
;; (make-array 2
;;      :initial-contents
;;      (list (+ (* as (aref a 0))
;;           (* weight (aref b 0)))
;;            (+ (* as (aref a 1))
;;           (* weight (aref b 1)))))))

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
