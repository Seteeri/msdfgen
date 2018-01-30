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

;; static Point2 ftPoint2(const FT_Vector &vector) {
;;     return Point2(vector.x/64., vector.y/64.);
;; }
;;
;; Note:
;; Point2 = Vector2
(defun make-ft-vec2 (x y)
  (vec2 (/ x 64d0) (/ y 64d0)))

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

;; (loop 
;;     :for x :upfrom 0 :below width
;;     :do (loop :for y :upfrom 0 :below height 
;;        :do (let* ((pixel (get-pixel buffer x y width))
;;               (red (aref pixel 0))
;;               (green (aref pixel 1))
;;               (blue (aref pixel 2)))
;;            (write-byte red stream)
;;            (write-byte green stream)
;;            (write-byte blue stream))))))

(defun main ()
  ;; Shape shape;
  ;; if (loadGlyph(shape, font, 'A')) {
  ;;   shape.normalize();
  ;;   //                      max. angle
  ;;   edgeColoringSimple(shape, 3.0);
  ;;   //           image width, height
  ;;   Bitmap<FloatRGB> msdf(32, 32);
  ;;   //                     range, scale, translation
  ;;   generateMSDF(msdf, shape, 4.0, 1.0, Vector2(4.0, 4.0));
  ;;   savePng(msdf, "output.png");
  ;; }
  
  (let ((face (freetype2:new-face "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf")))
    ;; check face if null

    ;; (format t "~a~%" (freetype2:check-font-file "/usr/share/fonts/TTF/ttf-inconsolata-g.ttf"))
    
    (let ((shape (load-glyph face #\a)))
      
      (normalize (contours shape))
      
      (edge-coloring-simple shape 3.0)
      
      ;; bitmap... (msdf 32 32)
      ;; each pixel contains RGB floats
      (let ((bitmap (make-array (* 32 32) :fill-pointer 0)))
	(iter (for i from 0 below (* 32 32))
	      (vector-push (make-array 3 :fill-pointer 0)
			   bitmap))
	
	(generate-msdf bitmap
		       shape
		       4.0 ; range
		       (vec2 1.0 1.0) ; scale
		       (vec2 4.0 4.0)) ; translation

					; 8,14
	;; (sb-ext:exit)

	;; Write output
	(when nil
	  (with-open-file (out #p"output.txt"
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists :supersede)
	    (iter (for y from 31 downto 0) ; height-1
		  (iter (for x from 0 below 32) ; width
			(let ((px (get-pixel bitmap x y 32)))
			  (write-line (format nil "(~a, ~a) ~5$ ~5$ ~5$" x y (aref px 0) (aref px 1) (aref px 2))
				      out))))))
	
	;; (when nil
	;;   (iter (for y from 31 downto 0) ; height-1
	;;  (iter (for x from 0 below 32) ; width
	;;        (let* ((px (get-pixel bitmap x y 32)))
	;;      (setf (aref px 0) (clamp (truncate (* (aref px 0) #x100)) 0 #xff))
	;;      (setf (aref px 1) (clamp (truncate (* (aref px 1) #x100)) 0 #xff))
	;;      (setf (aref px 2) (clamp (truncate (* (aref px 2) #x100)) 0 #xff))
	;;      (format t "(~a, ~a): ~a, ~a, ~a ~%" x y (aref px 0) (aref px 1) (aref px 2))))))

	;; (sb-ext:exit)
	
	(write-rgb-buffer-to-ppm-file "/home/user/output.ppm" bitmap 32 32))))
  
  (format t "DONE~%")
  (sb-ext:exit))


(declaim (inline clamp))
(defun clamp (number min max)
  "Clamps the NUMBER into [min, max] range. Returns MIN if NUMBER is lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (if (< number min)
      min
      (if (> number max)
	  max
	  number)))


(defun load-glyph (face unicode)
  
  (freetype2:load-char face
		       unicode
		       1) ; FT_LOAD_NO_SCALE
  ;; check chr

  (let* ((output (make-instance 'shape))
	 (context (make-instance 'ft-context :shape output)))
    
    ;; return advance if requested
    ;; font->face->glyph->advance.x/64

    ;; contours is a list
    ;; (clear (contours shape))
    ;; (setf (inverse-y-axis output) nil)

    ;; do in let*
    ;; (setf (shape context) output)

    (defparameter *ft-context* context)

    ;; (let* ((,outline-glyph (get-glyph ,face))
    ;;        (,outline (ft-outlineglyph-outline ,outline-glyph)))
    
    (let* ((glyph (freetype2:get-glyph face))
	   (outline (freetype2-types:ft-outlineglyph-outline glyph)))
      
      ;; (format t "glyph: ~a, outline: ~a~%" glyph outline)
      
      ;; callbacks need access to context
      ;; instead of pointer use a global name
      ;; (format t "Call do-outline-decompose~%")
      (freetype2:do-outline-decompose outline
	(op p p2 p3)
	(handle-outline-decompose op p p2 p3)))

    output))


(defun handle-outline-decompose (op p p2 p3)
  ;; (format t "~a, ~a, ~a, ~a~%"
  ;;      op p p2 p3)
  
  (cond
    ((eq op :moveto)
     (progn
       (setf (contour *ft-context*) (add-contour (shape *ft-context*)))
       (let ((point1 (make-ft-vec2 (freetype2-types:ft-vector-x p) (freetype2-types:ft-vector-y p))))
	 (format t "[moveto] ft-context pos: ~a -> ~a~%" (pos *ft-context*) point1)
	 (setf (pos *ft-context*) point1))))
    ((eq op :lineto)
     (progn
       (let ((point1 (make-ft-vec2 (freetype2-types:ft-vector-x p) (freetype2-types:ft-vector-y p)))
	     (point11 (make-ft-vec2 (freetype2-types:ft-vector-x p) (freetype2-types:ft-vector-y p)))
	     (edge (make-instance 'linear-segment)))
	 (setf (aref (points edge) 0) (pos *ft-context*))
	 (setf (aref (points edge) 1) point1)
	 (format t "[lineto] ft-context pos: ~a -> ~a~%" (pos *ft-context*) point1)
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) point11))))
    ((eq op :conicto)
     (progn
       (let ((control (make-ft-vec2 (freetype2-types:ft-vector-x p) (freetype2-types:ft-vector-y p)))
	     (to (make-ft-vec2 (freetype2-types:ft-vector-x p) (freetype2-types:ft-vector-y p2)))
	     (edge (make-instance 'quadratic-segment)))
	 (setf (aref (points edge) 0) (pos *ft-context*))
	 (setf (aref (points edge) 1) control)
	 (setf (aref (points edge) 2) to)
	 (format t "[conicto] ft-context pos: ~a -> ~a~%" (pos *ft-context*) to)
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) to))))
    ((eq op :cubicto)
     (progn
       (let ((control-1 (make-ft-vec2 (freetype2-types:ft-vector-x p) (freetype2-types:ft-vector-y p)))
	     (control-2 (make-ft-vec2 (freetype2-types:ft-vector-x p2) (freetype2-types:ft-vector-y p2)))
	     (to (make-ft-vec2 (freetype2-types:ft-vector-x p3) (freetype2-types:ft-vector-y p3)))
	     (edge (make-instance 'quadratic-segment)))
	 (setf (aref (points edge) 0) (pos *ft-context*))
	 (setf (aref (points edge) 1) control)
	 (setf (aref (points edge) 2) control)
	 (setf (aref (points edge) 3) to)
	 (format t "[cubicto] ft-context pos: ~a -> ~a~%" (pos *ft-context*) to)
	 (vector-push-extend edge (edges (contour *ft-context*)))
	 (setf (pos *ft-context*) to))))))


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


(defun mix (a b weight)
  (+ (* (- 1 weight)
	a)
     (* weight
	b)))

(defun mix-point (a b weight)
  ;; scale vector then add
  (let ((as (- 1 weight)))
    (vec2 (+ (* as (vx2 a))
	     (* weight (vx2 b)))
	  (+ (* as (vy2 a))
	     (* weight (vy2 b))))))

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
  ;; (format t "      [point-bounds] ~a, ~a, ~a, ~a, ~a~%" point left bottom right top)
  (values (if (< (vx2 point) left) (vx2 point) left)
	  (if (< (vy2 point) bottom) (vy2 point) bottom)
	  (if (> (vx2 point) right) (vx2 point) right)
	  (if (> (vy2 point) top) (vy2 point) top)))


;; int solveQuadratic(double x[2], double a, double b, double c) {
;;     if (fabs(a) < 1e-14) {
;;         if (fabs(b) < 1e-14) {
;;             if (c == 0)
;;                 return -1;
;;             return 0;
;;         }
;;         x[0] = -c/b;
;;         return 1;
;;     }
;;     double dscr = b*b-4*a*c;
;;     if (dscr > 0) {
;;         dscr = sqrt(dscr);
;;         x[0] = (-b+dscr)/(2*a);
;;         x[1] = (-b-dscr)/(2*a);
;;         return 2;
;;     } else if (dscr == 0) {
;;         x[0] = -b/(2*a);
;;         return 1;
;;     } else
;;         return 0;
;; }
(defun solve-quadratic (x a b c)
  (error "TODO: solve-quadratic")
  (when (< (abs a) 1E-14)
    (when (< (abs b) 1E-14)
      (when (= c 0)
	(return-from solve-quadratic -1))
      (return-from solve-quadratic 0))
    (setf (aref x 0) (/ (- c) b))
    (return-from solve-quadratic 1))
  (let ((dscr (- (* b b)
		 (* 4 a c))))
    (cond ((> dscr 0)
	   (setf dscr (sqrt dscr))
	   (setf (aref x 0) (/ (+ (- b) dscr) (* 2 a)))
	   (setf (aref x 1) (/ (- (- b) dscr) (* 2 a)))
	   (return-from solve-quadratic 2))
	  ((= dscr 0)
	   (setf (aref x 0) (/ (- b) (* 2 a)))
	   (return-from solve-quadratic 1))
	  (t
	   (return-from solve-quadratic 0)))))


;; int solveCubicNormed(double *x, double a, double b, double c) {
;;     double a2 = a*a;
;;     double q  = (a2 - 3*b)/9; 
;;     double r  = (a*(2*a2-9*b) + 27*c)/54;
;;     double r2 = r*r;
;;     double q3 = q*q*q;
;;     double A, B;
;;     if (r2 < q3) {
;;         double t = r/sqrt(q3);
;;         if (t < -1) t = -1;
;;         if (t > 1) t = 1;
;;         t = acos(t);
;;         a /= 3; q = -2*sqrt(q);
;;         x[0] = q*cos(t/3)-a;
;;         x[1] = q*cos((t+2*M_PI)/3)-a;
;;         x[2] = q*cos((t-2*M_PI)/3)-a;
;;         return 3;
;;     } else {
;;         A = -pow(fabs(r)+sqrt(r2-q3), 1/3.); 
;;         if (r < 0) A = -A;
;;         B = A == 0 ? 0 : q/A;
;;         a /= 3;
;;         x[0] = (A+B)-a;
;;         x[1] = -0.5*(A+B)-a;
;;         x[2] = 0.5*sqrt(3.)*(A-B);
;;         if (fabs(x[2]) < 1e-14)
;;             return 2;
;;         return 1;
;;     }
;; }
(defun solve-cubic-normed (x a b c)
  (error "TODO: solve-cubic-normed")
  (let* ((a2 (* a a))
	 (q (/ (- a2 (* 3 b)) 9))
	 (r (/ (+ (* a (- (* 2 a2) (* 9 b))) (* 27 c)) 54))
	 (r2 (* r r))
	 (q3 (* q q q)))
    (if (< r2 q3)
	(let ((tt (/ r (sqrt q3))))
	  (when (< tt -1) (setf tt -1))
	  (when (> tt 1) (setf tt 1))
	  (setf tt (acos tt))
	  (setf a (/ a 3))
	  (setf q (* -2 (sqrt q)))
	  (values (- (* q (cos (/ tt 3))) a)
		  (- (* q (cos (/ (+ tt (* 2 PI)) 3))) a)
		  (- (* q (cos (/ (- tt (* 2 PI)) 3))) a)
		  3))
	(let ((aa (- (expt (+ (abs r) (sqrt (- r2 q3))) (/ 1 3))))
	      (bb 0))
	  (when (< r 0) (setf aa (- aa)))
	  (setf bb (if (= aa 0) 0 (/ q aa)))
	  (setf aa (/ aa 3))
	  (values (- (+ aa bb) a)
		  (- (* (- 0.5) (sqrt 3.0)) aa)
		  (* 0.5 (sqrt 3.0) (- aa bb))
		  (if (< (abs (aref x 2)) 1E-14) 2 1))))))

;; int solveCubic(double x[3], double a, double b, double c, double d) {
;;     if (fabs(a) < 1e-14)
;;         return solveQuadratic(x, b, c, d);
;;     return solveCubicNormed(x, b/a, c/a, d/a);
;; }
(defun solve-cubic (x a b c d)
  (error "TODO: solve-cubic")
  (if (< (abs a) 1E-14)
      (solve-quadratic x b c d) ; x[2]
      (solve-cubic-normed x (/ b a) (/ c a) (/ d a)))) ; x[3]
