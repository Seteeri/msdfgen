(in-package :sdf)

(defconstant +px-red+ 0)
(defconstant +px-green+ 1)
(defconstant +px-blue+ 2)

(declaim (inline get-pixel))
(defun get-pixel (output x y w)
  (aref output (+ (* y w) x)))

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
		  (let* ((px (get-pixel bitmap x y width))
			 (r (clamp (truncate (* (aref px +px-red+) #x100)) 0 #xff))
			 (g (clamp (truncate (* (aref px +px-green+) #x100)) 0 #xff))
			 (b (clamp (truncate (* (aref px +px-blue+) #x100)) 0 #xff)))

		    ;; 14,5
		    ;; (when (and (= x 14) (= y 5))
		    ;;   (format t "(~a, ~a) ~$ -> ~a ~a ~a~%" x y px r g b)
		    ;;   (format t "~10$~%" (* (aref px +px-green+) (coerce #x100 'double-float)))
		    ;;   (format t "~a~%" (truncate (* (aref px +px-green+) #x100)) 0 #xff)
		    ;;   (format t "~a~%" (clamp (truncate (* (aref px +px-green+) #x100)) 0 #xff))
		    ;;  )
		    
		      
		      ;; (format t "~$~%"
		      ;; 	      (coerce (aref px +px-green+) 'double-float)
		      ;; 	      (coerce #x100))
		    
		    (write-byte r stream)
		    (write-byte g stream)
		    (write-byte b stream)))))))



(defconstant +buffer-size+ (expt 2 16))

(defun compare-ppm-files ()

  (iter (for code from 32 to 255)

	(let ((sdf-path (format nil "/home/user/font-gen/sdf/~a.ppm" code))
	      (msdf-path (format nil "/home/user/font-gen/msdfgen/~a.ppm" code)))
	  
	  (with-open-file (in-1 sdf-path
			      :direction :input
			      :element-type '(unsigned-byte 8))

	    (with-open-file (in-2 msdf-path
				:direction :input
				:element-type '(unsigned-byte 8))
	    
	      (let* ((buffer-1 (make-array +buffer-size+ :element-type (stream-element-type in-1)))
		     (buffer-2 (make-array +buffer-size+ :element-type (stream-element-type in-2)))
		     (size-1 (read-sequence buffer-1 in-1))
		     (size-2 (read-sequence buffer-2 in-2)))

		;; (iter (for ch-1 in-vector buffer-1)
		;;       (for ch-2 in-vector buffer-2)
		;;       (for j from 0 below (length buffer-1))
		;;       (for k from 0 below (length buffer-2))
		;;       (let ((e (eq ch-1 ch-2)))
		;; 	(when (not e)
		;; 	  (format t "[~a][~a] ~a != ~a~%" code j ch-1 ch-2))))
		
		(when (and (plusp size-1) (plusp size-2))
		  (format t "equalp @ ~a: ~a~%" code (equalp buffer-1 buffer-2)))))))))
