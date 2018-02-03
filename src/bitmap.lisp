(in-package :sdf)

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


(declaim (inline get-pixel))
(defun get-pixel (output x y w)
  (aref output (+ (* y w) x)))
