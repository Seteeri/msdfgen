(in-package :sdf)

;; Represents a signed distance and alignment, which together can be compared to uniquely determine the closest edge segment.
(defclass signed-distance ()
  ((distance
    :accessor distance
    :initarg :distance
    :initform -1d240
    :documentation "")
   (dot
    :accessor dot
    :initarg :dot
    :initform 1d0
    :documentation "")))


(defun make-sd-infinite () (make-instance 'signed-distance))

(defmethod sd< ((a signed-distance) (b signed-distance))
  (or (< (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a))
	      (abs (distance b)))
	   (< (dot a) (dot b)))))

(defmethod sd> ((a signed-distance) (b signed-distance))
  (or (> (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
	   (> (dot a) (dot b)))))

(defmethod sd<= ((a signed-distance) (b signed-distance))
  (or (< (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
	   (<= (dot a) (dot b)))))

(defmethod sd>= ((a signed-distance) (b signed-distance))
  (or (> (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
	   (>= (dot a) (dot b)))))
