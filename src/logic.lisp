(in-package :sdf)

;; from cl-alexandria
(declaim (inline clamp))
(defun clamp (number min max)
  "Clamps the NUMBER into [min, max] range. Returns MIN if NUMBER is lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (if (< number min)
      min
      (if (> number max)
	  max
	  number)))

;; Returns the middle out of three values
(declaim (inline median))
(defun median (a b c)
  (max (min a b)
       (min (max a b) c)))

;; Returns 1 for non-negative values and -1 for negative values.
(declaim (inline non-zero-sign))
(defun non-zero-sign (n)
  (- (* 2 (if (> n 0) 1 0)) 1))

;; Returns the weighted average of a and b.
(declaim (inline mix))
(defmethod mix ((a number) (b number) weight)
  (+ (* (- 1 weight)
	a)
     (* weight
	b)))

(defmethod mix ((a vec2) (b vec2) weight)
  (v+ (v* a (- 1 weight))
      (v* b weight)))





