(in-package :sdf)

;; /// Represents a signed distance and alignment, which together can be compared to uniquely determine the closest edge segment.
;; class SignedDistance {

;; public:
;;     static const SignedDistance INFINITE;

;;     double distance;
;;     double dot;

;;     SignedDistance();
;;     SignedDistance(double dist, double d);

;;     friend bool operator<(SignedDistance a, SignedDistance b);
;;     friend bool operator>(SignedDistance a, SignedDistance b);
;;     friend bool operator<=(SignedDistance a, SignedDistance b);
;;     friend bool operator>=(SignedDistance a, SignedDistance b);
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

;; const SignedDistance SignedDistance::INFINITE(-1e240, 1);

;; SignedDistance::SignedDistance() : distance(-1e240), dot(1) { }

;; SignedDistance::SignedDistance(double dist, double d) : distance(dist), dot(d) { }

;; bool operator<(SignedDistance a, SignedDistance b) {
;;     return fabs(a.distance) < fabs(b.distance) || (fabs(a.distance) == fabs(b.distance) && a.dot < b.dot);
;; }
(defmethod sd< ((a signed-distance) (b signed-distance))
  (or (< (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
       (< (dot a) (dot b)))))

;; bool operator>(SignedDistance a, SignedDistance b) {
;;     return fabs(a.distance) > fabs(b.distance) || (fabs(a.distance) == fabs(b.distance) && a.dot > b.dot);
;; }
(defmethod sd> ((a signed-distance) (b signed-distance))
  (or (> (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
       (> (dot a) (dot b)))))

;; bool operator<=(SignedDistance a, SignedDistance b) {
;;     return fabs(a.distance) < fabs(b.distance) || (fabs(a.distance) == fabs(b.distance) && a.dot <= b.dot);
;; }
(defmethod sd<= ((a signed-distance) (b signed-distance))
  (or (< (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
       (<= (dot a) (dot b)))))

;; bool operator>=(SignedDistance a, SignedDistance b) {
;;     return fabs(a.distance) > fabs(b.distance) || (fabs(a.distance) == fabs(b.distance) && a.dot >= b.dot);
;; }
(defmethod sd>= ((a signed-distance) (b signed-distance))
  (or (> (abs (distance a)) (abs (distance b)))
      (and (= (abs (distance a)) (abs (distance b)))
       (>= (dot a) (dot b)))))
