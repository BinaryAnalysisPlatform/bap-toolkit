(require incident)
(require taint-sources)

;; Rule:
;; undefined-input (x) |- T(x)
;;
;; Motivation:
;; We don't know anything about data that is defined externally, so we
;; will assume, that it is also controlled by a user.
(defmethod undefined-input (x)
  (untrust/value x))
