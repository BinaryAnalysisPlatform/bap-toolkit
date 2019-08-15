
;; The algorithm itself is quite simple:
;; We taint every constant that is saved into a register or a memory location
;; via stored/written methods. If the taint reaches any condition then we
;; consider this constant checked and no worries needed. Finally, if
;; there is a loading/storing operation with unchecked and tainted constant
;; then we trigger an incident.

;; Dealing with false positives.
;; The next considerations are used to reduce false positives.
;; 1) Analysis is written with keeping in mind that if a programmer
;;    checked a pointer somehow - no matter if it was right or not -
;;    then using of the pointer is considered as a safe one. For example,
;;    we apply this approach in the next case: mem [RAX + RBX] := 42,
;;    when both registers hold zero and only one of them was checked -
;;    in this case no incident will be emited
;; 2) unresolved calls: we don't know, if an unresolved function
;;    returns anything or not, hence we need to taint a value in
;;    the abi-specific register which is responsible for the return
;;    argument and not use it as a source of incidents.
;; 3) we also don't want to see multiple manifestations of a single
;;    dereference
;; 4) If any subroutine wasn't visited in the current path because it
;;    was visited in the other, we need to drop its result.
;;
(require taint)

(defun notify-null-deref (taint)
  (let ((pc (get-current-program-counter))
        (start (dict-get 'intro taint))
        (intro (dict-get 'intro/location taint)))
    (when (not (is-reported-deref pc))
      (when start
        (msg "pointer was introduced at $0" start))
      (notify-null-ptr-dereference start pc)
      (incident-report 'null-ptr-deref (incident-location) intro))))

(defun is-null (ptr)
  (and (not ptr) (all-static-constant ptr)))

(defun is-safe (ptr)
  (or
      (taint-get-indirect 'dont-believe ptr)
      (taint-get-direct 'dont-believe ptr)
      (taint-get-indirect 'failed ptr)
      (taint-get-direct 'failed ptr)))

(defun check-deref-null-ptr (ptr)
  (when (is-null ptr)
    (let ((taint (taint-get-direct 'const-ptr ptr))
          (checked (is-checked-pointer ptr)))
      (when (and taint (not (is-safe ptr)) (not checked))
        (taint-introduce-indirectly 'failed ptr 1)
        (taint-introduce-directly   'failed ptr)
        (notify-null-deref taint)))))

(defmethod eval-cond (x)
  (let ((taint (taint-get-direct 'const-ptr x)))
    (when taint
      (mark-taint-as-checked taint))))

(defun is-static-const (x)
  (and (all-static-constant x) (not (is-initial-value x))))

(defmethod written (v x)
  (when (is-static-const x)
    (let ((taint (taint-introduce-directly 'const-ptr x)))
      (dict-add 'intro taint (get-current-program-counter))
      (dict-add 'intro/location taint (incident-location)))))

;; stored method will detach previous indirect taints by the
;; address, and we can get a manifestation of the same
;; dereference that we discovered earlier. That's why we
;; do this trick in the storing/stored methods.
(defmethod stored (a x)
  (let ((known (dict-has 'known-fail (get-current-program-counter))))
    (when known
      (taint-introduce-indirectly 'failed a 1))
    (when (and (not known) (is-static-const x))
      (let ((taint (taint-introduce-indirectly 'const-ptr a 1)))
        (dict-add 'intro taint (get-current-program-counter))
        (dict-add 'intro/location taint (incident-location))))))

(defmethod read (var val)
  (when (is-return-from-unresolved val)
    (taint-introduce-directly 'dont-believe val)))

(defmethod loading (ptr)
  (check-deref-null-ptr ptr))

(defmethod storing (ptr)
  (let ((known (taint-get-indirect 'failed ptr)))
    (when known
      (dict-add 'known-fail (get-current-program-counter) ptr))
    (when (not known)
      (check-deref-null-ptr ptr))))
