(require taint)

;; The algorithm itself is quite simple:
;; We taint every constant that is saved into a register or a memory location
;; via stored/written methods. If the taint reaches any condition then we
;; consider this constant checked and no worries needed. Finally, if
;; there is a loading/storing operation with unchecked and tainted constant
;; then we trigger an incident. All the other tricks with unresolved
;; calls, known failures is used to reduce false positives.

;; Dealing with false positives
;; There are next consideration abour false positives.
;; 1) Analysis is written with keeping in mind that if programmer
;; checked a pointer somehow - no matter if it was right or not
;; then using of the pointer is considered as a safe one. For example,
;; we apply this approach in the next case: mem [RAX + RBX] := 42,
;; when both registers hold zero and only one of them was checked.
;; 2) unresolved calls: we don't know, if an unresolved function
;; returns anything or not, hence we need to taint a value in
;; the abi-specific register which is responsible for the return
;; argument and not use it as a source of incidents.
;; 3) we also don't want to see multiple manifestations of a single
;; dereference
;; 4) If any subroutine wasn't visited in the current path because it
;; was visited in the other, we need to drop its result.
;;

(require x86-correct-sp)

(defun notify-null-deref (start)
  (when (not (is-reported (get-current-program-counter)))
    (when start
      (msg "pointer was introduced at $0" start))
    (null-ptr-dereference)
    (incident-report 'null-pointer-dereference (incident-location))))

(defun is-null (ptr)
  (and (not ptr) (all-static-constant ptr)))

(defun is-safe (ptr)
  (or (is-untrusted)
      (taint-get-direct 'dont-believe ptr) ;; why we use direct here ??
      (taint-get-indirect 'failed ptr)))

(defun check-deref-null-ptr (ptr)
  (when (is-null ptr)
    (let ((taint (taint-get-direct 'const-ptr ptr))
          (checked (is-checked ptr)))
      (when (and taint (not (is-safe ptr)) (not checked))
        (taint-introduce-indirectly 'failed ptr 1) ;; why we use both here ??
        (taint-introduce-directly 'failed ptr)
        (notify-null-deref (dict-get 'intro taint))))))

(defmethod eval-cond (x)
  (let ((taint (taint-get-direct 'const-ptr x)))
    (when taint
      (mark-checked taint))))

(defmethod written (v x)
  (when (all-static-constant x)
    (let ((taint (taint-introduce-directly 'const-ptr x)))
      (dict-add 'intro taint (get-current-program-counter)))))

;; stored method will detach previous indirect taints by the
;; address, and we can get a manifestation of the same
;; dereference that we discovered earlier. That's why we
;; do this trick in the storing/stored methods.
(defmethod stored (a x)
  (let ((known (dict-has 'known-fail (get-current-program-counter))))
    (when known
      (taint-introduce-indirectly 'failed a 1))
    (when (and (not known) (all-static-constant x))
      (let ((taint (taint-introduce-indirectly 'const-ptr a 1)))
        (dict-add 'intro taint (get-current-program-counter))))))

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
