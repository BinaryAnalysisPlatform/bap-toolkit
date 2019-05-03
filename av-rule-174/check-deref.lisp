
(require taint)

(defun notify-null-deref ()
  (null-ptr-dereference)
  (incident-report 'null-pointer-dereference (incident-location)))

(defun check-deref-null-ptr (ptr)
  (let ((taint (taint-get-direct 'const-ptr ptr))
        (checked (dict-has 'checked taint)))
    (when (and taint (not checked))
      (notify-null-deref))
    (when (and taint checked)
      (let ((addr (dict-get 'checked taint))
             (dependent (has-control-dependency addr)))
        (when (not dependent)
          (notify-null-deref))))))

(defmethod eval-cond (x)
  (let ((taint (taint-get-direct 'const-ptr x))
        (addr (get-current-program-counter)))
    (when taint
      (dict-add 'checked taint addr))))

(defmethod stored (a x)
  (when (all-static-constant x)
    (taint-introduce-directly 'const-ptr x)))

(defmethod loading (ptr)
  (check-deref-null-ptr ptr))

(defmethod storing (ptr)
  (check-deref-null-ptr ptr))
