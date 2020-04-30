(defmethod spectre-hypot-partial (c)
  (spectre/register c))

(defmethod spectre-hypot-init (c)
  (spectre/register c))

(defmethod spectre-path (t c l s)
  (incident-report 'spectre-pattern
                   (dict-get 'taint-sources/untrusted t)
                   (dict-get 'spectre/locations c)
                   (dict-get 'spectre/locations l)
                   (incident-location)))

(defun spectre/register (c)
  (dict-add 'spectre/locations c (incident-location)))


(defun check-fail ()
  (declare (external "__stack_chk_fail"))
  (error "stack check failed"))
