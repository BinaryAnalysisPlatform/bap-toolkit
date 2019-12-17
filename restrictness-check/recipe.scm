(parameter depth 4096 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")

(option primus-lisp-load
        posix
        restrict)

(option passes
        with-no-return
        callsites
        run)

(option restrictness-check-enable)

(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)

(option run-entry-points ${entry-points})

(option primus-lisp-add $prefix)
(option primus-print-output incidents)
(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout
  <stderr>:$prefix/stderr)

(option primus-print-observations
all)
        ;; exception
        ;; pc-changed
        ;; jumping
        ;; call
        ;; call-return
        ;; machine-switch
        ;; machine-fork
        ;; lisp-message
        ;; incident
        ;; incident-location)

(option log-dir log)
