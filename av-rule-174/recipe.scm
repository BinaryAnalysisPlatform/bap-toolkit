(parameter depth 4096 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")

(option primus-lisp-load
        posix
        check-deref)

(option run)
(option run-entry-points ${entry-points})
(option constant-tracker-enable)

(option primus-lisp-add $prefix)
(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)
(option primus-print-output null-ptr-deref.incidents)

(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout
  <stderr>:$prefix/stderr)

(option primus-print-observations
        all
        -const
        -enter-exp
        -leave-exp)
