(parameter depth 4096 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")

(option primus-lisp-load
        unused-return
        posix)

(option passes
        callsites
        run)

(option primus-taint-gc conservative)
(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)
(option run-entry-points ${entry-points})

(option primus-lisp-add $prefix)
(option primus-print-output unused-result.incidents)
(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout
  <stderr>:$prefix/stderr)

(option primus-print-observations
        all
        -const
        -enter-exp
        -leave-exp)
