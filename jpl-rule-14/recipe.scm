(parameter depth 32768 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")
(parameter verbosity 1 "verbosity level")

(option primus-lisp-load
        unused-return
        posix)

(option passes
        callsites
        run)

(option unused-return-value-enable)
(option unused-return-value-verbose $verbosity)

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
        exception
        pc-changed
        jumping
        call
        call-return
        machine-switch
        machine-fork
        lisp-message
        incident
        incident-location)

(option log-dir log)
