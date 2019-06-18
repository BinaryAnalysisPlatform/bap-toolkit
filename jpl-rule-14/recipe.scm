(parameter depth 4096 "a depth of analysis")
(parameter loops 1024 "a maximum number of executions of the same block in a given machine")
(parameter entry-points all-subroutines "where to search")
(parameter incidents incidents "a path to file with incidents")

(option primus-lisp-load
        unused-return
        posix)

(option passes
        callsites
        run)
(option unused-return-value-enable)

(option primus-taint-gc conservative)
(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)
(option primus-limit-max-visited $loops)
(option run-entry-points ${entry-points})

(option primus-lisp-add $prefix)
(option primus-print-output $incidents)
(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout
  <stderr>:$prefix/stderr)

(option primus-print-observations
        pc-changed
        jumping
        call
        call-return
        machine-switch
        machine-fork
        lisp-message
        incident
        incident-location)
