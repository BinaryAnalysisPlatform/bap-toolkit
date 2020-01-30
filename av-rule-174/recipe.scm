(parameter depth 4096 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")
(parameter optimization 2 "optimization level")
(parameter verbosity 1 "verbosity level")

(option passes
        with-no-return
        run)

(option primus-lisp-load
        posix
        check-deref)

(option api-path $prefix/api)

(option run-entry-points ${entry-points})

(option constant-tracker-enable)
(option report-progress)
(option null-ptr-deref-enable)
(option null-ptr-deref-verbose $verbosity)

(option primus-lisp-add $prefix)
(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)
(option primus-print-output incidents)

(option optimization-level $optimization)

(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout
  <stderr>:$prefix/stderr)

(option primus-print-observations
        written
        stored
        exception
        pc-changed
        jumping
        call
        call-return
        lisp-message
        machine-switch
        machine-fork
        incident
        incident-location)

(option log-dir log)
