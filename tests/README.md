
# Intro

Tests are based on the incidents comparison: a new obtained set of incidents is
compared with an expected one.
There are the next three possible outcomes of the comparison:
- false positive is a presence of an incident when there is no confirmed bug in the code.
- false negative is an absence of an incident when there is a confirmed bug in the code.
- true  positive is a presence of an incident when there is a confirmed bug in the code.

An expected incident is such one that was checked manually and points to the real bug, i.e.
is a real true positivite.

Tests are devided into the two categories: litmuses and artifacts.


## Litmus tests
Litmus is a small program that covers only one check and usually contains just one simple test case
that trigger only one incident. Given a simplicity of the test case, we don't allow neither false
positivites nor false negativites and require 100% match between expected and testing set of the incidents.
Litmus can't be used as a proof of a check's reliability, but serves as a fast test for
an idea that stands behind it.

For instance, the litmus for null pointer dereference contains two cases:
```
1.  void good() {
2.     int *x = NULL;
3.     int b = 0;
4.     if (!x)
5.         b = *x;
6.  }
7
8.  void bad() {
9.     int *x = NULL;
10.    int b = 0;
11.    b = *x;
12. }
```
Our approach to this check is that if a pointer was checked - even in wrong way, like in the example
above - then the usage of the pointer is considered as a safe one. And vice versa - if there was no
check on the pointer, before derefence then we trigger an incident.
Thus, if we have an incident by an address corresponded to line '5', we consider it as a false positive,
meaning something went wrong. The same, if we don't have an incident on line `11`, we consider is
as false negative - and again it means that we have a bug in our implementation.
(or tests are outdated and need to be reviewed).

### Adding a new litmus
To add a new litmus test for the check named `foo`, one need to:
1) add a source code to `tests/litmus-tests/src/foo.c`
2) add a binary to `tests/litmus-tests/bin/foo`
3) add expected incidents into `tests/litmus-tests/data/foo/expected`


## Artifacts
Artifacts are more complex programs then litmuses and therefore it's harder to find a bug there.
Some of them were taken from Juliet test set, some of them are real programs with known bugs.
Given that, we slightly relax our comparison rules and allow checks to triger false positivites,
though the bugs must be still discovered, i.e. expected incidents must be a subset of the new obtained
incidents.

### Adding a new artifact

Although we can customize it in future, but right now tests depend on the
`binaryanalysisplatform/bap-artifacts` docker hub repository, where each artifact is represented by a tag,
e.g.: `binaryanalysisplatform/bap-artifacts:cron-3.0pl1-127`.

Basicaly, to add a new artifact `foo` one need to:
1) make `docker push binaryanalysisplatform/bap-artifacts:foo`
2) create a folder `tests/artifacts/foo`
3) create a subfolder with a desired check name (it's used for display purposes only), e.g. `tests/artifacts/foo/BAR`
4) create a `tests/artifacts/foo/BAR/run` file with parameters to `bap --recipe=` option
5) create a `tests/artifacts/foo/BAR/expected` file expected incidents

Each artifact can contain more than one check, e.g. `libssh-0.5.2`.

# Log
For the debug purposes, log is stored in `tests/toolkit.log` file, which contains basic
information for every check: time started/finished, status, missed incidents if any.


# Expected results

```
                             LITMUS TESTS
av-rule-17 .................................................. PASS
av-rule-174 ................................................. PASS
av-rule-189 ................................................. PASS
av-rule-19 .................................................. PASS
av-rule-20 .................................................. PASS
av-rule-21 .................................................. PASS
av-rule-22 .................................................. PASS
av-rule-23 .................................................. PASS
av-rule-24 .................................................. PASS
av-rule-25 .................................................. PASS
av-rule-3 ................................................... PASS
double-free ................................................. PASS
heap-overflow ............................................... PASS
jpl-rule-14 ................................................. PASS
jpl-rule-4 .................................................. PASS
must-check-value ............................................ PASS
restrictness-check .......................................... PASS
untrusted-argument .......................................... PASS
use-after-free .............................................. PASS
warn-unused ................................................. PASS

                         ARTIFACTS (patience!)
CVE-2019-9706 ............................................... PASS
CVE-2017-1000421 ............................................ PASS
juliet-cwe-122/heap-overflow ................................ PASS
juliet-cwe-252/jpl-rule-14 .................................. PASS
juliet-cwe-415/double-free .................................. PASS
juliet-cwe-416/use-after-free ............................... PASS
juliet-cwe-476/av-rule-174 .................................. PASS
CVE-2018-1000222 ............................................ PASS
CVE-2012-4559 ............................................... PASS
CVE-2012-6063 ............................................... PASS
CVE-2019-8936 ............................................... PASS
CVE-2019-8377 ............................................... PASS

OK
```
