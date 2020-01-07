
# Intro

We base our tests on the incidents comparison, and check new obtained incidents against the expected ones.
There are the next three possible outcomes of the comparison:
- false positive is a presence of an incident while there is no confirmed bug in the code.
- false negative is an absence of an incident while there is a confirmed bug in the code.
- true  positive is a presence of an incident while there is a confirmed bug in the code.

The fourth case, true negative, when there is an absence of an incident while there is
no confirmed bug in the code, is hardly can be reachable.

An expected incident is such one that was checked manually and points to the real bug, i.e.
is a real true positivite.

Tests are devided into two categories: concepts and artifacts.


## Concepts
Concept is a small program that covers only one check and usually contains just one simple test case
that trigger only one incident. Given a simplicity of the test case, we don't allow neither false
positivites nor false negativites and require 100% match between expected and testing set of the incidents.
Concept can't be used as a proof of a check's reliability, but serves as a fast test for
an idea that stands behind it.

For instance, the concept for null pointer dereference contains two cases:
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
Our approach to this check is that if null pointer was checked - even
in wrong way, like in the example above - then the usage of the pointer is
considered as a safe one.
And vice versa - if there was no check on the pointer, before derefence                                                                 then we trigger an incident.
Thus, if we have an incident by an address corresponded to line '5',
we consider it as a false positive, meaning something went wrong.
The same, if we don't have an incident on line `11`, we consider is
as false negative - and again it means that we have a bug in our implementation.
(or tests are outdated and need to be reviewed).


## Artifacts
Artifacts contains way more number of test cases and therefore quite useful for us.
Some of them were taken from Juliet test set, some of them are real programs with
known bugs. Given that, we slightly relax our comparison rules and allow checks
to triger false positivites, though the bugs must be still discovered,
i.e. expected incidents must be a subset of the new obtained incidents.
