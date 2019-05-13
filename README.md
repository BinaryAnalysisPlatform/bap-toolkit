Note: this repository is currently being updated, expect lots of changes in the upcoming few days. This README
is also WIP.

# Introduction

This repository provides a collection of ready to use binary analysis tools,
as well as a framework and a conventional repository structure for developing
new tools. Think of it as BAP on Rails. This repository should be seen as a collaboration
platform encouraging everyone to fork it, implement an analysis, and share it back with
the community. PRs are very welcomed and accepted with no questions asked.

# Table of Contents

- [Installation](#installation) - how to install all or some tools
- [Usage](#usage) - how to run tools and analyze results
- [Developing](#developing) - how to develop a new tool
- [Contributing](#contributing) - how to contribute a new tool
- Tools
  - [primus-checks](#primus-checks) - all-in-one analysis that uses Primus
  - checks from [Joint Strike Fighter coding standards](http://stroustrup.com/JSF-AV-rules.pdf)
    - [av-rule-3](#av-rule-3) - All functions shall have a cyclomatic complexity number of 20 or less
    - [av-rule-17](#av-rule-17) - The error indicator `errno` shall not be used
    - [av-rule-19](#av-rule-19) - `<locale.h>` and the `setlocale` function shall not be used.
    - [av-rule-20](#av-rule-20) - The `setjmp` macro and the `longjmp` function shall not be used.
    - [av-rule-21](#av-rule-21) - The signal handling facilities of `<signal.h>` shall not be used.
    - [av-rule-22](#av-rule-22) - The input/output library `<stdio.h>` shall not be used.
    - [av-rule-23](#av-rule-23) - The library functions `atof`, `atoi` and `atol` from library `<stdlib.h>` shall not be used.
    - [av-rule-24](#av-rule-24) - The library functions `abort`, `exit`, `getenv` and `system` from library `<stdlib.h>` shall not be used
    - [av-rule-25](#av-rule-25) - The time handling functions of library `<time.h>` shall not be used.
    - [av-rule-174](#av-rule-174) - The null pointer shall not be de-referenced.
    - [av-rule-189](#av-rule-189) - The goto statement shall not be used
  - checks from [JPL Institutional Coding Standard](http://bsivko.pbworks.com/w/file/fetch/68132300/JPL_Coding_Standard_C.pdf)
    - [jpl-rule-4](#jpl-rule-4) - There shall be no direct or indirect use of recursive function calls
    - [jpl-rule-11](#jpl-rule-11) - The goto statement shall not be used
    - [jpl-rule-14](#jpl-rule-14) - The return value of non-void functions shall be checked or used
  - [defect-symbol](#defect-symbol) - run the next checks all at once: av-rule-{3,17,19,20,21,22,23,24,25,189} and jpl-rule-4



## Installation

Although the recipes from this repository are installed by default in
binary and opam installations, it is useful to update them, as it is this
repository could move faster than the BAP release cycle. To install all
recipes to the default share folder just do


        ./install.sh


The script will install to the currently activated OPAM switch, if such
exits, otherwise it will install to the `/usr/local/share/bap` folder. To install
to a specific folder just pass it to the script, e.g.,

       ./install.sh <destination>

To install a specific recipe pass its name (the folder name) after the destination,
e.g.,

       ./install.sh <destination> <recipe>


# Usage

To use the installed recipe just pass its name to the `--recipe` option, e.g.,

       bap ./exe --recipe=primus-checks


To list available recipes, use

       bap --list-recipes

To peek into the details of a recipe pass its name to the `--show-recipe` option, e.g.,

       bap --show-recipe=primus-checks

If a recipe has parameters then they could be specified as colon
separated list of <key>=<value> pairs. See the --recipe parameter in
`bap --help` for more information.


# Developing

## Making Recipes

A recipe is either a single file or a directory (optionally zipped)
that contains a parametrized specification of command line parameters
and support files if necessary.

The main (and the only necessary) part of a recipe is the recipe
specification, that is a file that contains a list of recipe items in
an arbitrary order. Each item is either a command line option, a
parameter, or a reference to another recipe. All items share the same
syntax - they are flat s-expressions, i.e., a whitespace separated list
of strings enclosed in parentheses. The first string in the list
denotes the type of the item, e.g.,

        (option run-entry-points malloc calloc free)


The `option` command requires one mandatory parameter, the option name,
and an arbitrary number of arguments that will be passed to the
corresponding command line option. If there are more than one argument
then they will be concatenated with the comman symbol, e.g.,

        (option opt a b c d)

will be translated to

        --opt=a,b,c,d

Option arguments may contain _substitution symbols_. A subsitution
symbol starts with the dollar sign, that is followed by a named
(optionally delimited with curly braces, to disambiguate it from the
rest of the argument). There is one built in parameter `prefix`,
that is substituted with the path to the recipe top folder.

The `parameter` command introduces a parameter to the recipe, i.e., a
variable ingredient that could be changed when the recipe is used. The
`parameter` command has 3 arguments, all required. The first argument is
the parameter name, the second is the default value, that is used if
the parameter wasn't set, and the last argument is the parameter
description. The substitution symbol will be replaced with the default
value of a parameter, if a value of the parameter wasn't passed through
the command line. Example,

    (parameter depth 128 "maximum depth of analysis")
    (option analysis-depth $depth)


If the parameter is not set through the command line, then it will be
substituted with `128` otherwise it will receive whatever value a user
has passed.

Finally, the `extend` command is like the `#include` statement in the C
preprocessor as it includes all the ingredients from another
recipe. (Make sure that you're not introducing loops!). The command
has one mandatory argument, the name of the recipe to include.

## The recipe file grammar

           recipe ::= {<recipe-item>}
           recipe-item ::= <option> | <parameter> | <extend>
           option ::= (option <atom> {<atom>})
           parameter ::= (parameter <atom> <atom> <atom>)
           extend ::= (extend <atom>)
