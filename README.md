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
  - checks from the [Joint Strike Fighter coding standards](http://stroustrup.com/JSF-AV-rules.pdf)
    - [av-rule-3](av-rule-3/descr) - all functions have a cyclomatic complexity less than 20
    - [av-rule-17](av-rule-17/descr) - `errno` is not used as an error indicator
    - [av-rule-19](av-rule-19/descr) - `setlocale` et all functions are not be used
    - [av-rule-20](av-rule-20/descr) - `setjmp`/`longjmp` are not be used
    - [av-rule-21](av-rule-21/descr) - signal handling facilities of `<signal.h>` are not be used
    - [av-rule-22](av-rule-22/descr) - The input/output library `<stdio.h>` shall not be used
    - [av-rule-23](av-rule-23/descr) - `atof`, `atoi`, and `atol` are not be used
    - [av-rule-24](av-rule-24/descr) - `abort`, `exit`, `getenv` and `system` are not be used
    - [av-rule-25](av-rule-25/descr) - the `<time.h>` interface is not used
    - [av-rule-174](av-rule-174/descr) - potential null pointer dereferencings
    - [av-rule-189](av-rule-189/descr) - `goto` statements are not used
  - checks from the [JPL Institutional Coding Standard](http://bsivko.pbworks.com/w/file/fetch/68132300/JPL_Coding_Standard_C.pdf)
    - [jpl-rule-4](jpl-rule-4/descr) - no recursive functions
    - [jpl-rule-11](jpl-rule-11/descr) - `goto` statements are not used
    - [jpl-rule-14](jpl-rule-14/descr) - return values of all non-void functions are used
  - [defective-symbols](defective-symbol/descr) - detects all defective symbols from the av-rule-{3,17,19,20,21,22,23,24,25,189} and jpl-rule-4
  - [primus-checks](primus-checks/descr) - an all-in-one analysis that uses Primus to identify the following CWE:
    - CWE-122 (Buffer Overwrite)
    - CWE-125 (Buffer Overread)
    - CWE-416 (Use after free)
    - CWE-415 (Double free)
    - CWE-798 (Use of Hard-coded Credentials)
    - CWE-259 (Use of Hard-coded Password)
    - CWE-822 (Untrusted Pointer Dereference)
    - CWE-291 (Relience on IP Address for Authentication)
    - CWE-170 (Improper Null Termination)
    - CWE-138 (Improper Neutralization)
    - CWE-74  (Command Injection)
    - CWE-476 (NULL pointer dereference)
    - CWE-690 (Unchecked Return Value to NULL Pointer Dereference)
    - CWE-252 (Unchecked Return Value)



## Installation

The build and installation system is currently querying the opam tool for all the
necessary information, therefore make sure that opam 2.x is installed on your
system, and a switch is activated, with

        eval $(opam env)

Next, to install all tools in the repository to the default share folder just do


        python makes build
        python makes install

To install a specific tool, run the same commands but pass the tool name to them, e.g.,

        python makes build primus-checks
        python makes install primus-checks

# Usage

Tool are packed as BAP recipes,  therefore to run a tool just pass its name to the `--recipe` option, e.g.,

       bap ./exe --recipe=av-rule-3

To get a detailed description of a recipe, use the `--show-recipe` option, e.g.,

       bap --show-recipe=av-rule-3

You can also list all available using the `--list-recipes` option,

       bap --list-recipes

# Results
The results of the checks from this repository applied to [bap-artifacts](https://github.com/BinaryAnalysisPlatform/bap-artifacts) can
be seen [here](http://htmlpreview.github.io/?https://github.com/BinaryAnalysisPlatform/bap-toolkit/blob/master/results.html)


# Developing

## Creating a new tool

To create a new tool clone this repository,

      https://github.com/BinaryAnalysisPlatform/bap-toolkit.git

Then create a new folder inside the newly cloned `bap-toolkit` folder,

      cd bap-toolkit
      mkdir my-first-tool
      cd my-first-tool

All files in this folder will form the body of your tool. They may contain input
files, scripts for pre and post processing, BAP plugins and libraries, etc. The only
required file is the `recipe.scm` file which is the entry point of your tool. This
file contains a list of options which are passed to `bap`, for example, to create a
tool that just dumps a file in multiple formats, create a `recipe.scm` file with the
following contents

      (option dump asm:out.asm)
      (option dump bir:out.bir)

After the tool is [built and installed](#installation), you can run it with

      bap ./test-file --recipe=my-first-tool

And this would essentially the same as running bap with the following command line arguments

      bap ./test-file --dump=asm:out.asm --dump=bir:out.bir

Not a big deal so far, but typical bap invocation may contain lots of command line option.
You may also need to pass files, header files, BAP Lisp scripts, etc. This is where the recipe
system shines. In general, the recipe specification contains a list of recipe items in
an arbitrary order. Each item is either a command line option, a parameter, or a reference to
another recipe. All items share the same syntax - they are flat s-expressions, i.e., a whitespace
separated list of strings enclosed in parentheses. The first string in the list denotes the type
of the item, e.g.,

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
