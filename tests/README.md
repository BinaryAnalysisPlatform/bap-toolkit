
# bap-toolkit tests

### Structure
First of all, every directory in this folder corresponds to the name of a recipe,
and every file inside of it to the name of an artifact to check.
Artifacts themselfs are taken from the `binaryanalysisplatform/bap-artifacts` repository.
Every file (artifact name) contains a list of addresses where
analysis from recipe must trigger a result. For example, it could be an
address where null pointer dereference occures or an address of a
recursive function - a semantic of these addreses depends from every
particular recipe.

### How do we build test cases

Test case consists from the next levels, from down to top:

- docker container with an artifact from the `binaryanalysisplatform/bap-artifacts` repository
- docker container with all the `bap-toolkit` recipes and bap included
- auto generated `Tcl-Expect` test
- auto generated docker file that runs the `Tcl-Expect` test on the image build stage.

A Tcl/expect test case checks that every address in the addresses list
is present either in the `incident` file for primus-based checks or in the `bap`
output for others.

### Run
`$ make` will build and run all the tests. One can set a desired recipe(s) to check:
`$ make test recipe1 recipe2`
