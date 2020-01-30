#!/usr/bin/env sh

gen() {
    echo "FROM binaryanalysisplatform/bap:testing as base"
    echo
    echo "COPY --chown=opam:nogroup src /src"
    echo "WORKDIR /src"
    echo "RUN eval \"\$(opam env)\" && make build"
    echo
    echo "FROM binaryanalysisplatform/bap-toolkit:testing as toolkit"
    echo
    echo "COPY . /tests"
    echo "WORKDIR /tests"
    echo "COPY --from=base /src/compare-incidents /tests"

    for name in `ls artifacts/`; do
        arti=artifacts/$name
        echo "COPY --from=binaryanalysisplatform/bap-artifacts:$name /artifact $arti"

    done

    echo "RUN sh run.sh"
}

gen > Dockerfile
