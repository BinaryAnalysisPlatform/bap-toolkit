#!/usr/bin/env sh

for name in `ls artifacts/`; do

    arti=artifacts/$name

    if [ ! -f $arti/artifact ]; then
        docker run --rm -v `pwd`/$arti:/drive binaryanalysisplatform/bap-artifacts:$name cp /artifact /drive
    fi

done
