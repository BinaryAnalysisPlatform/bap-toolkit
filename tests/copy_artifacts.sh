#!/usr/bin/env sh

tags=`wget -q https://registry.hub.docker.com/v1/repositories/binaryanalysisplatform/bap-artifacts/tags -O -  | sed -e 's/[][]//g' -e 's/"//g' -e 's/ //g' | tr '}' '\n'  | awk -F: '{print $3}'`


tag_exists=false

check_tag() {
    tag_exists=
    for t in $tags; do
        if [ "tag$1" = "tag$t" ]; then
            tag_exists=true
        fi
    done
}

for name in `ls artifacts/`; do

    arti=artifacts/$name
    check_tag $name
    if [ ! -f $arti/artifact ] && [ "checked_$tag_exists" = "checked_true" ]; then
        docker run --rm -v `pwd`/$arti:/drive binaryanalysisplatform/bap-artifacts:$name cp /artifact /drive
    fi

done
