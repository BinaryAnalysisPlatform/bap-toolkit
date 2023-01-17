#!/usr/bin/env sh

ENDPOINT=https://registry.hub.docker.com/v2/repositories/binaryanalysisplatform/bap-artifacts/tags
TAGS=$(curl -L $ENDPOINT)


for name in `ls artifacts/`; do
    arti=artifacts/$name
    tag_exists=$(echo $TAGS | jq 'any(.results[].name; . == $x)' --arg x $name)
    echo "tag $name exists = $tag_exists"
    if [ ! -f $arti/artifact ] && [ "checked_$tag_exists" = "checked_true" ]; then
        docker run --rm -v `pwd`/$arti:/drive binaryanalysisplatform/bap-artifacts:$name cp /artifact /drive
    else
        echo "skipping $name"
    fi

done
