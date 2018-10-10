#!/bin/sh

# installs recipes to the specified folder
# Synopsis
#   install.sh <destination>
#   install.sh <source> <destination>

if [ "no-$2source" = "no-source" ]; then
    echo "no source"
    SRC=`ls`
    DST=$1
    ALL=true
else
    SRC=$1
    DST=$2
    ALL=false
fi

if [ "no-$2source" = "no-source" ]; then
    echo "Warning: no destination is specified, using OPAM to guess"
    DST="`opam config var prefix`/share/bap"
fi


echo "installing to $DST"


for src in $SRC; do
    if [ -d "$src" ] && [ -e $src/recipe.scm ]; then
        echo "installing $src"
        cp -R "$src/" "$DST/$src.recipe"
    elif [ -d "$src" ] && [ $ALL = true ]; then
        echo "Error: $src is not a recipe specification"
        exit 1
    fi
done
