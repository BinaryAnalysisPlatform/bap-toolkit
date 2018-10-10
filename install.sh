#!/bin/sh

# installs recipes to the specified folder
# Synopsis
#   install.sh <destination>
#   install.sh <source> <destination>

if [ "no-$2source" = "no-source" ]; then
    SRC=`ls`
    DST=$1
    ALL=true
else
    SRC=$1
    DST=$2
    ALL=false
fi

if [ "no-$1dest" = "no-dest" ]; then
    PREFIX="`opam config var prefix`" 2>/dev/null

    if [ "no-$DST-opam" = "no-opam" ]; then
        DST=/usr/local/share/bap
    else
        DST=$PREFIX/share/bap
    fi
fi


echo "installing to $DST"

if [ ! -d $DST ]; then
    echo "The destination either doesn't exist or not a folder"
    echo "Please, create the desintation, e.g.,"
    echo "mkdir -p $DST"
    exit 1
fi

if [ -w $DST ]; then
    CP="cp"
else
    echo "'sudo' will be used for installation..."
    CP="sudo cp"
fi


for src in $SRC; do
    if [ -d "$src" ] && [ -e $src/recipe.scm ]; then
        echo "installing $src"
        $CP -R "$src/" "$DST/$src.recipe"
    elif [ -d "$src" ] && [ $ALL = true ]; then
        echo "Error: $src is not a recipe specification"
        exit 1
    fi
done
