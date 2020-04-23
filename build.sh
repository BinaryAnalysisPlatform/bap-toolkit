#!/bin/sh

PLUGIN_MAIN=""
PLUGIN_DEPS=""
PLUGIN_DESC=""
HAS_BUILD_TOOLS="FALSE"

# checks that necessary build tools are installed
check_build_tools() {
    if [ "no-$(which bapbuild)" != "no-" ] && [ "no-$(which bapbundle)" != "no-" ]; then
        HAS_BUILD_TOOLS="TRUE"
    fi
}

# reads line env variable and extracts main, depends, descr fields
parse_line() {
    field=`echo $line | cut -d':' -f1 | sed 's/^[  *]//g;s/[  *]*$//'`

    if [ "no-$field" != "no-" ]; then
        if [ "$field" = "main" ]; then
            PLUGIN_MAIN=`echo $line | cut -d':' -f2`
        fi
        if [ "$field" = "depends" ]; then
            PLUGIN_DEPS=`echo $line | cut -d':' -f2 | sed 's/^[  *]//g'`
        fi
        if [ "$field" = "descr" ]; then
            PLUGIN_DESC=`echo $line | cut -d':' -f2`
        fi
    fi
}

# reads info file if exists
# pre: already in the directory with a check
read_info() {
    unset PLUGIN_MAIN
    unset PLUGIN_DEPS
    unset PLUGIN_DESC

    PLUGIN_MAIN=`echo $1 | sed 's/-/_/g'`
    if [ -e info ]; then
        while IFS= read -r line
        do
            parse_line $line
        done < info
        parse_line $line
    fi
}


# packs a recipe folder into a zip file for the ease of sharing
# Usage: pack_recipe <path-to-the-recipe-folder>
# pre: already in the directory with a check
pack_recipe() {
    NAME=`basename "$1"`
    recipe=`find . -name "*.scm" | head -n 1`
    if [ "no-$recipe" != "no-" ]; then
        zip -r $NAME.recipe * -x \*.ml  \*.mli \*.plugin _build/*
    fi
}

build() {
    OLD=`pwd`
    cd $1
    read_info $1
    mlfiles=`find . -name "*.ml" | head -n 1`

    if [ "no$mlfiles" != "no" ]; then
        if [ "$HAS_BUILD_TOOLS" = "TRUE" ]; then


           NAME=`echo "$PLUGIN_MAIN" | cut -d'.' -f1`
           DEPS=`echo "$PLUGIN_DEPS" | sed 's/  */,/g' `

           if [ "has-no$DEPS" != "has-no" ]; then
               echo bapbuild -pkgs $DEPS $NAME.plugin
               bapbuild -pkgs $DEPS $NAME.plugin
           else
               bapbuild $NAME.plugin
           fi

           if [ "no-$PLUGIN_DESC" != "no-" ]; then
               bapbundle update -desc \"$PLUGIN_DESC\" $NAME.plugin
           fi
        else
            echo "SKIP TARGET $1: bapbuild/bapbundle not found"
        fi
    fi

    pack_recipe $1
    cd $OLD
}


install() {
    OLD=`pwd`
    cd $1

    DST=`opam config var prefix`/share/bap
    plugin=`find . -name "*.plugin" | head -n 1`
    recipe=`find . -name "*.recipe" | head -n 1`

    if [ "no-$plugin" != "no-" ]; then
        bapbundle install $plugin
    fi
    if [ "no-$recipe" != "no-" ]; then
        cp *.recipe $DST
    fi
    cd $OLD
}


clean() {
    OLD=`pwd`
    cd $1
    rm -f  *.recipe
    bapbuild -clean > /dev/null
    cd $OLD
}


process_cmd() {
    CMD=$1
    DST=$2
    case $CMD in
        build)
            build $DST
            ;;
        install)
            install $DST
            ;;
        clean)
            clean $DST
            ;;
        *)
            echo "Usage: $0 {build|install|clean} with an optional check name "
            exit 1
    esac

}

check() {
    path=`which $1`
    if [ "no$path" = "no" ]; then
        echo "can't find $1, exiting ... "
        exit 1
    fi
}

# usage:
# run <build|install|clean> check
run() {
    check zip
    check sed
    check bapbundle
    check bapbuild

    COMMAND=$1
    TARGET=$2
    check_build_tools

    if [ "all$TARGET" = "all" ]; then
        for d in `ls`; do
            if [ -d "$d" ] && [ "$d" != "tests" ]; then
                process_cmd $COMMAND $d
            fi
        done
    else
        process_cmd $COMMAND $TARGET
    fi
}

run "$@"
