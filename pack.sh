#!/bin/sh

# packs a recipe folder into a zip file for the ease of sharing
# Usage: pack <path-to-the-recipe-folder>

OLD=`pwd`
NAME=`basename "$1"`
cd $1
zip -r $NAME.recipe *
mv $NAME.recipe $OLD
cd $OLD
