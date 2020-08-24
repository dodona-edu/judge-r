#!/bin/sh

set -e

path_to_exercise="$1"

[ -z "$path_to_exercise" ] \
    && echo 'Provide a path to an exercise as first argument.' \
    && exit 1

filename="$2"

[ -f "$path_to_exercise/workdir/$filename"] \
    && echo 'Provide a path to an exercise as first argument.' \
    && exit 1

sh "./run" <<HERE
{
    "resources": "$path_to_exercise/evaluation",
    "judge": ".",
    "source": "$path_to_exercise/workdir/$filename"
}
HERE

echo