#!/usr/bin/env bash

set -e

while getopts ":h" option; do
   case $option in
      h) # display Help
         echo "This is a script for running the R-judge local"
         echo
         echo "Syntax: test.sh [-h] PATH_TO_EXERCISE_DIR PATH_TO_DUMP_DIR [PATH_TO_SOLUTION_FILE]"
         printf "Param:
\tPATH_TO_EXERCISE_DIR    path to the exercise folder
\t[PATH_TO_DUMP_DIR]      optional path to the a folder where the exercise will be solved and stored,
\t                        deault is set to \"./tmp\"
\t[PATH_TO_SOLUTION_FILE] optional path to the solution file, default is set to
\t                        PATH_TO_EXERCISE_DIR/solution/solution.en.R\n"
         echo "options:"
         echo "h     Print this Help.\n"
         exit;;
   esac
done

path_to_exercise="$1"
[ -z "$path_to_exercise" ] || [ ! -d "$path_to_exercise" ] \
    && echo 'Provide a path to an exercise as first argument.' \
    && exit 1

test_dir=${2:-"./tmp"}
[ ! -d "$test_dir" ]\
    && echo 'Provide a path to a testing directory where the exercise will be solved and stored as second argument.' \
    && exit 1

path_to_solution=${3:-"$path_to_exercise/solution/solution.en.R"}
[ ! -f "$path_to_solution" ] \
    && echo "No solution found at $path_to_solution" \
    && exit 1

#find next available name in the test directory
number=0
base_name=$(basename "$path_to_exercise")
fname=$base_name
while [ -e "$test_dir/$fname" ]; do
    printf -v fname '%s-%02d' "$base_name" "$(( ++number ))"
done
mkdir "$test_dir/$fname/"
cp -R "$path_to_exercise/." "$test_dir/$fname/"

Rscript "../run" <<HERE
{
    "resources": "$test_dir/$fname/evaluation",
    "judge": "..",
    "source": "$path_to_solution"
}
HERE

echo
