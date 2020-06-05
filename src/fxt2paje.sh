#!/bin/bash

DIR=$(dirname $0)

#
# This script converts all FXT files in the current directory to Paje
#

##############################
# Usage                      #
##############################
function usage()
{
    echo "$0";
    echo "  no parameters are required"
}

ls -1 prof_file_* > /dev/null
es=$?
if [ $es -ne 0 ]
then
    echo "The directory dont have prof_file_* files"
    exit 1
fi

# get all the FXTs
FXTS=$(ls -1 prof_file_* | sort --version-sort)

# call the conversion
# -memory-states
starpu_fxt_tool $(echo $FXTS | sed -e "s/ / -i /g" -e "s/^/-i /") -o /dev/stdout\
         | gzip -c > paje.trace.gz
es=$?
if [ $es -ne 0 ]
then
    echo "Error when executing starpu_fxt_tool (exit status: $es)"
    exit 2
fi

POTI="false"

if ldd $(which starpu_fxt_tool) | grep -q "poti"; then
  POTI="true"
fi

# sort the file
paje_sort.sh $POTI paje.trace.gz | gzip -c > paje.sorted.trace.gz
