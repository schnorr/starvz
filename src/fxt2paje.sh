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

# get all the FXTs
FXTS=$(ls -1 prof_file_* | sort --version-sort)

# call the conversion
starpu_fxt_tool -memory-states $(echo $FXTS | sed -e "s/ / -i /g" -e "s/^/-i /")

POTI="false"

if ldd $(which starpu_fxt_tool) | grep -q "poti"; then
  POTI="true"
fi

# sort the file
paje_sort.sh $POTI paje.trace > paje.sorted.trace

