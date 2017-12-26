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
starpu_fxt_tool $(echo $FXTS | sed -e "s/ / -i /g" -e "s/^/-i /")

# sort the file
paje_sort.sh true paje.trace > paje.sorted.trace
