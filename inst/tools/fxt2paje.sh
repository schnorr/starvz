#!/bin/bash
set -o pipefail
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

if [ -z "$STARVZ_USE_PAJE_TRACE" ] || [ "$STARVZ_USE_PAJE_TRACE" -ne 1 ];
then 
    ls -1 prof_file_* > /dev/null
    es=$?
    if [ $es -ne 0 ]
    then
	echo "The directory dont have prof_file_* files"
	exit 1
    fi

    # get all the FXTs
    FXTS=$(ls -1 prof_file_* | sort --version-sort)

    echo "Execute stapu_fxt_tool"
    date "+%a %d %b %Y %H:%M:%S %Z"
    # call the conversion
    starpu_fxt_tool $STARPU_FXT_OPTIONS $(echo $FXTS | sed -e "s/ / -i /g" -e "s/^/-i /") -o /dev/stdout\
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
else
    if [ -f "paje.trace" ] && [ -f "data.rec" ] && [ -f "tasks.rec" ] && [ -f "dag.dot" ];
    then
	POTI="false"
	cat paje.trace | gzip -c > paje.trace.gz
    else
	echo "Error. Some files are missing, << --use-paje-trace|-t >> option requires paje.trace data.rec tasks.rec dag.dot files."
	exit 2
    fi
fi

echo "Sort paje.trace"
date "+%a %d %b %Y %H:%M:%S %Z"
# sort the file
paje_sort.sh $POTI paje.trace.gz | gzip -c > paje.sorted.trace.gz
