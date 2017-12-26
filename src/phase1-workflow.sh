#!/bin/bash

DIR=$(dirname $0)

#
# This script gets a directory with FXT files as input
#

##############################
# Usage                      #
##############################
function usage()
{
    echo "$0 <fxtdir> <application>";
    echo "  where <fxtdir> is a directory with prof_file_* FXT files from StarPU";
    echo "  where <application> is either cholesky or qrmumps";
}

##############################
# Parameters                 #
##############################
CASE=${1:-}
if [ -z "$CASE" ]; then echo "Error: <fxtdir> is empty"; usage; exit; fi
APPLICATION=${2:-}
if [ -z "$APPLICATION" ]; then echo "Error: <application> is empty"; usage; exit; fi

echo
echo "Start of $CASE"
echo

pushd $CASE

echo "Convert from FXT to paje.sorted.trace"
date

fxt2paje.sh
rm -f paje.trace

# Put Lionel's pmtool to get bounds to enrich the visualization
# Also create a hypothetical trace from the solution of the LP

rm -f activity.data distrib.data trace.html tasks.rec data.rec trace.rec

echo "Convert from paje.sorted.trace to paje.csv"
date

pj_dump -o -n --user-defined --entity-hierarchy=entities.csv --type-hierarchy=types.csv paje.sorted.trace > paje.csv
rm -f paje.sorted.trace

echo "Get states, links and variables in CSV"
date

PAJESTATE=paje.state.csv
echo "Nature, ResourceId, Type, Start, End, Duration, Depth, Value, Size, Params, Footprint, Tag, JobId, SubmitOrder, GFlop, X, Y, Iteration, Subiteration" > $PAJESTATE
cat paje.csv | grep ^State >> $PAJESTATE

PAJEVARIABLE=paje.variable.csv
echo "Nature, ResourceId, Type, Start, End, Duration, Value" > $PAJEVARIABLE
cat paje.csv | grep ^Variable >> $PAJEVARIABLE

PAJELINK=paje.link.csv
echo "Nature, Container, Type, Start, End, Duration, Size, Origin, Dest, Key" > $PAJELINK
cat paje.csv | grep ^Link >> $PAJELINK

rm -f paje.csv

echo "Convert (DAG) DOT to CSV"
date

OUTPUTDAGCSV=dag.csv
echo "Dependent,JobId" > $OUTPUTDAGCSV
cat dag.dot | \
    grep "\->" | \
    sed -e "s/[[:space:]]//g" -e "s/\"//g" -e "s/task_//g" -e "s/->/,/" | \
    sed -e "s/[[:space:]]//g" | \
    sort >> $OUTPUTDAGCSV
rm -f dag.dot

echo "Convert (ATREE) DOT to CSV"
ATREE=atree.dot
if [ -e "${ATREE}" ]; then
    OUTPUTATREECSV=atree.csv
    echo "Node,DependsOn" > $OUTPUTATREECSV
    cat $ATREE | grep - "--" | sed -e "s/ -- /,/" -e "s/node//g" -e "s/^0*//" -e "s/,0*/,/" >> $OUTPUTATREECSV
fi

echo "Post-processing CSV files"
../R/phase1-workflow.R . ${APPLICATION}
rm -f atree.csv dag.csv entities.csv paje.link.csv paje.state.csv paje.variable.csv types.csv

echo
echo "End of $CASE"
date
echo

popd
