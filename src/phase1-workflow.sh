#!/bin/bash

DIR=$(pwd)/$(dirname $0)
export PATH="$PATH:$DIR"
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
#if [ -z "$APPLICATION" ]; then echo "Error: <application> is empty"; usage; exit; fi

echo
echo "Start of $CASE"
echo

pushd $CASE

echo "Convert from FXT to paje.sorted.trace"
date

fxt2paje.sh
es=$?
if [ $es -ne 0 ]
then
    echo "ERROR: conversion from FXT failed! (exit status: $es)"
    exit 1
fi
rm -f paje.trace

# Put Lionel's pmtool to get bounds to enrich the visualization
# Also create a hypothetical trace from the solution of the LP

PMTOOLCSV=""

# Checking if Lionel's pmtool command and platform_file.rec file are available & accessible
if [ -x "$(command -v pmtool)" ] && [ -f "platform_file.rec" ]; then
  PMTOOLOUT="pmtool.csv"

  # This command should be executed with the cluster starpu.
  #starpu_perfmodel_recdump tasks.rec -o platform_file.rec

  # Running pmtool with default-normal configuration
  echo "Execute pmtool"
  pmtool -p platform_file.rec tasks.rec -d fast -a dmdas --threads --no-header -w -s pmtool_states.out > pmtool.out 2> /dev/null

  # Cleaning pmtools bounds.
  echo "Alg,Time" > $PMTOOLOUT
  cat pmtool.out | awk '{ print $(NF-2), $(NF-1)}' | sed -e 's/[[:space:]]/,/g' >> $PMTOOLOUT

  # Cleaning states
  cat pmtool_states.out | sed -e 's/[[:space:]][[:space:]]*/,/g' > pmtool_states.csv

  rm -f pmtool.out pmtool_states.out
else
  echo "Lionel's pmtool or platform_file.rec file are not available, skipping it."
fi

# Converting the data.rec and tasks.rec files
if [ -x "$(command -v rec2csv)" ]; then
  echo "Convert Rec files"
  DATACSV="rec.data_handles.csv"
  rec2csv -S Handle data.rec | sed 's/"//g' > $DATACSV

  TASKSCSV="rec.tasks.csv"
  rec2csv tasks.rec | sed 's/"//g' > $TASKSCSV
  PAPIFILE="papi.rec"
  if [ -f "$PAPIFILE" ]; then
    PAPICSV="rec.papi.csv"
    rec2csv $PAPIFILE | sed 's/"//g' > $PAPICSV
  fi
else
  # TODO: Read this files without the rec2csv tool?
  echo "WARN: The library recutils is required to read the data.rec and tasks.rec files, skipping this step."
fi

rm -f activity.data distrib.data trace.html tasks.rec papi.rec data.rec trace.rec

echo "Convert from paje.sorted.trace to paje.csv"
date

if [ ! -x "$(command -v pj_dump)" ]; then
  echo "ERROR: pj_dump is not installed, please install PajeNG or configure PATH"
  exit 1
fi
pj_dump -o -n --user-defined --entity-hierarchy=entities.csv --type-hierarchy=types.csv "paje.sorted.trace" > paje.csv
es=$?
if [ $es -ne 0 ]
then
    echo "Error when executing pj_dump (exit status: $es)"
    exit 1
fi
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

PAJEEVENT=paje.events.csv
echo "Nature, Container, Type, Start, Value, Handle, Info, Size, Tid, Src" > $PAJEEVENT
cat paje.csv | grep ^Event >> $PAJEEVENT

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
${DIR}/../R/phase1-workflow.R . ${APPLICATION}
es=$?
if [ $es -ne 0 ]
then
    echo "Error when executing phase1-workflow.R (exit status: $es)"
    exit 2
fi
rm -f atree.csv dag.csv entities.csv paje.link.csv paje.state.csv paje.variable.csv types.csv

echo
echo "End of $CASE"
date
echo

popd
