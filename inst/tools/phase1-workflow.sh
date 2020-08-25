#!/bin/bash
set -o pipefail
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

if [ ! -x "$(command -v starpu_fxt_tool)" ]; then
    echo "ERROR: Required application starpu_fxt_tool not found"
    exit 1
fi

if [ ! -x "$(command -v pj_dump)" ]; then
    echo "ERROR: Required application pj_dump not found"
    exit 1
fi

if [ ! -x "$(command -v rec2csv)" ]; then
    echo "ERROR: Required application rec2csv not found"
    exit 1
fi

##############################
# Parameters                 #
##############################
CASE=${1:-}
if [ -z "$CASE" ]; then echo "Error: <fxtdir> is empty"; usage; exit; fi
APPLICATION=${2:-}
#if [ -z "$APPLICATION" ]; then echo "Error: <application> is empty"; usage; exit; fi

echo "StarVZ Phase 1 - Start of $CASE"
date "+%a %d %b %Y %H:%M:%S %Z"

pushd $CASE

# If sorted is already present dont reexecute
if [ ! -f "paje.sorted.trace" ] || [ ! -f "data.rec" ] || [ ! -f "tasks.rec" ]; then
  echo "Convert from FXT to paje.sorted.trace"
  fxt2paje.sh
  es=$?
  if [ $es -ne 0 ]
  then
      echo "ERROR: conversion from FXT failed! (exit status: $es)"
      exit 1
  fi
  if [ -z "$STARVZ_KEEP" ]; then
    rm -f paje.trace.gz
  fi
else
  echo "fxt2paje files already present"
fi
#aa
# Put Lionel's pmtool to get bounds to enrich the visualization
# Also create a hypothetical trace from the solution of the LP

PMTOOLCSV=""

echo "Execute pmtool"
date "+%a %d %b %Y %H:%M:%S %Z"
# Generating platform_file.rec if both pmtool and performance models are available. Note: hostname should match performance model
if [ -x "$(command -v pmtool)" ] && [ ! -f "platform_file.rec" ]; then
  if [ $STARPU_PERF_MODEL_DIR != "" ] && [ $STARPU_HOSTNAME != "" ]; then
    starpu_perfmodel_recdump tasks.rec -o platform_file.rec
  else
    echo "Lionel's platform_file.rec file is not available and cannot be generated, skipping it."
  fi
fi

# Checking if Lionel's pmtool command and platform_file.rec file are available & accessible
if [ -x "$(command -v pmtool)" ] && [ -f "platform_file.rec" ]; then
  PMTOOLOUT="pmtool.csv"

  # This command should be executed with the cluster starpu.
  #starpu_perfmodel_recdump tasks.rec -o platform_file.rec

  # Running pmtool with default-normal configuration
  pmtool -p platform_file.rec tasks.rec -d fast -a dmdas --threads --no-header -w -s pmtool_states.out > pmtool.out 2> /dev/null

  # Cleaning pmtools bounds.
  echo "Alg,Bound,Time" > $PMTOOLOUT
  cat pmtool.out | awk '{ print $(4), $(3), $(5)}' | sed '/^[[:space:]]*$/d' | sed -e 's/[[:space:]]/,/g' | sed 's/True/TRUE/g' | sed 's/False/FALSE/g' >> $PMTOOLOUT

  # Cleaning states
  cat pmtool_states.out | sed -e 's/[[:space:]][[:space:]]*/,/g' > pmtool_states.csv
  if [ -z "$STARVZ_KEEP" ]; then
    rm -f pmtool.out pmtool_states.out
  fi
else
  echo "Lionel's pmtool or platform_file.rec file are not available, skipping it."
fi

# Converting the data.rec and tasks.rec files
if [ -x "$(command -v rec2csv)" ]; then
  echo "Convert Rec files"
  date "+%a %d %b %Y %H:%M:%S %Z"
  DATACSV="rec.data_handles.csv.gz"
  rec2csv -S Handle data.rec | sed 's/"//g' | gzip -c > $DATACSV

  TASKSCSV="rec.tasks.csv.gz"
  rec2csv tasks.rec | sed 's/"//g' | gzip -c > $TASKSCSV
  PAPIFILE="papi.rec"
  if [ -f "$PAPIFILE" ] && [ -s "$PAPIFILE" ]; then
    PAPICSV="rec.papi.csv.gz"
    rec2csv $PAPIFILE | sed 's/"//g' | gzip -c > $PAPICSV
  fi
else
  # TODO: Read this files without the rec2csv tool?
  echo "WARN: The library recutils is required to read the data.rec and tasks.rec files, skipping this step."
fi

if [ -z "$STARVZ_KEEP" ]; then
  rm -f activity.data distrib.data trace.html tasks.rec papi.rec data.rec trace.rec
fi
echo "Convert from paje.sorted.trace to paje.csv"
date "+%a %d %b %Y %H:%M:%S %Z"

if [ ! -x "$(command -v pj_dump)" ]; then
  echo "ERROR: pj_dump is not installed, please install PajeNG or configure PATH"
  exit 1
fi
zcat "paje.sorted.trace.gz" | pj_dump -o -n --user-defined --entity-hierarchy=entities.csv --type-hierarchy=types.csv | gzip -c > paje.csv.gz
es=$?
if [ $es -ne 0 ]
then
    echo "Error when executing pj_dump (exit status: $es)"
    exit 1
fi

if [ -z "$STARVZ_KEEP" ]; then
  rm -f paje.sorted.trace.gz
fi
echo "Get states, links and variables in CSV"
date "+%a %d %b %Y %H:%M:%S %Z"

PAJE_MEMORY_STATE=paje.memory_state.csv.gz
echo "Nature, ResourceId, Type, Start, End, Duration, Depth, Value" | gzip -c > $PAJE_MEMORY_STATE
zgrep -e "Memory Node State" paje.csv.gz | gzip -c >> $PAJE_MEMORY_STATE

PAJE_COMM_STATE=paje.comm_state.csv.gz
echo "Nature, ResourceId, Type, Start, End, Duration, Depth, Value" | gzip -c > $PAJE_COMM_STATE
zgrep -e "Communication Thread State" paje.csv.gz | gzip -c >> $PAJE_COMM_STATE

PAJE_WORKER_STATE=paje.worker_state.csv.gz
echo "Nature, ResourceId, Type, Start, End, Duration, Depth, Value, Size, Params, Footprint, Tag, JobId, SubmitOrder, GFlop, X, Y, Iteration, Subiteration" | gzip -c > $PAJE_WORKER_STATE
zgrep -e "Worker State" paje.csv.gz | gzip -c >> $PAJE_WORKER_STATE

PAJE_OTHER_STATE=paje.other_state.csv.gz
echo "Nature, ResourceId, Type, Start, End, Duration, Depth, Value" | gzip -c > $PAJE_OTHER_STATE
zgrep -E "^State" paje.csv.gz | grep -E -v "(Memory Node State|Communication Thread State|Worker State)" | gzip -c >> $PAJE_OTHER_STATE

PAJEVARIABLE=paje.variable.csv.gz
echo "Nature, ResourceId, Type, Start, End, Duration, Value" | gzip -c > $PAJEVARIABLE
zgrep -E "^Variable" paje.csv.gz | gzip -c >> $PAJEVARIABLE

PAJELINK=paje.link.csv.gz
echo "Nature, Container, Type, Start, End, Duration, Size, Origin, Dest, Key, Tag" | gzip -c > $PAJELINK
zgrep -E "^Link" paje.csv.gz | gzip -c >> $PAJELINK

PAJEEVENT=paje.events.csv.gz
echo "Nature, Container, Type, Start, Value, Handle, Info, Size, Tid, Src" | gzip -c > $PAJEEVENT
zgrep -E "^Event" paje.csv.gz | gzip -c >> $PAJEEVENT

if [ -z "$STARVZ_KEEP" ]; then
  rm -f paje.csv.gz
fi

echo "Convert (DAG) DOT to CSV"
date "+%a %d %b %Y %H:%M:%S %Z"

OUTPUTDAGCSV=dag.csv.gz
echo "Dependent,JobId" | gzip -c > $OUTPUTDAGCSV
cat dag.dot | \
    grep "\->" | \
    sed -e "s/[[:space:]]//g" -e "s/\"//g" -e "s/task_//g" -e "s/->/,/" | \
    sed -e "s/[[:space:]]//g" | \
    sort | gzip -c >> $OUTPUTDAGCSV

echo "Convert (ATREE) DOT to CSV"
date "+%a %d %b %Y %H:%M:%S %Z"
ATREE=atree.dot
if [ -e "${ATREE}" ]; then
    OUTPUTATREECSV=atree.csv
    echo "Node,DependsOn" > $OUTPUTATREECSV
    cat $ATREE | grep - "--" | sed -e "s/ -- /,/" -e "s/node//g" -e "s/^0*//" -e "s/,0*/,/" >> $OUTPUTATREECSV
fi

echo "Post-processing CSV files"
date "+%a %d %b %Y %H:%M:%S %Z"
phase1-workflow.R . ${APPLICATION} ${3:-}
es=$?
if [ $es -ne 0 ]
then
    echo "Error when executing phase1-workflow.R (exit status: $es)"
    exit 2
fi

if [ -z "$STARVZ_KEEP" ]; then
  rm -f atree.csv $OUTPUTDAGCSV entities.csv $PAJELINK $PAJEVARIABLE\
        types.csv comms.rec $PAJEEVENT $PAJE_COMM_STATE $PAJE_MEMORY_STATE $PAJE_OTHER_STATE\
        $PAJE_WORKER_STATE $DATACSV $TASKSCSV sched_tasks.rec dag.dot
fi

echo
echo "End of $CASE"
date "+%a %d %b %Y %H:%M:%S %Z"
echo

popd
