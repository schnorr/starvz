#!/bin/bash

TEST="../../extra_test/lu_test/"

cd ../../inst/tools/
#./phase1-workflow.sh $TEST/fxt/ lu 1
./phase2-workflow.R $TEST/fxt/ $TEST/config.yaml $TEST/img.png
#./phase2-workflow.R $TEST/fxt/ $TEST/min.yaml $TEST/min.png
#./phase2-workflow.R $TEST/fxt/ $TEST/selected.yaml $TEST/selected.png
#./phase2-workflow.R $TEST/fxt/ $TEST/agg.yaml $TEST/agg.png
#./phase2-workflow.R $TEST/fxt/ $TEST/agg_dynamic.yaml $TEST/agg_dynamic.png
#./phase2-workflow.R $TEST/fxt/ $TEST/agg_static.yaml $TEST/agg_static.png
#./phase2-workflow.R $TEST/fxt/ $TEST/imb.yaml $TEST/imb.png
#./phase2-workflow.R $TEST/fxt/ $TEST/pmtool.yaml $TEST/pmtool.png
cd $TEST
