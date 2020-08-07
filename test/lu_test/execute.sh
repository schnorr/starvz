#!/bin/bash

cd ../../src/
#./phase1-workflow.sh ../test/lu_test/fxt/ lu 1
cd ../R/
./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/config.yaml ../test/lu_test/img.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/min.yaml ../test/lu_test/min.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/selected.yaml ../test/lu_test/selected.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/agg.yaml ../test/lu_test/agg.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/agg_dynamic.yaml ../test/lu_test/agg_dynamic.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/agg_static.yaml ../test/lu_test/agg_static.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/imb.yaml ../test/lu_test/imb.png
#./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/pmtool.yaml ../test/lu_test/pmtool.png
cd ../test/lu_test/
