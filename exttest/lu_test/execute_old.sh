#!/bin/bash

cd ../../src/
./phase1-workflow.sh ../test/lu_test/fxt/old_data/ lu 1
cd ../R/
./phase2-workflow.R ../test/lu_test/fxt/old_data/ ../test/lu_test/config.yaml ../test/lu_test/img_old.png
cd ../test/lu_test/
