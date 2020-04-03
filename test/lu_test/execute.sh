#!/bin/bash

cd ../../src/
./phase1-workflow.sh ../test/lu_test/fxt/
cd ../R/
./phase2-workflow.R ../test/lu_test/fxt/ ../test/lu_test/config.yaml ../test/lu_test/img.png
cd ../test/lu_test/
