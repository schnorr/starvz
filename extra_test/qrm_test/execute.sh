#!/bin/bash
cd fxt
gzip -d prof_file_qrm.gz

cd ../../../src/
./phase1-workflow.sh ../test/qrm_test/fxt/ qrmumps
cd ../R/
./phase2-workflow.R ../test/qrm_test/fxt/ ../test/qrm_test/config.yaml ../test/qrm_test/img.png
cd ../test/
