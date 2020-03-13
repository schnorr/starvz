#!/bin/bash

cd ../src/
./phase1-workflow.sh ../test/fxt/
cd ../R/
./phase2-workflow.R ../test/fxt/ ../test/config.yaml ../test/img.png
cd ../test/
