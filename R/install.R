#!/usr/bin/Rscript
install.packages(c('tidyverse', 'devtools', 'arrow'))
library(arrow)
arrow::install_arrow()
library(devtools);
devtools::install_local(path='./starvz/R_package')
