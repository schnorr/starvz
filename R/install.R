#!/usr/bin/Rscript
install.packages(c('tidyverse', 'devtools'), repos = 'https://cloud.r-project.org')
library(devtools);
devtools::install_local(path='./starvz/R_package')
