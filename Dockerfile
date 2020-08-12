FROM r-base:3.5.1

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y libxml2-dev libssl-dev libcurl4-openssl-dev libgit2-dev
RUN apt-get install -y libboost-dev

RUN echo "install.packages(c('tidyverse', 'devtools'), repos = 'http://cran.us.r-project.org')" | R --vanilla

RUN echo "install.packages('devtools', repos = 'http://cran.us.r-project.org')" | R --vanilla

RUN echo "library(devtools); devtools::install_github('schnorr/starvz')" | R --vanilla

RUN useradd -s /bin/bash --create-home user
USER user

ENTRYPOINT /bin/bash
WORKDIR /home/user
