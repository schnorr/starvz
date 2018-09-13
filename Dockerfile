FROM r-base:3.5.1

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y libssl-dev libcurl4-openssl-dev libgit2-dev

RUN echo "install.packages('devtools')" | R
