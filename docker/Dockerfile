FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update 
RUN apt-get install -y --no-install-recommends build-essential gnupg2 software-properties-common

# update indices
RUN apt-get update -qq
# install two helper packages we need
RUN apt-get install --no-install-recommends software-properties-common dirmngr
# import the signing key (by Michael Rutter) for these repo
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

RUN apt-get update 

RUN apt-get install -y --no-install-recommends r-base python3 python3-pip python3-setuptools python3-dev liblapack-dev gfortran


WORKDIR /app

COPY requirements.txt /app
COPY requirements.r /app

# installing python libraries
RUN pip3 install -r requirements.txt

# installing r libraries
RUN Rscript requirements.r