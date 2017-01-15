#!/bin/bash

# Pass in as an argument the directory where the executables should end up.
cd $PROJ_DIR
docker build -f ${PROJ_DIR}/deployment/Dockerfile.build -t builder:latest .
docker run --rm -v $1:/opt/build/dist builder:latest
docker image prune
docker container prune
