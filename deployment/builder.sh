#!/bin/bash

# Pass in as an argument the directory where the executables should end up.

docker run --rm -v $1:/opt/build/dist builder:latest
