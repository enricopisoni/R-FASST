#!/bin/sh

# Script file to simply test the model.
# We call the R script with dummies: project name (Project), model name (Model)
# and model version (1.0); the working directory will be '../Files/testing' and
# in the latter directory the script will use the configuration
# file './testing-config.json'.
# 
./fasst-4-sherpa.sh  Project  FASST  1.0  ../Files/testing  ./fasst-config.json
