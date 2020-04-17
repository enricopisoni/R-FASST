@echo off

rem  Script file to simply test the model.
rem  We call the R script with dummies: project name (Project), model name (Model)
rem  and model version (1.0); the working directory will be '../Files/testing' and
rem  in the latter directory the script will use the configuration
rem  file './testing-config.json'.
rem  
call  fasst-4-sherpa.bat  Project  Model  1.0  ../Files/testing  ./testing-config.json
