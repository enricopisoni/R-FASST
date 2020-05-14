
# **FASST** for **SHERPA** #

Health impacts from high resolution **FASST** grid maps GBD2017 CMIP6.


## Required packages ##

The required packages are:

 * rjson
 * raster
 * tidyverse
 * ncdf4

To install a package use the following command in R interpreter:
```R
install.packages( "<package-name>" )
```


### Linux environment ###

In Linux operating system (like Fedora) you should install the following packages:

 * libcurl-devel
 * openssl-devel
 * libxml2-devel
 * netcdf-devel


## Command line usage ##

To run easily the script in command line, in the same directory this file is stored, run one of the following files:

 * `fasst-4-sherpa.bat`
 * `fasst-4-sherpa.sh`

The first one if you are on Windows system or the second one if you are using a Linux operating system.


Run script without arguments to show arguments list.

Arguments are:

 * mandatory:
    * project name
    * model name
    * version
    * path to working directory, it must be a either a full path or a relative path from current directory

 * optional:
    * configuration JSON file, either an absolute path or a relative one from the working directory


## Run in IDE ##

You can run the script in R IDE/Gui as well.

Let suppose the directory where this file is stored is: `<src-dir>`.
In the IDE console type the following commands (by replacing "<src-dir>" with the full path to the directory where this file is stored):

 * `setwd( '<src-dir>' )`
 * `source( 'fasst-config.R' )`
 * `source( 'health-impacts.R' )`
 * `health.impact( 'Project', 'Model',  '1.0',  '../Files/testing',  health.impact.config( './testing-config.json' ) )`

In the last command, we run the script with testing files and arbitrary project and model name and version.


## Output files ##

Under the working directory defined as argument, the script writes the following files and directories:

 * `<project>/` directory, the project name defined as script argument;
    * `tables/`
       * `ALLCNTRIES_<project>_<model>_<version>.TXT` where: _project_, _model_ and _version_ are defined as script arguments:
          * project name,
          * model name, and
          * model version
