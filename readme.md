
# **FASST** for **SHERPA** #

Health impacts from high resolution **FASST** grid maps GBD2017 CMIP6.


## Required packages ##

The required packages are:

 * rjson
 * raster

To install a package use the following command in R interpreter:
```R
install.packages( "<package-name>" )
```

## Command line usage ##

Run script without arguments to show arguments list.

Arguments are:

 * mandatory:
    * project name
    * model name
    * version
    * path to working directory, it must be a either a full path or a relative path from current directory

 * optional:
    * configuration JSON file, either an absolute path or a relative one from the working directory


## Output files ##

Under the working directory defined as argument, the script writes the following files and directories:

 * `<project>/` directory, the project name defined as script argument;
    * `tables/`
       * `ALLCNTRIES_<project>_<model>_<version>.TXT` where: _project_, _model_ and _version_ are defined as script arguments:
          * project name,
          * model name, and
          * model version
