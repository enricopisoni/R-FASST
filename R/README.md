
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

Under the working directory, defined as script argument, the script reads and writes the files listed here below.
In detail, the list shows:

 - the file key as defined in the configuration file;
 - the file name used for test;

Moreover, text between braces is replaced at runtime with the values as stated by following table:

 - `project`
        the project name as defined as script argument;
 - `model`
        the model name as defined as script argument;
 - `version`
        the version as defined as script argument;
 - `scenario`
        the current scenario name as defined in the configuration file under the path
        `$.scenarios.name`
 - `year`
        the current year as defined in the configuration file under the path
        `$.scenarios.year`
 - `resolution`
        the resolution of output grids;

### Input files by key ###
 - `in.file.pop.country`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/POP_1990-2100_UN2017_AGEGRP.csv`
 - `in.file.cntrgrid`
     - `INPUT/ANCILLARY/CIESIN_COUNTRY_MASK/CIESIN_V4/15minx15min/gpw_v4_national_identifier_grid_rev10_15_min.asc`
 - `in.file.copd`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/COPD_MORT_RATE_GBD2016.csv`
 - `in.file.lc`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/LC_MORT_RATE_GBD2016.csv`
 - `in.file.lri`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/DMT2_MORT_RATE_GBD2016.csv`
 - `in.file.ihd`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/LRI_MORT_RATE_GBD2016.csv`
 - `in.file.stroke`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/IHD_MORT_RATE_GBD2016.csv`
 - `in.file.dmt2`
     - `INPUT/ANCILLARY/MORTALITY/BASEMORT2018/STROKE_MORT_RATE_GBD2016.csv`
 - `in.file.rr`
     - `INPUT/ANCILLARY/MORTALITY/RRs2018/FIT/RR_ALL_GBD_2017_FITTINGS_ANALYT.csv`
 - `in.tmpl.pop.map`
     - `INPUT/ANCILLARY/POPULATION_SSP/NETCDF/${scenario}_NETCDF/total/netcdf/${scenario}_${year}.nc`
 - `in.tmpl.scenario` defines file and variables to read
     - `filename`
         - `INPUT/NCDF_IN/${scenario}/FASST_75x75_${scenario}_${year}.nc`
     - `total_pm`
         - `TOT_PM_35`
     - `anthropogenic_pm`
         - `ANT_PM_35`
     - `annual_mean_of_daily_mean`
         - `ADM8h`
     - `seasonal_mean_of_daily_mean`
         - `SDM8h`

### Intermediate files by key ###
These files are read by script if they are present, otherwise they are created using the above files as input.
The following files have both extensions: `csv` and `nc`.

 - `in.tmpl.mrate.copd`
     - `INPUT/ANCILLARY/MORTALITY/GBD_2017_BASEMORT_GRIDMAPS/MRATE_COPD_GBD_${year}`
 - `in.tmpl.mrate.lc`
     - `INPUT/ANCILLARY/MORTALITY/GBD_2017_BASEMORT_GRIDMAPS/MRATE_LC_GBD_${year}`
 - `in.tmpl.mrate.lri`
     - `INPUT/ANCILLARY/MORTALITY/GBD_2017_BASEMORT_GRIDMAPS/MRATE_DMT2_GBD_${year}`
 - `in.tmpl.mrate.dmt2`
     - `INPUT/ANCILLARY/MORTALITY/GBD_2017_BASEMORT_GRIDMAPS/MRATE_LRI_GBD_${year}`
 - `in.tmpl.mrate.ihd`
     - `INPUT/ANCILLARY/MORTALITY/GBD_2017_BASEMORT_GRIDMAPS/MRATE_IHD_GBD_${year}`
 - `in.tmpl.mrate.stroke`
     - `INPUT/ANCILLARY/MORTALITY/GBD_2017_BASEMORT_GRIDMAPS/MRATE_STROKE_GBD_${year}`
 - `in.tmpl.pop_age_fr`
     - `INPUT/ANCILLARY/MORTALITY/POP_AGE_CLASS_FRACTIONS_UN2017_${year}`

### Output files ###
 - `out.tmpl.countries`
     - `${project}/tables/ALLCNTRIES_${project}_${model}_${version}`
 - `out.tmpl.mortalities`
     - `${project}/ncdf/${scenario}/FASST_${resolution}_MORTALITIES_${project}_${year}_${scenario}`
