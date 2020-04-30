# -*- mode: R -*-

#' FASST configuration, with both model and files configuration;
#'
#' @param file path to JSON file with path to ancillary files,
#'             or NA to use internal static configuration;
#'             see function \code{\link{health.impact.config.json()}} for
#'             for further details about this parameter;
#'
#' @return
#'      a list with following entries:
#'      \describe{
#'              \item{model}
#'                      {the static model cofiguration;}
#'              \item{files}
#'                      {paths to files;}
#'              \item{tmpls}
#'                      {for CSV file: column names and types;}
#'      }
#'
health.impact.config <- function( file = NULL )
{
        if ( is.null( file ) || is.na( file ) )
        {
                files <- health.impact.config.static()
        } else {
                files <- health.impact.config.json( file )
        }
        list(
                model = health.impact.config.model(),
                files = files,
                tmpls = health.impact.config.templates()
        )
}

# --------------------------------------------------------------------------------

#' FASST configuration with the definition of files
#' needed by function: \code{\link{health:impact()}};
#'
#' Notes:
#' \itemize{
#'      \item all files paths are relative to working directory;
#'      \item for definition of entry \code{in.tmpl.pop.map} refer
#'            to fucntion \code{\link{get.population.map()}};
#' }
#'
#' @return
#'   named list with input/output files;
#'

health.impact.config.static <- function()
{
        in.dir.root     <- '.'
        in.dir.ancil    <- file.path( in.dir.root,  'ANCILLARY' )
        in.dir.gbd      <- file.path( in.dir.ancil, 'MORTALITY', 'BASEMORT2018' )
        in.dir.rr       <- file.path( in.dir.ancil, 'MORTALITY', 'RRs2018', 'FIT' )
        in.dir.tmpls    <- file.path( in.dir.root,  'CODE', 'TEMPLATES' )
        in.dir.ssp      <- file.path( in.dir.ancil, 'POPULATION_SSP', 'NETCDF' )
        in.dir.ciesin   <- file.path( in.dir.ancil, 'CIESIN_COUNTRY_MASK', 'CIESIN_V4', '15minx15min' )
        in.dir.ncdf     <- file.path( in.dir.root,  'NCDF_IN' )

        list(
                in.file.pop.country = file.path( in.dir.gbd, 'POP_1990-2100_UN2017_AGEGRP.csv' ),

                # files with base mortality rates (per 100k population)
                in.file.copd        = file.path( in.dir.gbd,    'COPD_MORT_RATE_GBD2016.csv' ),
                in.file.lc          = file.path( in.dir.gbd,    'LC_MORT_RATE_GBD2016.csv' ),
                in.file.dmt2        = file.path( in.dir.gbd,    'DMT2_MORT_RATE_GBD2016.csv' ),
                in.file.lri         = file.path( in.dir.gbd,    'LRI_MORT_RATE_GBD2016.csv' ),
                in.file.ihd         = file.path( in.dir.gbd,    'IHD_MORT_RATE_GBD2016.csv' ),
                in.file.stroke      = file.path( in.dir.gbd,    'STROKE_MORT_RATE_GBD2016.csv' ),
                in.file.cntrgrid    = file.path( in.dir.ciesin, 'gpw_v4_national_identifier_grid_rev10_15_min.asc' ),

                # risk function parameters
                in.file.rr          = file.path( in.dir.rr,     'RR_ALL_GBD_2017_FITTINGS_ANALYT.csv' ),

                # population map - NetCDF files template
                in.tmpl.pop.map     = file.path( in.dir.ssp, '${scenario}_NETCDF', 'total', 'netcdf', '${scenario}_${year}.nc' ),

                # scenario input file - NetCDF files template
                in.tmpl.scenario    = file.path( in.dir.ncdf, '${scenario}', 'FASST_75x75_${scenario}_${year}.nc' ),

                # project dependent scenarios
                scenarios           = data.frame(
                                                "name" = c( "SSP1_26" ),
                                                "year" = c( 2015 ),
                                                "ssp"  = c( "sep1" )
                                      )
        )
}

# --------------------------------------------------------------------------------

#' Read the FASST configuration from JSON file with
#' the definition of files needed by function:
#' \code{\link{health:impact()}};
#'
#' Constraint:
#' \itemize{
#'      \item configuration JSON file must have the same entries
#'            defined in list returned by function
#'            \code{\link{health.impact.config()}};
#' }
#'
#' @param file path to JSON configuration file;
#'
#' @return
#'   named list with input/output files;
#'

library( "rjson" )

health.impact.config.json <- function( file )
{
    # --- read the JSON configuration file ---
    config <- fromJSON( file = file )

    # --- update the configuration read accordingly the expected configuration ---
    config$scenarios <- as.data.frame( config$scenarios )

    # --- return the configuration ---
    config
}

# --------------------------------------------------------------------------------

#' Configuration of FASST model needed
#' by function: \code{\link{health:impact()}};
#'
#' Constraint:
#' \itemize{
#'      \item model configuration defined by this function
#'            MUST not be modified;
#' }
#'
#' @return
#'      named list with model configuration;
#'
#' @examples
health.impact.config.model <- function()
{
        # --- model configuration ---
        config.definition <- list(

              # available years with ssp populatation. if needed for scenarios, interpolate between those years
              ssp_yrs = c( 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100 ),

              # counterfactual level for SDMA8h for O3 health impact - GBD2017 uses between 29.1 and 35.7 for M3M. Malley/Turner use between 26.7 and 31.1
              SDM8THR = 29.1,
              # counterfactual level for ADMA8h for O3 health impact - Turner uses both 26.7 and 31.1
              ADM8THR = 26.7,

              # AGE CLASSES TO CONSIDER:
              ages = seq( from = 0, to = 100, by = 5 ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              agefrac_copd = c( 0, 100 ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              agefrac_lc   = c( 0, 100 ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              agefrac_lri  = c( 0, 100 ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              agefrac_dmt2 = c( 0, 100 ),

              # AGE SPECIFIC FROM MIN TO MAX (MAX 95, BECAUSE AGE-SPECIFIC RR AVAILABLE TILL 95)
              agefrac_ihd  = c( 25, 95 ),

              # OZONE MORTALITIES - NOTE that mortality rates in GBD are expressed as number divided by total population.
              # COPD mortalitiy rates below 25 are zero.
              # ALL AGES FROM MIN TO MAX (MAX 100)
              agefrac_o3   = c( 0, 100 ),

              # CODs for which RR is aggregated over all age groups
              COD1         = c( 'COPD', 'LC', 'LRI', 'DMT2' ),
              # CODs for which RR is age group sepcific
              COD2         = c( 'IHD', 'STROKE' ),

              # DO NOT CHANGE!
              # Generates string array of AGE GROUP NAMES (25 TO 95 IN 5 YEAR BINS) 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
              AGE_GRP      = seq( from = 25, to = 95, by = 5 )
        )

        # --- model configurationd erived from the above one ---
        config.derived <- list(

              # ALL AGES FROM MIN TO MAX (MAX 100)
              C1  = ( config.definition $ ages  ==  config.definition $ agefrac_copd[ 1 ] ),
              C2  = ( config.definition $ ages  == config.definition $ agefrac_copd[ 2 ] ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              L1  = ( config.definition $ ages  ==  config.definition $ agefrac_lc[ 1 ] ),
              L2  = ( config.definition $ ages  ==  config.definition $ agefrac_lc[ 2 ] ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              LR1 = ( config.definition $ ages  ==  config.definition $ agefrac_lri[ 1 ] ),
              LR2 = ( config.definition $ ages  ==  config.definition $ agefrac_lri[ 2 ] ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              DM1 = ( config.definition $ ages  ==  config.definition $ agefrac_dmt2[ 1 ] ),
              DM2 = ( config.definition $ ages  ==  config.definition $ agefrac_dmt2[ 2 ] ),

              # AGE SPECIFIC FROM MIN TO MAX (MAX 95, BECAUSE AGE-SPECIFIC RR AVAILABLE TILL 95)
              IH1 = ( config.definition $ ages  ==  config.definition $ agefrac_ihd[ 1 ] ),
              IH2 = ( config.definition $ ages  ==  config.definition $ agefrac_ihd[ 2 ] ),

              # OZONE MORTALITIES - NOTE that mortality rates in GBD are expressed as number divided by total population.
              O1  = ( config.definition $ ages  ==  config.definition $ agefrac_o3[ 1 ] ),
              O2  = ( config.definition $ ages  ==  config.definition $ agefrac_o3[ 2 ] )
        )

        # --- the two configurations merged ---
        c( config.definition, config.derived )
}

# --------------------------------------------------------------------------------

#' Templates for CSV files.
#' The function defines templates with column names to use for csv file,
#' the latter column names are used by function: \code{\link{health:impact()}};
#'
#' @return
#'      named list with templates for column names;
#'
#' @examples
health.impact.config.templates <- function()
{
        list(
                pop.name  = c( 'CNTR_ID', 'CNTR_NAME', 'CNTR_ISO3', 'CNTR_FASST', 'VARIANT', 'YEAR', 'AGE_GRP', 'AGE_POP' ),
                pop.type  = cols(
                                CNTR_ID    = col_integer(),
                                CNTR_NAME  = col_character(),
                                CNTR_ISO3  = col_character(),
                                CNTR_FASST = col_character(),
                                VARIANT    = col_character(),
                                YEAR       = col_integer(),
                                AGE_GRP    = col_integer(),
                                AGE_POP    = col_double()
                            ),

                rr.name   = c( 'COD', 'PARAM', 'AGE', 'RRMED', 'RRLO', 'RRHI' ),
                rr.type   = cols(
                                COD   = col_character(),
                                PARAM = col_character(),
                                AGE   = col_integer(),
                                RRMED = col_double(),
                                RRLO  = col_double(),
                                RRHI  = col_double()
                            ),

                mort.name = c( 'FASSTREG', 'POP', 'MEASURE', 'CNTR_ID', 'CNTR_NAME', 'SEX_ID', 'SEX_NAME',
                               'AGE_ID', 'AGE_NAME', 'CAUSE_ID', 'CAUSE_NAME', 'METRIC_ID', 'METRIC_NAME',
                               'YEAR', 'VAL', 'HI', 'LO', 'RATIO' ),
                mort.type = cols(
                                FASSTREG    = col_character(),
                                POP         = col_double(),
                                MEASURE     = col_character(),
                                CNTR_ID     = col_integer(),
                                CNTR_NAME   = col_character(),
                                SEX_ID      = col_integer(),
                                SEX_NAME    = col_character(),
                                AGE_ID      = col_integer(),
                                AGE_NAME    = col_character(),
                                CAUSE_ID    = col_integer(),
                                CAUSE_NAME  = col_character(),
                                METRIC_ID   = col_integer(),
                                METRIC_NAME = col_character(),
                                YEAR        = col_integer(),
                                VAL         = col_double(),
                                HI          = col_double(),
                                LO          = col_double(),
                                RATIO       = col_double()
                            )
        )
}
