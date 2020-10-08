# -*- mode: R -*-

#' FASST configuration, with both model and files configuration;
#'
#' @param file path to JSON file with path to ancillary files,
#'             or NA to use internal static configuration;
#'             see function \code{\link{health.impact.config.json()}}
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
        in.dir.root     <- file.path( '..', 'INPUT' )
        in.dir.ancil    <- file.path( in.dir.root,  'ANCILLARY' )
        in.dir.mort     <- file.path( in.dir.ancil, 'MORTALITY' )
        in.dir.gbd      <- file.path( in.dir.mort,  'BASEMORT2018' )
        in.dir.bsmrt    <- file.path( in.dir.mort,  'GBD_BASEMORT_GRIDMAPS' )
        in.dir.rr       <- file.path( in.dir.mort,  'RRs2018', 'FIT' )
        in.dir.tmpls    <- file.path( in.dir.root,  'CODE', 'TEMPLATES' )
        in.dir.ssp      <- file.path( in.dir.ancil, 'POPULATION_SSP', 'NETCDF' )
        in.dir.ciesin   <- file.path( in.dir.ancil, 'CIESIN_COUNTRY_MASK', 'CIESIN_V4', '15minx15min' )
        in.dir.ncdf     <- file.path( in.dir.root,  'NCDF_IN' )

        out.dir.root    <- file.path( '.', '${project}' )
        out.dir.tables  <- file.path( out.dir.root, 'tables' )
        out.dir.ncdf    <- file.path( out.dir.root, 'ncdf', '${scenario}' )

        list(
                # reduction factor for medium resolution grid
                reduction.factor      = 4L,

                # population per country
                in.file.pop.country   = file.path( in.dir.gbd, 'POP_1990-2100_UN2017_AGEGRP.csv' ),

                # countries grids
                in.file.cntrgrid.hi   = file.path( in.dir.ciesin, 'gpw_v4_national_identifier_grid_rev10_15_min.asc' ),
                in.file.cntrgrid.lo   = file.path( in.dir.ancil,  'FASST_REGION_MASK', '0.5x0.5_INDIV_COUNTRY_MASK.asc' ),

                # files with base mortality rates (per 100k population)
                in.file.copd          = file.path( in.dir.gbd,    'COPD_MORT_RATE_GBD2016.csv' ),
                in.file.lc            = file.path( in.dir.gbd,    'LC_MORT_RATE_GBD2016.csv' ),
                in.file.dmt2          = file.path( in.dir.gbd,    'DMT2_MORT_RATE_GBD2016.csv' ),
                in.file.lri           = file.path( in.dir.gbd,    'LRI_MORT_RATE_GBD2016.csv' ),
                in.file.ihd           = file.path( in.dir.gbd,    'IHD_MORT_RATE_GBD2016.csv' ),
                in.file.stroke        = file.path( in.dir.gbd,    'STROKE_MORT_RATE_GBD2016.csv' ),

                # risk function parameters
                in.file.rr            = file.path( in.dir.rr,     'RR_ALL_GBD_2017_FITTINGS_ANALYT.csv' ),

                # population map - NetCDF files template
                in.tmpl.pop.map       = file.path( in.dir.ssp, '${scenario}_NETCDF', 'total', 'netcdf', '${scenario}_${year}.nc' ),

                # scenario input file - NetCDF files template
                in.tmpl.scenario      = list(
                                                "filename"                    = file.path( in.dir.ncdf, '${scenario}', 'FASST_75x75_${scenario}_${year}.nc' ),
                                                "total_pm"                    = "TOT_PM_35",
                                                "anthropogenic_pm"            = "ANT_PM_35",
                                                "annual_mean_of_daily_mean"   = "ADM8h",
                                                "seasonal_mean_of_daily_mean" = "SDM8h",
                                                "natural.dust.ss"             = "NAT_PM_dry",
                                                "residual.water.ss"           = "H2O35_SS"
                                        ),

                # mortality base incidences - csv file templates
                in.tmpl.mrate.copd    = file.path( in.dir.bsmrt,  'MRATE_COPD_GBD_${year}' ),
                in.tmpl.mrate.lc      = file.path( in.dir.bsmrt,  'MRATE_LC_GBD_${year}' ),
                in.tmpl.mrate.dmt2    = file.path( in.dir.bsmrt,  'MRATE_DMT2_GBD_${year}' ),
                in.tmpl.mrate.lri     = file.path( in.dir.bsmrt,  'MRATE_LRI_GBD_${year}' ),
                in.tmpl.mrate.ihd     = file.path( in.dir.bsmrt,  'MRATE_IHD_GBD_${year}' ),
                in.tmpl.mrate.stroke  = file.path( in.dir.bsmrt,  'MRATE_STROKE_GBD_${year}' ),

                # population fraction per age class for all classes
                in.tmpl.pop_age_fr    = file.path( in.dir.mort,   'POP_AGE_CLASS_FRACTIONS_UN2017_${year}' ),

                # output files: all countries
                out.tmpl.countries    = file.path( out.dir.tables, 'ALLCNTRIES_${project}_${model}_${version}' ),
                # output files: mortalities grid
                out.tmpl.mortalities  = file.path( out.dir.ncdf, 'FASST_05x05_MORTALITIES_${project}_${year}_${scenario}' ),


                # project dependent scenarios
                scenarios             = list(
                                                  "name" = c( "SSP1_26" ),
                                                  "year" = c( 2015 ),      # Scenario years to be analyzed
                                                  "ssp"  = c( "SEP1" )     # SSP array has same dimension as "name", each "name" corresponds to a matching SSP
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
    config$scenarios <- as.list( config$scenarios )

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

# -- not- used --              # DO NOT CHANGE;  AVAILABLE YEARS OF STATS FOR MORTALITIES: 1990 - 2040.
# -- not- used --              MORT_YEARS   = seq( from = 1990, to = 2040, by = 5 )
# -- not- used --              # DO NOT CHANGE;  AVAILABLE YEARS OF POP AGE STRUCTURE DATA: 1990 - 2100 (UN 2017 REVISION, MEDIUM VARIANT FOR PROJECTED).
# -- not- used --              POP_YEARS    = seq( from = 1990, to = 2100, by = 5 )
        )

        # --- model configurationd erived from the above one ---
        config.derived <- list(

              # ALL AGES FROM MIN TO MAX (MAX 100)
              C1  = match( config.definition $ agefrac_copd[ 1 ], config.definition $ ages ),
              C2  = match( config.definition $ agefrac_copd[ 2 ], config.definition $ ages ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              L1  = match( config.definition $ agefrac_lc[ 1 ], config.definition $ ages ),
              L2  = match( config.definition $ agefrac_lc[ 2 ], config.definition $ ages ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              LR1 = match( config.definition $ agefrac_lri[ 1 ], config.definition $ ages ),
              LR2 = match( config.definition $ agefrac_lri[ 2 ], config.definition $ ages ),

              # ALL AGES FROM MIN TO MAX (MAX 100)
              DM1 = match( config.definition $ agefrac_dmt2[ 1 ], config.definition $ ages ),
              DM2 = match( config.definition $ agefrac_dmt2[ 2 ], config.definition $ ages ),

              # AGE SPECIFIC FROM MIN TO MAX (MAX 95, BECAUSE AGE-SPECIFIC RR AVAILABLE TILL 95)
              IH1 = match( config.definition $ agefrac_ihd[ 1 ], config.definition $ ages ),
              IH2 = match( config.definition $ agefrac_ihd[ 2 ], config.definition $ ages ),

              # OZONE MORTALITIES - NOTE that mortality rates in GBD are expressed as number divided by total population.
              O1  = match( config.definition $ agefrac_o3[ 1 ], config.definition $ ages ),
              O2  = match( config.definition $ agefrac_o3[ 2 ], config.definition $ ages )
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
