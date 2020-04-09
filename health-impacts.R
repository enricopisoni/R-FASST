# -*- mode: R -*-

#' Health impacts from high resolution FASST grid maps;
#'
#' Function constraints:
#' \itemize{
#'      \item output directory defined by formal parameter \code{dir.root.out} must exist;
#' }
#'
#' @param project       project name;
#' @param model         model name;
#' @param version       current version;
#' @param dir.root.out  output directory root (absolute path or
#'                      relative to working directory);
#' @param config        named list with paths configuration;
#'                      the list should define fields listed
#'                      in file: fasst-config.R;
#'

health.impact <- function(
                   project,
                   model,
                   version,
                   dir.root.out,
                   config
                 )
{
    # check input parameters
    if ( ! file.exists( dir.root.out ) )
    {
        stop( sprintf( "output directory '%s' does not exist (current working directory: '%s').", dir.root.out, getwd() ) )
    }

    # set working directory
    dir.home <- getwd()
    setwd( dir.root.out )

    # internal configuration
    dir.output  <- file.path( dir.root.out, project )
    dir.tables  <- file.path( dir.output, 'tables' )
    dir.netcdf  <- file.path( dir.output, 'ncdf' )

    # internal configuration - do not modify block - begin



    #    -- line 72 - block 2
    print( config.internal$L1 )


    # internal configuration - do not modify block - end

    # prepare output directories
    dir.create( dir.tables, recursive = TRUE, showWarnings = FALSE )



    # as last return back to home
    setwd( dir.home )
}

# ------------------------------------------------------------

# this is an internal configuration and must NOT be modified;
#
config.definition <- list(

      # available years with ssp populatation. if needed for scenarios, interpolate between those years
      ssp_yrs = c( '2000','2010','2020','2030','2040','2050','2060','2070','2080','2090','2100' ),

      # counterfactual level for SDMA8h for O3 health impact - GBD2017 uses between 29.1 and 35.7 for M3M. Malley/Turner use between 26.7 and 31.1
      SDM8THR = 29.1,
      # counterfactual level for ADMA8h for O3 health impact - Turner uses both 26.7 and 31.1
      ADM8THR = 26.7,

      # risk function parameters
      rr_param_file = 'RR_ALL_GBD_2017_FITTINGS_ANALYT.csv',

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
      AGE_GRP      = as.character( seq( from = 25, to = 95, by = 5 ) )
# --remove--      nage         = length( AGE_GRP )
)
config.derived <- list(

      # ALL AGES FROM MIN TO MAX (MAX 100)
      C1           = ( config.definition $ ages  ==  config.definition $ agefrac_copd[ 1 ] ),
      C2           = ( config.definition $ ages  == config.definition $ agefrac_copd[ 2 ] ),

      # ALL AGES FROM MIN TO MAX (MAX 100)
      L1           = ( config.definition $ ages  ==  config.definition $ agefrac_lc[ 1 ] ),
      L2           = ( config.definition $ ages  ==  config.definition $ agefrac_lc[ 2 ] ),

      # ALL AGES FROM MIN TO MAX (MAX 100)
      LR1          = ( config.definition $ ages  ==  config.definition $ agefrac_lri[ 1 ] ),
      LR2          = ( config.definition $ ages  ==  config.definition $ agefrac_lri[ 2 ] ),

      # ALL AGES FROM MIN TO MAX (MAX 100)
      DM1          = ( config.definition $ ages  ==  config.definition $ agefrac_dmt2[ 1 ] ),
      DM2          = ( config.definition $ ages  ==  config.definition $ agefrac_dmt2[ 2 ] ),

      # AGE SPECIFIC FROM MIN TO MAX (MAX 95, BECAUSE AGE-SPECIFIC RR AVAILABLE TILL 95)
      IH1          = ( config.definition $ ages  ==  config.definition $ agefrac_ihd[ 1 ] ),
      IH2          = ( config.definition $ ages  ==  config.definition $ agefrac_ihd[ 2 ] ),
      
      # OZONE MORTALITIES - NOTE that mortality rates in GBD are expressed as number divided by total population.
      O1           = ( config.definition $ ages  ==  config.definition $ agefrac_o3[ 1 ] ),
      O2           = ( config.definition $ ages  ==  config.definition $ agefrac_o3[ 2 ] )
)
config.internal <- c( config.definition, config.definition )
