# -*- mode: R -*-

library( 'raster' )
library( 'tidyverse' )

source( 'fasst-write.R' )
source( 'rrate.R' )


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
    print( sprintf( "Working directory is now: '%s'", getwd() ) )

    # internal configuration
    dir.output  <- file.path( '.', project )
    dir.tables  <- file.path( dir.output, 'tables' )
    dir.netcdf  <- file.path( dir.output, 'ncdf' )


    # ----------------------------------------------------------------------
    # ------------------------------- block 2-------------------------------
    # load population, base mortality data and risk rate function parameters
    # ----------------------------------------------------------------------

    # note: available population totals from SSP: 2000, 2010, 2020, 2030, ..., 2100
    # available base mortalities and fraction of pop <5yr and >30yr: 2005 2010 2015 2030 2050.
    # If other years are needed, an additional interpolation is carried out on the latter.
    # For years > 2040 mortality stats for 2040 are used.
    # For years < 2005, mortality stats for 2005 are used.

    # population country totals
    pop <- read_csv( config $ files $ in.file.pop.country,
                     skip = 1,
                     col_types = config $ tmpls $ pop.type,
                     col_names = config $ tmpls $ pop.name
                     )

    # generate dataframe with unique country codes and names from POP file
    cntr <- unique(
                subset(
                        x = pop,
                        select = c( CNTR_ID, CNTR_NAME, CNTR_ISO3 )
                      )
                  )

    # read country identification gridmap (Ciesin GPW v4)
    cntrgrid <- raster( config $ files $ in.file.cntrgrid )                 # here we don't flip rows!

    # increase both the area and the resolution
    hrcntrcode <- disaggregate(
                        merge(
                                raster( ncol = 1440, nrow = 720, xmn = -180, xmx = 180, ymn = -90, ymx = 90 ),
                                cntrgrid
                                ),
                        fact = 2
                        )

    # files with base mortality rates (per 100k population)
    copd       <- read_csv( config $ files $ in.file.copd,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    lc         <- read_csv( config $ files $ in.file.lc,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    lri        <- read_csv( config $ files $ in.file.lri,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    ihd        <- read_csv( config $ files $ in.file.ihd,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    stroke     <- read_csv( config $ files $ in.file.stroke,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    dmt2       <- read_csv( config $ files $ in.file.dmt2,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )

    # read the fitting parameters for the Burnett IER functions for all CODs and age classes
    rr         <- read_csv( config $ files $ in.file.rr,
                            skip = 1,
                            col_types = config $ tmpls $ rr.type,
                            col_names = config $ tmpls $ rr.name
                            )

    copd_med   <- rr $ RRMED[ ( rr $ COD == 'COPD' ) & ( rr $ AGE == 99 ) ]
    copd_lo    <- rr $ RRLO[  ( rr $ COD == 'COPD' ) & ( rr $ AGE == 99 ) ]
    copd_hi    <- rr $ RRHI[  ( rr $ COD == 'COPD' ) & ( rr $ AGE == 99 ) ]

    lri_med    <- rr $ RRMED[ ( rr $ COD == 'LRI' ) & ( rr $ AGE == 99 ) ]
    lri_lo     <- rr $ RRLO[  ( rr $ COD == 'LRI' ) & ( rr $ AGE == 99 ) ]
    lri_hi     <- rr $ RRHI[  ( rr $ COD == 'LRI' ) & ( rr $ AGE == 99 ) ]

    lc_med     <- rr $ RRMED[ ( rr $ COD == 'LC' ) & ( rr $ AGE == 99 ) ]
    lc_lo      <- rr $ RRLO[  ( rr $ COD == 'LC' ) & ( rr $ AGE == 99 ) ]
    lc_hi      <- rr $ RRHI[  ( rr $ COD == 'LC' ) & ( rr $ AGE == 99 ) ]

    dt2_med    <- rr $ RRMED[ ( rr $ COD == 'DT2' ) & ( rr $ AGE == 99 ) ]
    dt2_lo     <- rr $ RRLO[  ( rr $ COD == 'DT2' ) & ( rr $ AGE == 99 ) ]
    dt2_hi     <- rr $ RRHI[  ( rr $ COD == 'DT2' ) & ( rr $ AGE == 99 ) ]

    # extract the appropriate RR parameters for each COD and assign to each variable - for easier tracking.
    ihd.med    <- matrix( nrow = 4, data = rr $ RRMED[ rr $ COD == 'IHD' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    ihd.lo     <- matrix( nrow = 4, data = rr $ RRLO[  rr $ COD == 'IHD' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    ihd.hi     <- matrix( nrow = 4, data = rr $ RRHI[  rr $ COD == 'IHD' & rr $ AGE %in% config $ model $ AGE_GRP ] )

    stroke.med <- matrix( nrow = 4, data = rr $ RRMED[ rr $ COD == 'STROKE' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    stroke.lo  <- matrix( nrow = 4, data = rr $ RRLO[  rr $ COD == 'STROKE' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    stroke.hi  <- matrix( nrow = 4, data = rr $ RRHI[  rr $ COD == 'STROKE' & rr $ AGE %in% config $ model $ AGE_GRP ] )


    # Ozone RRs with log-lin ER function
    beta_rr_copd_tu  <- c( 1.14, 1.08, 1.21 ) / 10        # new TURNER!, for new exposure metric annual mean of daily 8h max!
    beta_rr_copd_gbd <- c( 1.06, 1.05, 1.10 ) / 10        # new GBD2017!, for new exposure metric 6-month mean of daily 8h max!


    # *** file 'country_mask_0.5x0.5_v3.sav' is loaded but NOT used;

    # high resolution lon lat dimensions
    img          <- ncol( hrcntrcode )
    jmg          <- nrow( hrcntrcode )
    scale        <- img / ( xmax( hrcntrcode ) - xmin( hrcntrcode ) )
# --not-used--    hr_lats <- ( ( ( 0:( ( ymax( hrcntrcode ) - ymin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + ymin( hrcntrcode ) )
# --not-used--    hr_lons <- ( ( ( 0:( ( xmax( hrcntrcode ) - xmin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + xmin( hrcntrcode ) )

    # med resolution lon lat dimensions
    imed         <- 720   # img / 4 ?
    jmed         <- 360   # jmg / 4 ?
    scale        <- img / ( xmax( hrcntrcode ) - xmin( hrcntrcode ) )
    mr_lats      <- ( ( ( 0:( ( ymax( hrcntrcode ) - ymin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + ymin( hrcntrcode ) )
    mr_lons      <- ( ( ( 0:( ( xmax( hrcntrcode ) - xmin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + xmin( hrcntrcode ) )

    # lon-lat arrays
# --not-used-remove--    hr_grid_tot  <- array( 0, c( jmg, img ) )
# --not-used-remove--    hr_grid_tot1 <- array( 0, c( jmg, img ) )    # intermediate placeholders needed when interpolating
# --not-used-remove--    hr_grid_tot2 <- array( 0, c( jmg, img ) )    # intermediate placeholders needed when interpolating
# --not-used-remove--    scenpopmask  <- hr_grid_tot
# --not-used-remove--    scenpop      <- hr_grid_tot

    # layers containing the AFs, depends on PM fields
    af_copd_grid   <- array( 0, c( 3, jmg, img ) )       # ALL AGES >25
    af_lri_grid    <- array( 0, c( 3, jmg, img ) )       # ALL AGES
    af_lc_grid     <- array( 0, c( 3, jmg, img ) )       # ALL AGES >25
    af_dmt2_grid   <- array( 0, c( 3, jmg, img ) )       # ALL AGES >25
    af_ihd_grid    <- array( 0, c( 3, 15, jmg, img ) )   # 15 AGE CLASSES >25
    af_stroke_grid <- array( 0, c( 3, 15, jmg, img ) )   # 15 AGE CLASSES >25

    # BLOCK 3 ############ START LOOP WITH SCENARIO ANALYSIS  - EACH LOOP = 1 SCENARIO #################
    for ( scen in config $ file $ scenarios $ name )
        for ( year in config $ file $ scenarios $ year )
        {
            # restore HIGH RESOLUTION (HIRES) population map(s) and interpolate if needed
            population.map <- get.population.map(
                                        scen,
                                        year,
                                        config $ model $ ssp_yrs,
                                        config $ file  $ in.tmpl.pop.map
                              )

            hr_grid_tot    <- merge(
                                        raster( ncol = img, nrow = jmg, xmn = -180, xmx = 180, ymn = -90, ymx = 90 ),
                                        population.map
                              )
            hr_grid_tot[ is.na( hr_grid_tot[] ) ] <- 0

            # identify grids with valid population data
            hr_grid_mask <- ( hr_grid_tot > 0 ) & ( hr_grid_tot <  population.map @ file @ nodatavalue )

            # read the scenario input file (high resolution grid map)
            infile <- get.file.name.population(
                                config $ file $ in.tmpl.scenario,
                                scen,
                                year
                      )
            print( sprintf( "Processing file '%s'", infile ) )

            # calculate attributable fractions AF = 1-1/RR for central values, low and high confidence interval bound
            print( sprintf( "Calculate AFs @ %s", format( Sys.time(), "%c" ) ) )

            sc_hires      <- raster( infile, varname = 'TOT_PM_35' )  # extract total pm from SC structure and load into SC_HIRES variable
            sc_anth_hires <- raster( infile, varname = 'ANT_PM_35' )  # extract anthropogenic pm from SC structure and load into SC_HIRES variable

            # calculate attributable fractions AF = 1-1/RR for central values, low and high confidence interval bound
            af_ac <- compute.attributable.functions(
                                sc_hires,
                                copd_med,
                                copd_lo,
                                copd_hi
                     )

            af_lc <- compute.attributable.functions(
                                sc_hires,
                                lc_med,
                                lc_lo,
                                lc_hi
                     )

            af_lri <- compute.attributable.functions(
                                sc_hires,
                                lri_med,
                                lri_lo,
                                lri_hi
                      )

            af_dmt2 <- compute.attributable.functions(
                                sc_hires,
                                dt2_med,
                                dt2_lo,
                                dt2_hi
                       )

        }


    # write the output
    fasst.write(
             dir.tables,
             list(
                   project.name  = project,
                   model.name    = model,
                   model.version = version,
                   sDM8THR       = config $ model $ SDM8THR,
                   ADM8THR       = config $ model $ ADM8THR,
                   AGEFRAC_COPD  = config $ model $ agefrac_copd,
                   AGEFRAC_LC    = config $ model $ agefrac_lc ,
                   AGEFRAC_LRI   = config $ model $ agefrac_lri,
                   AGEFRAC_IHD   = config $ model $ agefrac_ihd,
                   AGEFRAC_O3    = config $ model $ agefrac_o3
             )
          )




    # as last return back to home
    setwd( dir.home )
}

# ------------------------------------------------------------

#' The function gets from NetCDF file the population map(s);
#' if needed, formal parameter \code{interpolation} set at TRUE,
#' an interpolation is carried out to create the map;
#'
#' Constraints:
#'      NetCDF files must define the following variables:
#'      \description{
#'          \item{2000total}
#'                      {for files with values about year 2000;}
#'          \item{<scenario>_<year>}
#'                      {for all other files;}
#'      }
#'
#' @param scenario         the scenario to use;
#' @param year             the year we are working on;
#' @param ssp_yrs          the available years;
#' @param netcdf.template  path template to NetCDF files;
#'                         path can be either absolute or relative the
#'                         working directory;
#'                         path template can contain the placeholders:
#'                         \describe{
#'                              {scenario} {the scenario name;}
#'                              {year}     {the year;}
#'                         }
#'
get.population.map <- function(
                                scenario,
                                year,
                                ssp_yrs,
                                netcdf.template
                      )
{
        # CHECK IF SSP POPULATION YEAR IS AVAILABLE, IF NOT:INTERPOLATE BETWEEN AVAILABLE YEARS
        intpol <- FALSE
        npop   <- max( ssp_yrs[ ssp_yrs <= year ] )
        jpop   <- min( ssp_yrs[ ssp_yrs >= year ] )

        if ( npop != jpop )
        {
            intpol <- TRUE
            fyr    <- ( year - npop ) / ( jpop - npop )
        }
        else
        {
            # *** this branch is not present in the original IDL prg
            fyr    <- 1
        }

        if ( ! intpol )
        {
                if ( year == 2000 )
                {
                        varname <- '2000total'
                } else {
                        varname <- paste( str_to_lower( scenario ), year, sep = '_' )
                }
                totfil <- get.file.name.population( netcdf.template, scenario, year )

                r      <- raster( totfil, varname = varname )
# ---                grid <- array(
# ---                                getValues( r ),                 # here we don't flip by rows!
# ---                                c( nrow( r ), ncol( r ) )
# ---                        )

        } else {

                if ( year == 2000 )
                {
                        varname <- '2000total'
                } else {
                        varname <- paste( str_to_lower( scenario ), npop, sep = '_' )
                }
                totfil <- get.file.name.population( netcdf.template, scenario, npop )

                r_n    <- raster( totfil, varname = varname )
# ---                grid_n <- array(
# ---                                getValues( r_n ),                 # here we don't flip by rows!
# ---                                c( nrow( r_n ), ncol( r_n ) )
# ---                          )

                if ( year == 2000 )
                {
                        varname <- '2000total'
                } else {
                        varname <- paste( str_to_lower( scenario ), jpop, sep = '_' )
                }
                totfil <- get.file.name.population( netcdf.template, scenario, jpop )

                r_j    <- raster( totfil, varname = varname )
# ---                grid_j <- array(
# ---                                getValues( r_j ),                 # here we don't flip rows!
# ---                                c( nrow( r_j ), ncol( r_j ) )
# ---                          )

                # interpolate years
                r <- r_n + fyr * ( r_j - r_n )
# ---                grid <- grid_n + fyr * ( grid_j - grid_n )

                # don't lose the no-data value
                r @ file @ nodatavalue = r_n @ file @ nodatavalue
        }
        return ( r )
}
get.file.name.population <- function(
                                path.remplate,
                                scene,
                                year
                            )
{
        pattern <- c( '\\$\\{scenario\\}' = scene, '\\$\\{year\\}' = year )

        str_replace_all( path.remplate, pattern )
}

# ------------------------------------------------------------

#' The function computes the attributable functions for
#' central values, low and high confidence interval bound;
#'
#' @param sc  total pm from SC structure;
#' @param med
#' @param lo
#' @param hi
#'
#' @return named list with fields:
#'         \describe{
#'              \item{grid}    {raster brick with central, low and high values;}
#'              \item{sig_min} {standard deviation;}
#'              \item{sig_max} {standard deviation;}
#'         }
#'

compute.attributable.functions <- function(
                                        sc,
                                        med,
                                        lo,
                                        hi
                                  )
{
        grid <- brick(
                        1 - 1 / rrate( med, sc ),
                        1 - 1 / rrate( lo,  sc ),
                        1 - 1 / rrate( hi,  sc )
                )
        names( grid ) <- c( 'central', 'low', 'high' )

        list(
                grid    = grid,

                sig_min = grid[[1]] *
                          sqrt(
                                (
                                        ( rrate( med, sc ) - rrate( lo, sc ) )
                                        /
                                        rrate( med, sc )
                                ) ^2
                          ),

                sig_max = grid[[1]] *
                          sqrt(
                                (
                                        ( rrate( med, sc ) - rrate( hi, sc ) )
                                        /
                                        rrate( med, sc)
                                ) ^2
                          )
        )
}
