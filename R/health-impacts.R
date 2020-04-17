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

library( 'raster' )

source( 'fasst-write.R' )


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
    pop <- read.csv( file = config$files$in.file.pop.country )

    # generate dataframe with unique country codes and names from POP file
    cntr <- unique(
                subset(
                        x = pop,
                        select = c( cntry_id, CNTRY_NAME, ISO )
                      )
                  )

    # read country identification gridmap (Ciesin GPW v4)
    cntrgrid <- raster( config$files$in.file.cntrgrid )

    # increase both the area and the resolution
    hrcntrcode <- disaggregate(
                        merge(
                                raster( ncol = 1440, nrow = 720, xmn = -180, xmx = 180, ymn = -90, ymx = 90 ),
                                cntrgrid
                                ),
                        fact = 2
                        )

    # files with base mortality rates (per 100k population)
    copd    <- read.csv( file = config$files$in.file.copd )
    lc      <- read.csv( file = config$files$in.file.lc )
    lri     <- read.csv( file = config$files$in.file.lri )
    ihd     <- read.csv( file = config$files$in.file.ihd )
    stroke  <- read.csv( file = config$files$in.file.stroke )
    dmt2    <- read.csv( file = config$files$in.file.dmt2 )

    # read the fitting parameters for the Burnett IER functions for all CODs and age classes
    rr <- read.csv( file = config$files$in.file.rr )

    copd.med <- rr$MED[     ( rr$COD == 'COPD' ) & ( rr$AGE == 99 ) ]
    copd.lo  <- rr$X95CL_L[ ( rr$COD == 'COPD' ) & ( rr$AGE == 99 ) ]
    copd.hi  <- rr$X95CL_H[ ( rr$COD == 'COPD' ) & ( rr$AGE == 99 ) ]

    lri.med <- rr$MED[     ( rr$COD == 'LRI' ) & ( rr$AGE == 99 ) ]
    lri.lo  <- rr$X95CL_L[ ( rr$COD == 'LRI' ) & ( rr$AGE == 99 ) ]
    lri.hi  <- rr$X95CL_H[ ( rr$COD == 'LRI' ) & ( rr$AGE == 99 ) ]

    lc.med <- rr$MED[     ( rr$COD == 'LC' ) & ( rr$AGE == 99 ) ]
    lc.lo  <- rr$X95CL_L[ ( rr$COD == 'LC' ) & ( rr$AGE == 99 ) ]
    lc.hi  <- rr$X95CL_H[ ( rr$COD == 'LC' ) & ( rr$AGE == 99 ) ]

    dt2.med <- rr$MED[     ( rr$COD == 'DT2' ) & ( rr$AGE == 99 ) ]
    dt2.lo  <- rr$X95CL_L[ ( rr$COD == 'DT2' ) & ( rr$AGE == 99 ) ]
    dt2.hi  <- rr$X95CL_H[ ( rr$COD == 'DT2' ) & ( rr$AGE == 99 ) ]

    # extract the appropriate RR parameters for each COD and assign to each variable - for easier tracking.
    ihd.med <- matrix( nrow = 4, data = rr$MED[     rr$COD == 'IHD' & rr$AGE %in% config$model$AGE_GRP ] )
    ihd.lo  <- matrix( nrow = 4, data = rr$X95CL_L[ rr$COD == 'IHD' & rr$AGE %in% config$model$AGE_GRP ] )
    ihd.hi  <- matrix( nrow = 4, data = rr$X95CL_H[ rr$COD == 'IHD' & rr$AGE %in% config$model$AGE_GRP ] )

    stroke.med <- matrix( nrow = 4, data = rr$MED[     rr$COD == 'STROKE' & rr$AGE %in% config$model$AGE_GRP ] )
    stroke.lo  <- matrix( nrow = 4, data = rr$X95CL_L[ rr$COD == 'STROKE' & rr$AGE %in% config$model$AGE_GRP ] )
    stroke.hi  <- matrix( nrow = 4, data = rr$X95CL_H[ rr$COD == 'STROKE' & rr$AGE %in% config$model$AGE_GRP ] )


    # Ozone RRs with log-lin ER function
    beta_rr_copd_tu  <- c( 1.14, 1.08, 1.21 ) / 10        # new TURNER!, for new exposure metric annual mean of daily 8h max!
    beta_rr_copd_gbd <- c( 1.06, 1.05, 1.10 ) / 10        # new GBD2017!, for new exposure metric 6-month mean of daily 8h max!


    # *** file 'country_mask_0.5x0.5_v3.sav' is loaded but NOT used;

    # high resolution lon lat dimensions
    img     <- ncol( hrcntrcode )
    jmg     <- nrow( hrcntrcode )
    scale   <- img / ( xmax( hrcntrcode ) - xmin( hrcntrcode ) )
# --not-used--    hr_lats <- ( ( ( 0:( ( ymax( hrcntrcode ) - ymin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + ymin( hrcntrcode ) )
# --not-used--    hr_lons <- ( ( ( 0:( ( xmax( hrcntrcode ) - xmin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + xmin( hrcntrcode ) )

    # med resolution lon lat dimensions
    imed    <- 720   # img / 4 ?
    jmed    <- 360   # jmg / 4 ?
    scale   <- img / ( xmax( hrcntrcode ) - xmin( hrcntrcode ) )
    mr_lats <- ( ( ( 0:( ( ymax( hrcntrcode ) - ymin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + ymin( hrcntrcode ) )
    mr_lons <- ( ( ( 0:( ( xmax( hrcntrcode ) - xmin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + xmin( hrcntrcode ) )

    # lon-lat arrays
    hr_grid_tot  <- array( 0, c( jmg, img ) )
    hr_grid_tot1 <- array( 0, c( jmg, img ) )    # intermediate placeholders needed when interpolating
    hr_grid_tot2 <- array( 0, c( jmg, img ) )    # intermediate placeholders needed when interpolating
    scenpopmask  <- hr_grid_tot
    scenpop      <- hr_grid_tot
    iminlat      <- 273                                    # index where hr_lats eq min lat in SSP hr grid
    imaxlat      <- 1389                                   # index where hr_lats eq max lat in SSP hr grid

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
            # CHECK IF SSP POPULATION YEAR IS AVAILABLE, IF NOT:INTERPOLATE BETWEEN AVAILABLE YEARS
            intpol <- FALSE
            npop   <- max( as.numeric( config $ model $ ssp_yrs[ config $ model $ ssp_yrs <= year ] ) )
            jpop   <- min( as.numeric( config $ model $ ssp_yrs[ config $ model $ ssp_yrs >= year ] ) )

            if ( npop != jpop )
            {
                intpol <- TRUE
                fyr    <- ( year - npop ) / ( jpop - npop )
            }
            else        # *** this branch is not present in the original IDL prg (bug?)
            {
                fyr <- npop
            }

            # restore HIGH RESOLUTION (HIRES) population map(s) and interpolate if needed




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
