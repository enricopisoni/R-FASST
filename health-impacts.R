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

# --remove--  print( config$files$in.file.copd )
# --remove--      print( head( copd   ))              # --remove--
# --remove--  print( config$files$in.file.lc )
# --remove--      print( head( lc     ))
# --remove--  print( config$files$in.file.lri )
# --remove--      print( head( lri    ))
# --remove--  print( config$files$in.file.ihd )
# --remove--      print( head( ihd    ))
# --remove--  print( config$files$in.file.stroke )
# --remove--      print( head( stroke ))
# --remove--  print( config$files$in.file.dmt2 )
# --remove--      print( head( dmt2   ))


    # read the fitting parameters for the Burnett IER functions for all CODs and age classes
    rr <- read.csv( file = config$files$in.file.rr )
# --remove--print( config$files$in.file.rr )                                        # --remove--
# --remove--    print( head( rr   ))

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

# --remove--    print( copd.med )      #--remove--
# --remove--    print( copd.lo  )
# --remove--    print( copd.hi  )
# --remove--    print( lri.med  )
# --remove--    print( lri.lo   )
# --remove--    print( lri.hi   )
# --remove--    print( lc.med   )
# --remove--    print( lc.lo    )
# --remove--    print( lc.hi    )
# --remove--    print( dt2.med  )
# --remove--    print( dt2.lo   )
# --remove--    print( dt2.hi   )

    # extract the appropriate RR parameters for each COD and assign to each variable - for easier tracking.
    ihd.med <- matrix( nrow = 4, data = rr$MED[     rr$COD == 'IHD' & rr$AGE %in% config$model$AGE_GRP ] )
    ihd.lo  <- matrix( nrow = 4, data = rr$X95CL_L[ rr$COD == 'IHD' & rr$AGE %in% config$model$AGE_GRP ] )
    ihd.hi  <- matrix( nrow = 4, data = rr$X95CL_H[ rr$COD == 'IHD' & rr$AGE %in% config$model$AGE_GRP ] )

# --remove--  print( ihd.med )            # --remove--
# --remove--  print( ihd.lo )
# --remove--  print( ihd.hi )

    stroke.med <- matrix( nrow = 4, data = rr$MED[     rr$COD == 'STROKE' & rr$AGE %in% config$model$AGE_GRP ] )
    stroke.lo  <- matrix( nrow = 4, data = rr$X95CL_L[ rr$COD == 'STROKE' & rr$AGE %in% config$model$AGE_GRP ] )
    stroke.hi  <- matrix( nrow = 4, data = rr$X95CL_H[ rr$COD == 'STROKE' & rr$AGE %in% config$model$AGE_GRP ] )

# --remove--  print( stroke.med )            # --remove--
# --remove--  print( stroke.lo )
# --remove--  print( stroke.hi )






    # prepare output directories
    dir.create( dir.tables, recursive = TRUE, showWarnings = FALSE )



    # as last return back to home
    setwd( dir.home )
}

# ------------------------------------------------------------
