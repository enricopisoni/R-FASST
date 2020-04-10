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
    print( cntrgrid )                                                       # --remove--
    latmin <- ymin( cntrgrid )
    latmax <- ymax( cntrgrid )

    # values of: cntrgrid --> matrice di: 580 righe - 1440 colonne
    # dati valori della matrice: cambia l'ordine delle righe (quelle in basso vanno in alto e vice versa )
    # CNTRCODE := matice ( 1440,720 )
    # LATFUL   := ( ( array con valori in intervallo: 0 |-| 719 ) - 360 / 4.
    # LONFUL  -- NOT USED!!!!!!
    # indmin   := indici di LATFUL dove i valori sono == latmin
    # indmax   := indici di LATFUL dove i valori sono == latmax
    # CNTRCODE[ 0 : 1439, indmin : indmax-1 ]  = cntrgrid
    # HRCNTRCODE := CNTRCODE expanded/shrinked to size: 2880,1440


    # given an empty matrix( 1440, 720 ) - all values set to 0 - where the indeces 0..719 mean -90..89.75 (steps by .25)
    # values of cntrgrid are 'overimposed' to empty matrix in order that the cntrgrid latitude, min'n'max, are correct
    # doulbes the matrix size: from 1440,720 to 2880,1440


    # prepare output directories
    dir.create( dir.tables, recursive = TRUE, showWarnings = FALSE )



    # as last return back to home
    setwd( dir.home )
}

# ------------------------------------------------------------
