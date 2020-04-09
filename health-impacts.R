# -*- mode: R -*-

# this is an internal configuration and must NOT be modified;
#



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

