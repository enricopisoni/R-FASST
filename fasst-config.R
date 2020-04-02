# -*- mode: R -*-

#' FASST configuration with the definition of files
#' needed by function: health:impact (health-impacts.R);
#'
#' Notes:
#'   - all files paths are relative to working directory;
#'
#' @Return
#'   named list with input/output files;
#'

health:impact:config <- function()
{
        in.dir.root     <- '.'
        in.dir.ancil    <- file.path( in.dir.root, 'ANCILLARY' )
        in.dir.gbd      <- file.path( in.dir.ancil, 'MORTALITY', 'BASEMORT2018' )
        in.dir.rr       <- file.path( in.dir.ancil, 'MORTALITY', 'RRs2018', 'FIT' )
        in.dir.tmpls    <- file.path( in.dir.root, 'CODE', 'TEMPLATES' )
        in.dir.ssp      <- file.path( in.dir.ancil, 'POPULATION_SSP', 'NETCDF' )
        
        c
        (
                in.file.pop-country = file.path( in.dir.gbd, 'POP_1990-2100_UN2017_AGEGRP.csv),

                # files with base mortality rates (per 100k population)
                in.file.copd        = file.path( in.dir.gbd, 'COPD_MORT_RATE_GBD2016.csv' ),
                in.file.lc          = file.path( in.dir.gbd, 'LC_MORT_RATE_GBD2016.csv' ),
                in.file.dmt2        = file.path( in.dir.gbd, 'DMT2_MORT_RATE_GBD2016.csv' ),
                in.file.lri         = file.path( in.dir.gbd, 'LRI_MORT_RATE_GBD2016.csv' ),
                in.file.ihd         = file.path( in.dir.gbd, 'IHD_MORT_RATE_GBD2016.csv' ),
                in.file.stroke      = file.path( in.dir.gbd, 'STROKE_MORT_RATE_GBD2016.csv' )

        )
}
