# -*- mode: R -*-

#' Read the FASST configuration from JSON file;
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
