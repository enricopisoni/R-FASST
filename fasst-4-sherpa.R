# -*- mode: R -*-

#' script to glue together functions and command line arguments;
#'
#' @name fasst-4-sherpa
#'
#' Command line arguments are:listed in \code{script.args};
#'


source( 'fasst-config.R' )
source( 'health-impacts.R' )


script.name <- "fasst-4-sherpa"
script.args <- c(
    "project name",
    "model name",
    "version",
    "path to output directory, it must be a either a full path or a relative path from current directory"
)
script.opts <- c(
    "configuration JSON file, either an absolute path or a relative one from the working directory"
)

help.banner <- function( name, arguments, optional = NULL )
{
    banner <- sprintf( "%s\nCommand line arguments:\n", name )
    for( arg in arguments )
    {
        banner <- paste( banner, sprintf( " - %s;\n", arg ), sep = '' )
    }
    if ( ! is.null( optional ) )
    {
        banner <- paste( banner, "Optional arguments:\n", sep = '' )
        for( arg in optional )
        {
            banner <- paste( banner, sprintf( " - %s;\n", arg ), sep = '' )
        }
    }
    banner
}


args = commandArgs( trailingOnly = TRUE )

if ( length( args ) >= length( script.args ) )
{
    health.impact( args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], health.impact.config( args[ 5 ] ) )
} else {
    stop( help.banner( script.name, script.args, optiona = script.opts ) )
}
