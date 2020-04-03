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
    "either a relative path from working directory or full path to output directory"
)

help.banner <- function( name, arguments )
{
    banner <- sprintf( "%s\ncommand line arguments:\n", name )
    for( arg in arguments )
    {
        banner <- paste( banner, sprintf( " - %s;\n", arg ) )
    }
    banner
}


args = commandArgs( trailingOnly = TRUE )
if ( length( args ) >= length( script.args ) )
{
    health.impact( args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], health.impact.config() )
} else {
    stop( help.banner( script.name, script.args ) )
}
