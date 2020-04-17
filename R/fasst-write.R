# -*- mode: R -*-

#' writes the output in text file in tabular format;
#'
#' @param dir.output    the output directory; it can be defined either with
#'                      a full path or a relative path from the working
#'                      directory; directory will be created if it does not
#'                      exist; file will be overwritten if it already exists;
#' @param parameters    named list with following entries:
#'                      \describe{
#'                         \item{project.name}    {the project name}
#'                         \item{model.name}      {the model name}
#'                         \item{model.version}   {the model version}
#'                         \item{sDM8THR}         {the counterfactual level for SDMA8h}
#'                         \item{ADM8THR}         {the counterfactual level for ADMA8h}
#'                         \item{AGEFRAC_COPD}    {all ages from min to max}
#'                         \item{AGEFRAC_LC}      {all ages from min to max}
#'                         \item{AGEFRAC_LRI}     {all ages from min to max}
#'                         \item{AGEFRAC_IHD}     {all ages from min to max}
#'                         \item{AGEFRAC_O3}      {all ages from min to max}
#'                         \item{} {}
#'                         \item{} {}
#'                      }
#'
fasst.write <- function(
               dir.output,
               parameters
            )
{
    # prepare output directories
    dir.create( dir.output, recursive = TRUE, showWarnings = FALSE )

    # prepare file name
    file.name <-file.path(
                           dir.output,
                           paste(
                                paste( 'ALLCNTRIES', parameters$project.name, parameters$model.name, parameters$model.version, sep = '_' ),
                                'TXT',
                                sep = '.'
                           )
                       )
    
    # create output file
    file <- file( file.name, 'wt' )

    # write headers
    writeLines( 'NEW GBD2017 IER', file )
    writeLines( 'Programme: FASST 4 SHERPA', file )
    writeLines( paste( 'Date of run:', format( Sys.time(), "%c" ), sep = ' ' ), file )
    writeLines( paste( 'GBD 6MDMA8H threshold = ', parameters $ sDM8THR, sep = ' ' ), file )
    writeLines( paste( 'TURNER ADMA8H threshold = ', parameters $ ADM8THR, sep = ' ' ), file )
    writeLines( paste( 'IER MODEL:', parameters $ model.name, sep = ' ' ), file )
    writeLines( paste( 'AGE CLASSES:', sep = ' ' ), file )
    writeLines( paste( 'COPD:',         paste( parameters $ AGEFRAC_COPD, collapse = ', ' ), sep = ' ' ), file )
    writeLines( paste( 'LC:',           paste( parameters $ AGEFRAC_LC,   collapse = ', ' ), sep = ' ' ), file ) 
    writeLines( paste( 'LRI:',          paste( parameters $ AGEFRAC_LRI,  collapse = ', ' ), sep = ' ' ), file ) 
    writeLines( paste( 'IHD + STROKE:', paste( parameters $ AGEFRAC_IHD,  collapse = ', ' ), sep = ' ' ), file ) 
    writeLines( paste( 'COPD O3:',      paste( parameters $ AGEFRAC_O3,   collapse = ', ' ), sep = ' ' ), file )  








    close( file )
}
