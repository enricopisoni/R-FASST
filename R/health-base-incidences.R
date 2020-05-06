# -*- mode: R -*-

library( 'ncdf4' )


#' Base incidences (MED,LO,UP) will be calculated and stored if gridmap was
#' not previously stored (for particular year). Otherwise retrieve stored
#' files.
#'
#' Calculation done for central, lower and upper boundary values,
#' and for each of 15 age classes for IHD and STROKE.
#'
#' Method: read country mortality values from a restored ASCI table
#' and assign the value to each grid cell of the country in the global high
#' resolution map.
#'
#' @param in.file.mr    country mortality archive; input file, if it has been
#'                      already computed in a previous run; otherwise, output
#'                      file if it does not exist;
#' @param 
#'
#' @return
#'

# http://geog.uoregon.edu/GeogR/topics/netCDF-write-ncdf4.html

get.base.incidences <- function(
                            in.file.mr
                       )
{
}

