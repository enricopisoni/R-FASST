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
#' @param in.file.mr      country mortality archive; input file, if it has been
#'                        already computed in a previous run; otherwise, output
#'                        file if it does not exist;
#' @param year            the current year;
#' @param countries.grid  the countries grid;
#' @param countries.list  the list of all countries;
#'
#' @return
#'

# http://geog.uoregon.edu/GeogR/topics/netCDF-write-ncdf4.html

get.base.incidences <- function(
                            in.file.mr,
                            year,
                            countries.grid,
                            countries.list
                       )
{
        # --- check if input file exists ---
print( in.file.mr )                                             # --remove--
        if ( file.exists( in.file.mr ) )
        {
                # --- input file exists, read it ---


        } else {

                # --- input file does not exist, compute it ---
                compute.base.incidences(
                            year,
                            countries.grid,
                            countries.list
                       )


                # --- input file does not exist, write it ---


        }
}

# ------------------------------------------------------------

#' Core function to compute the mortality base incidence;
#'
#' @param year            the current year;
#' @param countries.grid  the countries grid;
#' @param countries.list  the list of all countries;
#' @param
#'
#' @return
#'

compute.base.incidences <- function(
                            year,
                            countries.grid,
                            countries.list
                       )
{
    img           <- ncol( countries.grid )
    jmg           <- nrow( countries.grid )

print( img )                                            #--remove--
print( jmg )                                            #--remove--

    MRATE_COPD0   <- array( 0, c( img, jmg ) )
    MRATE_COPD1   <- array( 0, c( img, jmg ) )
    MRATE_COPD2   <- array( 0, c( img, jmg ) )

    MRATE_LC0     <- array( 0, c( img, jmg ) )
    MRATE_LC1     <- array( 0, c( img, jmg ) )
    MRATE_LC2     <- array( 0, c( img, jmg ) )

    MRATE_LRI0    <- array( 0, c( img, jmg ) )
    MRATE_LRI1    <- array( 0, c( img, jmg ) )
    MRATE_LRI2    <- array( 0, c( img, jmg ) )

    MRATE_DMT20   <- array( 0, c( img, jmg ) )
    MRATE_DMT21   <- array( 0, c( img, jmg ) )
    MRATE_DMT22   <- array( 0, c( img, jmg ) )

    MRATE_IHD0    <- array( 0, c( img, jmg ) )
    MRATE_IHD1    <- array( 0, c( img, jmg ) )
    MRATE_IHD2    <- array( 0, c( img, jmg ) )

    MRATE_STROKE0 <- array( 0, c( img, jmg ) )
    MRATE_STROKE1 <- array( 0, c( img, jmg ) )
    MRATE_STROKE2 <- array( 0, c( img, jmg ) )

    # lOOP THROUGH COUNTRIES; RETRIEVE BASE MORTALITY RATES AND MAP TO EACH OF
    # THE 3 GRID LAYERS (MED,LO,UP)
    for ( cntr.id in  countries.list $ CNTR_ID )
    {
print( cntr.id )                                           # --remove--
       # country identification
       imask <-  countries.grid == cntr.id

       # new code for Sudan since splitting South Sudan
       if ( cntr.id == 736 )
       {
            imask <-  countries.grid == 729             # !!!!!!!!!!!!!11111
       }
    }

}
