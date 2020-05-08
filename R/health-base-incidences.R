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
#' @param copd
#' @param lc
#' @param lri
#' @param dmt2
#'
#' @return
#'

get.base.incidences <- function(
                            in.file.mr,
                            year,
                            countries.grid,
                            countries.list,
                            copd,
                            lc,
                            lri,
                            dmt2
                       )
{
        # --- check if input file exists ---
        if ( file.exists( in.file.mr ) )
        {
                # --- input file exists, read it ---


        } else {

                # --- input file does not exist, compute it ---
                compute.base.incidences(
                            year,
                            countries.grid,
                            countries.list,
                            copd,
                            lc,
                            lri,
                            dmt2
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
#' @param copd
#' @param lc
#' @param lri
#' @param dmt2
#'
#' @return
#'

compute.base.incidences <- function(
                            year,
                            countries.grid,
                            countries.list,
                            copd,
                            lc,
                            lri,
                            dmt2
                       )
{
print( 'countries.list' )                               #--remove--
print( countries.list )                                 # --remove--
print( 'copd' )                                         #--remove
print( copd )                                         #--remove


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

    # check whether the year we are working on exists
    theyear <- NULL
    maxyear <- max( copd $ YEAR )
    minyear <- min( copd $ YEAR )
    if ( year > maxyear )
    {
        theyear <- maxyear
    } else if ( year < minyear ) {
        theyear <- minyear
    } else {
        if ( year %in%  copd $ YEAR )
        {
           theyear <- year
        }
    }
print( theyear )                                        # --remove--

    # country names in upper case (used to match country names)
    countries   <- countries.list                                                       %>%
                   select( CNTR_ID, CNTR_NAME )                                         %>%
                   transmute(
                        CNTR_ID   = CNTR_ID,
                        CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en")
                   )
print( countries )                                                                              # --remove--

    # read the values or interpolate if the requested year does not exist
    if ( is.null( theyear ) )
    {
      year.lo     <-  max( copd $ YEAR[ copd $ YEAR <= year ] )
      year.hi     <-  min( copd $ YEAR[ copd $ YEAR >= year ] )

      copd.bycntr <-  join.filter.interpolated( copd, countries, year.lo, year.hi )

      lc.bycntr   <-  join.filter.interpolated( lc,   countries, year.lo, year.hi )

      lri.bycntr  <-  join.filter.interpolated( lri,  countries, year.lo, year.hi )

      dmt2.bycntr <-  join.filter.interpolated( dmt2, countries, year.lo, year.hi )

    } else {

      copd.bycntr <- join.filter( copd, countries, year )

      lc.bycntr   <- join.filter( lc,   countries, year )

      lri.bycntr  <- join.filter( lri,  countries, year )

      dmt2.bycntr <- join.filter( dmt2, countries, year )
    }

print( copd.bycntr )                                              # --remove--
print( lc.bycntr )                                              # --remove--
print( lri.bycntr )                                              # --remove--
print( dmt2.bycntr )                                              # --remove--

}

# ------------------------------------------------------------

#' Retrieve values by country and year then add the United Nations
#' country identifier;
#'
#' @param table      the data tibble;
#' @param countries  the countries name and identifier;
#' @param year       the year to filter;
#'
#' @return  join between 'table' and 'countries' filtered
#'          by 'year';
#'

join.filter <- function(
                   table,
                   countries,
                   year
               )
{
   table                                                                %>%
   select( CNTR_NAME, YEAR, VAL, LO, HI )                               %>%
   filter( YEAR == year )                                               %>%
   transmute(
       CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
       VAL       = VAL,
       LO        = LO,
       HI        = HI
   )                                                                    %>%
   inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )
}

# ------------------------------------------------------------

#' Retrieve country values by interpolating values of two years
#' then add the United Nations country identifier;
#'
#' @param table      the data tibble;
#' @param countries  the countries name and identifier;
#' @param year.lo    the lower year to filter;
#' @param year.hi    the higher year to filter;
#'
#' @return  join between 'table' and 'countries' filtered
#'          by 'year';
#'

join.filter.interpolated <- function(
                                table,
                                countries,
                                year.lo,
                                year.hi
                            )
{
print( 'inter' )                                                #--remove--
    year   <-  year.lo + 2      # --remove--
print( year )                           #--remove--

    num    <-  year - year.lo
    den    <-  year.hi - year.lo
print( num )                                    # --remove--
print( den )                                    # --remove--

    lower  <-  table                                                                %>%
               select( CNTR_NAME, YEAR, VAL, LO, HI )                               %>%
               filter( YEAR == year.lo )                                            %>%
               transmute(
                   CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                   VAL       = VAL,
                   LO        = LO,
                   HI        = HI
               )                                                                    %>%
               inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )
print( 'lower' )                                                                                  #--remove--
print( lower )                                                                                  #--remove--

    higher  <- table                                                                %>%
               select( CNTR_NAME, YEAR, VAL, LO, HI )                               %>%
               filter( YEAR == year.hi )                                            %>%
               transmute(
                   CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                   VAL       = VAL,
                   LO        = LO,
                   HI        = HI
               )                                                                    %>%
               inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )
print( 'higher' )                                                                                  #--remove--
print( higher )                                                                                  #--remove--


    inner_join( lower, higher, by = c( "CNTR_ID" = "CNTR_ID" ), suffix = c( ".lo", ".hi" ) )    %>%
    transmute(
        CNTR_ID    =  CNTR_ID,
        CNTR_NAME  =  CNTR_NAME.lo,
        VAL        =  VAL.lo + ( VAL.hi - VAL.lo ) * num / den,
        LO         =  LO.lo  + ( LO.hi  - LO.lo )  * num / den,
        HI         =  HI.lo  + ( HI.hi  - HI.lo )  * num / den
    )
}
