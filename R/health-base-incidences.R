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
#' @param ages_grp.size   the expected size of ages group;
#' @param ihd
#' @param stroke
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
                            dmt2,
                            ages_grp.size,
                            ihd,
                            stroke
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
                            dmt2,
                            ages_grp.size,
                            ihd,
                            stroke
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
#' @param ages_grp.size   the expected size of ages group;
#' @param ihd
#' @param stroke
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
                            dmt2,
                            ages_grp.size,
                            ihd,
                            stroke
                       )
{
    # lOOP THROUGH COUNTRIES; RETRIEVE BASE MORTALITY RATES AND MAP TO EACH OF
    # THE 3 GRID LAYERS (MED,LO,UP)

    # country names in upper case (used to match country names)
    countries   <- countries.list                                                       %>%
                   select( CNTR_ID, CNTR_NAME )                                         %>%
                   transmute(
                        CNTR_ID   = CNTR_ID,
                        CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en")
                   )

    # read the values or interpolate if the requested year does not exist
    copd.bycntr    <- join.filter( copd, countries, year )

    lc.bycntr      <- join.filter( lc,   countries, year )

    lri.bycntr     <- join.filter( lri,  countries, year )

    dmt2.bycntr    <- join.filter( dmt2, countries, year )

    ihd.bycntr     <- join.filter.ages( ihd,    countries, year, ages_grp.size )

    stroke.bycntr  <- join.filter.ages( stroke, countries, year, ages_grp.size )

}

# ------------------------------------------------------------

#' Retrieve values by country and year then add the United Nations
#' country identifier;
#' If the year defined by formal parameter \code{year} is not
#' available in the \code{table}, the requested values are
#' computed by interpolation;
#'
#' @param table      the data tibble;
#'                   constraint: country name in field \code{CNTR_NAME}
#'                   must be in upper case;
#' @param countries  the countries name and identifier;
#' @param year       the year to filter;
#'
#' @return  per each country, defined by its identifier code,
#'          the three values: median, lower and higher;
#'

join.filter <- function(
                   table,
                   countries,
                   year
               )
{
    # check whether the year we are working on exists
    theyear <- NULL
    maxyear <- max( table $ YEAR )
    minyear <- min( table $ YEAR )
    if ( year > maxyear )
    {
        theyear <- maxyear
    } else if ( year < minyear ) {
        theyear <- minyear
    } else if ( year %in%  table $ YEAR ) {
        theyear <- year
    }

    # read the values or interpolate if the requested year does not exist
    if ( is.null( theyear ) )
    {
        year.lo <-  max( table $ YEAR[ table $ YEAR <= year ] )
        year.hi <-  min( table $ YEAR[ table $ YEAR >= year ] )
        num     <-  year - year.lo
        den     <-  year.hi - year.lo

        lower   <-  table                                                               %>%
                    select( CNTR_NAME, YEAR, VAL, LO, HI )                              %>%
                    filter( YEAR == year.lo )                                           %>%
                    transmute(
                        CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                        VAL       = VAL,
                        LO        = LO,
                        HI        = HI
                    )                                                                   %>%
                    inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )        %>%
                    select( CNTR_ID, VAL, LO, HI )

        higher  <-  table                                                               %>%
                    select( CNTR_NAME, YEAR, VAL, LO, HI )                              %>%
                    filter( YEAR == year.hi )                                           %>%
                    transmute(
                        CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                        VAL       = VAL,
                        LO        = LO,
                        HI        = HI
                    )                                                                   %>%
                    inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )        %>%
                    select( CNTR_ID, VAL, LO, HI )

        result  <-  inner_join( lower, higher, by = c( "CNTR_ID" = "CNTR_ID" ), suffix = c( ".lo", ".hi" ) )    %>%
                    transmute(
                        CNTR_ID    =  CNTR_ID,
                        VAL        =  VAL.lo + ( VAL.hi - VAL.lo ) * num / den,
                        LO         =  LO.lo  + ( LO.hi  - LO.lo )  * num / den,
                        HI         =  HI.lo  + ( HI.hi  - HI.lo )  * num / den
                    )
    } else {
        result  <-  table                                                                %>%
                    select( CNTR_NAME, YEAR, VAL, LO, HI )                               %>%
                    filter( YEAR == year )                                               %>%
                    transmute(
                        CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                        VAL       = VAL,
                        LO        = LO,
                        HI        = HI
                    )                                                                    %>%
                    inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )         %>%
                    select( CNTR_ID, VAL, LO, HI )
    }
    return( result )
}

# ------------------------------------------------------------

#' Retrieve values by country and year and ages groups then add the United
#' Nations country identifier; only if values are defined for all ages groups;
#' If the year defined by formal parameter \code{year} is not
#' available in the \code{table}, the requested values are
#' computed by interpolation;
#'
#' @param table      the data tibble;
#'                   constraint: country name in field \code{CNTR_NAME}
#'                   must be in upper case;
#' @param countries  the countries name and identifier;
#' @param year       the year to filter;
#' @param grp.size   the ages groups size;
#'
#' @return  per each country, defined by its identifier code,
#'          the three values: median, lower and higher;
#'

join.filter.ages <- function(
                        table,
                        countries,
                        year,
                        grp.size
                    )
{
    # check whether the year we are working on exists
    theyear <- NULL
    maxyear <- max( table $ YEAR )
    minyear <- min( table $ YEAR )
    if ( year > maxyear )
    {
        theyear <- maxyear
    } else if ( year < minyear ) {
        theyear <- minyear
    } else if ( year %in%  table $ YEAR ) {
        theyear <- year
    }

    # read the values or interpolate if the requested year does not exist
    if ( is.null( theyear ) )
    {
        year.lo          <-  max( table $ YEAR[ table $ YEAR <= year ] )
        year.hi          <-  min( table $ YEAR[ table $ YEAR >= year ] )
        num              <-  year - year.lo
        den              <-  year.hi - year.lo

        not.enough.data  <- union(
                                list.countries.without.enough.values(
                                            table,
                                            year.lo,
                                            grp.size
                                ),
                                list.countries.without.enough.values(
                                            table,
                                            year.hi,
                                            grp.size
                                )
                            )

        lower            <-  table                                                               %>%
                             select( CNTR_NAME, YEAR, AGE_ID, VAL, LO, HI )                      %>%
                             filter( YEAR == year.lo )                                           %>%
                             anti_join( not.enough.data, by = c( "CNTR_NAME" = "CNTR_NAME" ) )   %>%
                             transmute(
                                 CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                                 AGE_ID    = AGE_ID,
                                 VAL       = VAL,
                                 LO        = LO,
                                 HI        = HI
                             )                                                                   %>%
                             inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )        %>%
                             select( CNTR_ID, AGE_ID, VAL, LO, HI )

        higher           <-  table                                                               %>%
                             select( CNTR_NAME, YEAR, AGE_ID, VAL, LO, HI )                      %>%
                             filter( YEAR == year.hi )                                           %>%
                             anti_join( not.enough.data, by = c( "CNTR_NAME" = "CNTR_NAME" ) )   %>%
                             transmute(
                                 CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                                 AGE_ID    = AGE_ID,
                                 VAL       = VAL,
                                 LO        = LO,
                                 HI        = HI
                             )                                                                   %>%
                             inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )        %>%
                             select( CNTR_ID, AGE_ID, VAL, LO, HI )

        result           <-  inner_join(
                                 lower,
                                 higher,
                                 by = c( "CNTR_ID" = "CNTR_ID", "AGE_ID" = "AGE_ID" ),
                                 suffix = c( ".lo", ".hi" )
                             )                                                                   %>%
                             transmute(
                                 CNTR_ID    =  CNTR_ID,
                                 AGE_ID     =  AGE_ID,
                                 VAL        =  VAL.lo + ( VAL.hi - VAL.lo ) * num / den,
                                 LO         =  LO.lo  + ( LO.hi  - LO.lo )  * num / den,
                                 HI         =  HI.lo  + ( HI.hi  - HI.lo )  * num / den
                             )

    } else {

        not.enough.data  <- list.countries.without.enough.values(
                                             table,
                                             year,
                                             grp.size
                            )

        result           <-  table                                                                %>%
                             select( CNTR_NAME, YEAR, AGE_ID, VAL, LO, HI )                       %>%
                             filter( YEAR == year )                                               %>%
                             anti_join( not.enough.data, by = c( "CNTR_NAME" = "CNTR_NAME" ) )    %>%
                             transmute(
                                 CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en"),
                                 AGE_ID    = AGE_ID,
                                 VAL       = VAL,
                                 LO        = LO,
                                 HI        = HI
                             )                                                                    %>%
                             inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )         %>%
                             select( CNTR_ID, AGE_ID, VAL, LO, HI )
    }
    return( result )
}

# ------------------------------------------------------------

#' List which countries do not have enough values for a given year;
#'
#' @param table      the data tibble;
#'                   constraint: country name in field \code{CNTR_NAME}
#'                   must be in upper case;
#' @param year       the year to filter;
#' @param grp.size   the ages groups size;
#'
#' @return  a set of \code{CNTR_NAME}, of given \code{year},
#'          belonging to \code{table} where values al less
#'          than \code{grp.size};
#'

list.countries.without.enough.values <- function(
                                            table,
                                            year,
                                            grp.size
                                        )
{
        table                           %>%
        filter( YEAR == year )          %>%
        select( CNTR_NAME, YEAR )       %>%
        group_by( CNTR_NAME )           %>%
        summarise( COUNT = n() )        %>%
        filter( COUNT < grp.size )      %>%
        select( CNTR_NAME )
}
