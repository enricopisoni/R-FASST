# -*- mode: R -*-


#' Retrieve age structure per country is returned;
#' if input file is already present, it is loaded,
#' otherwise age structure per country is computed and
#' stored for future retrieval;
#'
#' @param file            the input/output file with age structure per country;
#'                        actual parameter must not have the file extansion,
#'                        the function will append the file extension accordingly
#'                        the file type handled;
#' @param year            the current year;
#' @param countries.list  the list of all countries;
#'                        as returned from function \code{slice.countries.list()};
#' @param table           population country totals table;
#'
#' @return table with: population fraction per age class for all classes 0 to 100
#'         (21 classes);
#' 

get.age.structure <- function(
                        file,
                        year,
                        countries.list,
                        table
                     )
{
    file <- paste( file, 'csv', sep ='.' )

    if ( file.exists( file ) )
    {
        # --- input file exists, read it ---
        table.bycntr   <- read_csv(
                            file,
                            col_types = cols(
                                           CNTR_ID  = col_integer(),
                                           AGE_GRP  = col_integer(),
                                           POP_FRAC = col_double()
                                        )
                          )
        print( sprintf( "Age structure per country (table) read from: '%s'.", file ) )
        
    } else {

        # --- input file does not exist, compute it ---

        # read the values or interpolate if the requested year does not exist
        table.bycntr   <- join.filter.age.structure(
                                    table,
                                    countries.list,
                                    year
                          )

        # --- input file does not exist, write it ---
        dir.name  <-  dirname( file )
        dir.create( dir.name, recursive = TRUE, showWarnings = FALSE )
        write_csv(
            table.bycntr,
            file
        )
        print( sprintf( "Age structure per country (table) wrote to: '%s'.", file ) )

    }

    # --- the tables and rasters collection ---
    return( table.bycntr )
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
#'          the age group and the population fraction;
#'

join.filter.age.structure <- function(
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
        year.lo      <-  max( table $ YEAR[ table $ YEAR <= year ] )
        year.hi      <-  min( table $ YEAR[ table $ YEAR >= year ] )
        num          <-  year - year.lo
        den          <-  year.hi - year.lo

        lower        <-  table                                                                %>%
                         filter( YEAR == year.lo )                                            %>%
                         transmute(
                             CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en" ),
                             AGE_GRP   = AGE_GRP,
                             AGE_POP   = AGE_POP,
                         )                                                                    %>%
                         inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )         %>%
                         select( CNTR_ID, AGE_GRP, AGE_POP )

        pop.tot.lo   <-  lower                                                                %>%
                         group_by( CNTR_ID )                                                  %>%
                         summarize( POP_SUM = sum( AGE_POP ) )
                    
        higher       <-  table                                                                %>%
                         filter( YEAR == year.hi )                                            %>%
                         transmute(
                             CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en" ),
                             AGE_GRP   = AGE_GRP,
                             AGE_POP   = AGE_POP,
                         )                                                                    %>%
                         inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )         %>%
                         select( CNTR_ID, AGE_GRP, AGE_POP )

        pop.tot.hi   <-  higher                                                               %>%
                         group_by( CNTR_ID )                                                  %>%
                         summarize( POP_SUM = sum( AGE_POP ) )

        pop.tot      <-  inner_join(
                             pop.tot.lo,
                             pop.tot.hi,
                             by     = c( "CNTR_ID" = "CNTR_ID" ),
                             suffix = c( ".lo", ".hi" )
                         )                                                                    %>%
                         transmute(
                             CNTR_ID    =  CNTR_ID,
                             POP_SUM    =  POP_SUM.lo + ( POP_SUM.hi - POP_SUM.lo ) * num / den
                         )

        result       <-  inner_join(
                             lower,
                             higher,
                             by     = c( "CNTR_ID" = "CNTR_ID", "AGE_GRP" = "AGE_GRP" ),
                             suffix = c( ".lo", ".hi" )
                         )                                                                    %>%
                         transmute(
                             CNTR_ID    =  CNTR_ID,
                             AGE_GRP    =  AGE_GRP,
                             AGE_POP    =  AGE_POP.lo + ( AGE_POP.hi - AGE_POP.lo ) * num / den
                         )                                                                    %>%
                         inner_join(
                             pop.tot,
                             by     = c( "CNTR_ID" = "CNTR_ID" )
                         )                                                                    %>%
                         transmute(
                             CNTR_ID   = CNTR_ID,
                             AGE_GRP   = AGE_GRP,
                             POP_FRAC  = AGE_POP / POP_SUM
                         )

    } else {

        result       <-  table                                                                %>%
                         filter( YEAR == year )                                               %>%
                         transmute(
                             CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en" ),
                             AGE_GRP   = AGE_GRP,
                             AGE_POP   = AGE_POP,
                         )                                                                    %>%
                         inner_join( countries, by = c( "CNTR_NAME" = "CNTR_NAME" ) )         %>%
                         select( CNTR_ID, AGE_GRP, AGE_POP )

        pop.tot      <-  result                                                               %>%
                         group_by( CNTR_ID )                                                  %>%
                         summarize( POP_SUM = sum( AGE_POP ) )
                         
        result       <-  result                                                               %>%
                         inner_join( pop.tot, by = c( "CNTR_ID" = "CNTR_ID" ) )               %>%
                         transmute(
                             CNTR_ID   = CNTR_ID,
                             AGE_GRP   = AGE_GRP,
                             POP_FRAC  = AGE_POP / POP_SUM
                         )
                    
    }
    return( result )
}
