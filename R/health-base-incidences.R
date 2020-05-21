# -*- mode: R -*-

library( 'ncdf4' )


#' Notes on this file usage.
#'
#' Functions \code{get.base.incidences()} and \code{get.base.incidences.by.ages()}
#' return the countries base incidence.
#' Function attenpt to retrievea countries base incidence from file;
#' if file doesn't exist, given input table, functions compute the base
#' incidence and store it in file.
#'
#' Base incidences returned by \code{get.base.incidences()}
#' can be used to create a raster via \code{raster.base.incidences()};
#' the later function will store in file the raster computed, the same
#' file will be used to load the raster instead to compute it.
#'
#' Base incidences returned by \code{get.base.incidences.by.ages()}
#' can be used to create a 4 dimensions vector via
#' \code{raster.base.incidences.by.ages()};
#' also this function will store the 4 dimensions vector in a file
#' to be used later, if it exists, to avoid the computation.
#'
#' Before the above function could be used, the countries table with
#' identifier and name must be prepared via function
#' \code{slice.countries.list()}.
#'

# ------------------------------------------------------------

#' To slice the countries table to columns with: country identifier and
#' country name, where the latter is replaced by its name in upper case;
#' This function prepares the countries table to be used with functions defined
#' in this file, as described in functions comments below;
#'
#' @param countries.list  the list of all countries;
#'
#' return the input list with only the columns: CNTR_ID and CNTR_NAME
#'        where the country name is uppercase;
#'

slice.countries.list <- function(
                            countries.list
                        )
{
    countries.list                                                       %>%
    select( CNTR_ID, CNTR_NAME )                                         %>%
    transmute(
         CNTR_ID   = CNTR_ID,
         CNTR_NAME = str_to_upper( CNTR_NAME, locale = "en")
    )
}

# ------------------------------------------------------------

#' Base incidences (MED, LO, UP) per country is returned;
#' if input file is already present, it is loaded,
#' otherwise base incidences per country are computed and
#' stored for future retrieval;
#'
#' @param file            the input/output file with countries information;
#'                        actual parameter must not have the file extansion,
#'                        the function will append the file extension accordingly
#'                        the file type handled;
#' @param year            the current year;
#' @param countries.list  the list of all countries;
#'                        as returned from function \code{slice.countries.list()};
#' @param table           table with informations per country;
#'
#' @return table with: country identifier and its values;
#'

get.base.incidences <- function(
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
                                           CNTR_ID = col_integer(),
                                           VAL     = col_double(),
                                           LO      = col_double(),
                                           HI      = col_double()
                                        )
                          )

    } else {

        # --- input file does not exist, compute it ---

        # read the values or interpolate if the requested year does not exist
        table.bycntr   <- join.filter(
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

    }

    # --- the tables and rasters collection ---
    return( table.bycntr )
}

# ------------------------------------------------------------

#' Base incidences (MED, LO, UP) per country is returned;
#' if input files are already present, they are loaded,
#' otherwise base incidences per country are computed and
#' stored for future retrieval;
#'
#' Calculation done for central, lower and upper boundary values,
#' and for each of 15 age classes.
#'
#' @param file            the input/output file with countries information;
#'                        actual parameter must not have the file extansion,
#'                        the function will append the file extension accordingly
#'                        the file type handled;
#' @param year            the current year;
#' @param countries.list  the list of all countries;
#'                        as returned from function \code{slice.countries.list()};
#' @param ages_grp.size   the expected size of ages group;
#' @param table           table with informations per country;
#'
#' @return table with: country identifier and its values;
#'

get.base.incidences.by.ages <- function(
                                  file,
                                  year,
                                  countries.list,
                                  ages_grp.size,
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
                                           CNTR_ID = col_integer(),
                                           AGE_ID  = col_integer(),
                                           VAL     = col_double(),
                                           LO      = col_double(),
                                           HI      = col_double()
                                        )
                          )
    } else {

        # --- input file does not exist, compute it ---

        # read the values or interpolate if the requested year does not exist
        table.bycntr   <- join.filter.ages(
                                    table,
                                    countries.list,
                                    year,
                                    ages_grp.size
                          )

        # --- input file does not exist, write it ---
        dir.name  <-  dirname( file )
        dir.create( dir.name, recursive = TRUE, showWarnings = FALSE )
        write_csv(
            table.bycntr,
            file
        )

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

# ------------------------------------------------------------

#' Projects the countries values to grid.
#' Given a grid map, where in each cell there is a country identifier,
#' and a table with, per each country three values, the fucntion
#' creates a three layer raster with the country values, one value
#' per layer.
#'
#' @param file     the input/output file with countries information;
#'                 actual parameter must not have the file extansion,
#'                 the function will append the file extension accordingly
#'                 the file type handled;
#' @param base.map grid with countries identifiers;
#' @param table    countries values;
#'                 as returned from function \code{get.base.incidences()}
#'                 with columns: CNTR_ID, VAL, LO, HI;
#'
#' @return three layer raster with country values;
#'

raster.base.incidences <- function(
                                file,
                                base.map,
                                table
                          )
{
    file <- paste( file, 'nc', sep ='.' )

    if ( file.exists( file ) )
    {
        # --- load the raster ---

        grid  <-  raster( file )

    } else {
        # --- compute the layers matrices ---

        # prepare the three layers
        layer.val <- raster(
                        ncol = ncol( base.map ),
                        nrow = nrow( base.map ),
                        xmn  = xmin( base.map ),
                        xmx  = xmax( base.map ),
                        ymn  = ymin( base.map ),
                        ymx  = ymax( base.map )
                 )
        values( layer.val ) <- 0

        layer.lo  <- raster(
                        ncol = ncol( base.map ),
                        nrow = nrow( base.map ),
                        xmn  = xmin( base.map ),
                        xmx  = xmax( base.map ),
                        ymn  = ymin( base.map ),
                        ymx  = ymax( base.map )
                 )
        values( layer.lo ) <- 0

        layer.hi  <- raster(
                        ncol = ncol( base.map ),
                        nrow = nrow( base.map ),
                        xmn  = xmin( base.map ),
                        xmx  = xmax( base.map ),
                        ymn  = ymin( base.map ),
                        ymx  = ymax( base.map )
                 )
        values( layer.hi ) <- 0

        # fill the three layer
        for( icntr in 1:nrow( table ) )
        {
            cntr.id  <-  table[ icntr, ] $ CNTR_ID
            if ( cntr.id == 736 )
            {
                 cntr.id <- 729
            }
            country               <-  base.map[]  ==  cntr.id

            layer.val[ country ]  <-  table[ icntr, ] $ VAL
            layer.lo[  country ]  <-  table[ icntr, ] $ LO
            layer.hi[  country ]  <-  table[ icntr, ] $ HI
        }

        # stack the three layers
        grid           <-  stack( layer.val, layer.lo, layer.hi )
        names( grid )  <-  c( 'VAL', 'LO', 'HI' )

        # --- store the matrices ---
        writeRaster(
                grid,
                filename  = file,
                format    = "CDF",
                overwrite = TRUE
        )

    }

    # return the stacked layers
    return( grid )
}

# ------------------------------------------------------------

#' Projects the countries values to 4 dimensions vector.
#' Given a grid map, where in each cell there is a country identifier,
#' and a table with, per each country three values per age group,
#' the fucntion a four dimensions vector: value type, age group and
#' the grid.
#'
#' @param file            the input/output file with countries information;
#'                        actual parameter must not have the file extansion,
#'                        the function will append the file extension
#'                        accordingly the file type handled;
#' @param base.map        grid with countries identifiers;
#' @param ages_grp.size   size of ages group;
#' @param table           countries values;
#'                        as returned from function
#'                        \code{get.base.incidences.by.ages()}
#'                        with columns: CNTR_ID, AGE_ID, VAL, LO, HI;
#'
#' @return four dimensions vector with indexes:
#'         value type (1 = VAL, 2 = LO, 3 = HI), age id, grid;
#'

raster.base.incidences.by.ages <- function(
                                        file,
                                        base.map,
                                        ages_grp.size,
                                        table
                          )
{
    file <- paste( file, 'rds', sep ='.' )

    if ( file.exists( file ) )
    {
        # --- load the vector ---

        grid  <-  readRDS( file )

    } else {
        ptm <- proc.time()
        print( 'Computing grid by country and age group - begin' );

        # --- compute the vector ---

        grid          <-  array( 0, c( 3, ages_grp.size, nrow( base.map ), ncol( base.map ) ) )

        last.cntr.id  <-  -1
        for( icntr in 1:nrow( table ) )
        {
            # --- which country and age group ---
            cntr.id  <-  table[ icntr, ] $ CNTR_ID
            if ( cntr.id != last.cntr.id )
            {
                if ( cntr.id == 736 )
                {
                     cntr.id <- 729
                }
                country       <-  base.map[]  ==  cntr.id
                last.cntr.id  <-  cntr.id
            }
            age.idx  <-  ageid.2.index( table[ icntr, ] $ AGE_ID )

            # --- fill the grid ---
            grid.val               <-  grid[ 1, age.idx, , ]
            grid.lo                <-  grid[ 2, age.idx, , ]
            grid.hi                <-  grid[ 3, age.idx, , ]

            grid.val[ country ]    <-  table[ icntr, ] $ VAL
            grid.lo [ country ]    <-  table[ icntr, ] $ LO
            grid.hi [ country ]    <-  table[ icntr, ] $ HI

            grid[ 1, age.idx, , ]  <-  grid.val
            grid[ 2, age.idx, , ]  <-  grid.lo
            grid[ 3, age.idx, , ]  <-  grid.hi
        }

        # elapsed time to build up the grid
        elapsed <-  proc.time() - ptm
        print( sprintf( 'Computing grid by country and age group - end (elapsed time: %s, system time: %s)', elapsed[ 'elapsed' ], elapsed[ 'system' ] );

        # --- store the vector ---
        saveRDS( grid, file )
    }

    # return the vector
    return( grid )
}

#' This small fucntion returns the age identifier index within a vector
#' of age identifiers given the age identifier.
#'
#' This function supposes the age identifiers vector is the one defined
#' in configurations as: AGE_GRP;
#' it shall be correct to define this function in confguration module.
#'
#' @param id  the age identifier;
#'
#' @return the age identifier index;
#'
ageid.2.index <- function( id )
{
        1 + ( id - 25 ) / 5
}
