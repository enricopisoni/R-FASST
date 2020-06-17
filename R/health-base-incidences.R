# -*- mode: R -*-

library( 'ncdf4' )


#' Notes on this file usage.
#'
#' Functions \code{get.base.incidences()} and \code{get.base.incidences.by.ages()}
#' return the countries base incidence.
#' Function attenpts to retrieve countries base incidence from file;
#' if file doesn't exist, given input table, functions compute the base
#' incidence and store it in file.
#'
#' Base incidences returned by \code{get.base.incidences()}
#' can be used to create a raster via \code{raster.base.incidences()};
#' the later function will store in file the raster computed, the same
#' file will be used to load the raster instead to compute it.
#'
#' Base incidences returned by \code{get.base.incidences.by.ages()}
#' can be used to create a rasters stack via
#' \code{raster.base.incidences.by.ages()};
#' also this function will store the rasters stack in a file
#' to be used later, if it exists, to avoid the computation.
#'
#' The fucntions: \code{index.by.agr_grp.type()} and
#' \code{index.by.agr_id.type()}, can be used to get raster layer,
#' from the raster stack returned by \code{raster.base.incidences.by.ages()},
#' given the age class and the value type.
#' These two functions can be used to get layer from any ratsers stack,
#' the layers must be stacked in the following order;
#'       age class 1, type 1
#'       age class 1, type 2
#'       age class 1, type 3
#'       age class 2, type 1
#'       ...
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
        print( sprintf( "Base incidence per country (table) read from: '%s'.", file ) )

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
        print( sprintf( "Base incidence per country (table) wrote to: '%s'.", file ) )

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
#' @return table with columns:
#'           - CNTR_ID = country identifier;
#'           - AGE_ID  = age class identifier (values: 25, 30, 35, ...);
#'           - VAL     = value type: median;
#'           - LO      = value type: lower;
#'           - HI      = value type: upper;
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
        print( sprintf( "Base incidence per country and age group (table) read from: '%s'.", file ) )
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
        print( sprintf( "Base incidence per country and age group (table) wrote to: '%s'.", file ) )

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

        grid  <-  brick( file )

        print( sprintf( "Grid with base incidence by country read from file: '%s'.", file ) )

    } else {
        # --- compute the layers matrices ---
        print( 'Computing grid with base incidence by country - begin' );
        ptm <- proc.time()

        # replace country id for Sud Sudan (736 --> 729)
        sudan          <- table                                 %>%
                          filter( CNTR_ID == 736 )              %>%
                          transmute(
                                CNTR_ID = 729,
                                VAL     = VAL,
                                LO      = LO,
                                HI      = HI
                          )

        # use Morocco country id (504) for West Sahara (732)
        west.sahara    <- table                                 %>%
                          filter( CNTR_ID == 504 )              %>%
                          transmute(
                                CNTR_ID = 732,
                                VAL     = VAL,
                                LO      = LO,
                                HI      = HI
                          )

        # remove unwanted country id and append the above tables
        table          <- table                                 %>%
                          filter( CNTR_ID != 736 )              %>%
                          bind_rows( sudan )                    %>%
                          bind_rows( west.sahara )

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
            cntr.id               <-  table[ icntr, ] $ CNTR_ID
            country               <-  base.map[]  ==  cntr.id

            layer.val[ country ]  <-  table[ icntr, ] $ VAL
            layer.lo[  country ]  <-  table[ icntr, ] $ LO
            layer.hi[  country ]  <-  table[ icntr, ] $ HI
        }

        # stack the three layers
        grid           <-  stack( layer.val, layer.lo, layer.hi )

        # --- store the matrices ---
        writeRaster(
                grid,
                filename  = file,
                format    = "CDF",
                overwrite = TRUE
        )

        # --- elapsed time to build up the grid ---
        elapsed <-  proc.time() - ptm
        print( sprintf( 'Computing grid with base incidence by country - end (elapsed time: %s; file: %s)', elapsed[ 'elapsed' ], file ) )
    }

    # --- set layers name ---
    names( grid )  <-  c( 'VAL', 'LO', 'HI' )


    # --- return the stacked layers ---
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
#'                        where: AGE_ID is the age class identifier,
#'                        with values: 25, 30, 35, ...
#'
#' @return rasters stack, where each layer index is defined by:
#'         3 * ( AGE_ID - 25 ) / 5 + <value type>
#'         and <value type> defines: 1 = VAL, 2 = LO, 3 = HI;
#'

raster.base.incidences.by.ages <- function(
                                        file,
                                        base.map,
                                        ages_grp.size,
                                        table
                                  )
{
    file <- paste( file, 'nc', sep ='.' )

    if ( file.exists( file ) )
    {
        # --- load the vector ---

        grid  <-  brick( file )

        print( sprintf( "Grid with base incidence by country and age group read from file: '%s'.", file ) )

    } else {
        print( 'Computing grid with base incidence by country and age group - begin' );
        ptm <- proc.time()

        # replace country id for Sud Sudan (736 --> 729)
        sudan          <- table                                 %>%
                          filter( CNTR_ID == 736 )              %>%
                          transmute(
                                CNTR_ID = 729,
                                AGE_ID  = AGE_ID,
                                VAL     = VAL,
                                LO      = LO,
                                HI      = HI
                          )

        # use Morocco country id (504) for West Sahara (732)
        west.sahara    <- table                                 %>%
                          filter( CNTR_ID == 504 )              %>%
                          transmute(
                                CNTR_ID = 732,
                                AGE_ID  = AGE_ID,
                                VAL     = VAL,
                                LO      = LO,
                                HI      = HI
                          )

        # append tables and sort by age group identifier
        table          <- table                                 %>%
                          filter( CNTR_ID != 736 )              %>%
                          bind_rows( sudan )                    %>%
                          bind_rows( west.sahara )              %>%
                          arrange( AGE_ID )


        # --- compute the vector ---

        grid             <-  brick()

        last.cntr.id     <-  -1
        last.age.idx     <-  -1
        sub.grid.filled  <-  FALSE
        for( icntr in 1:nrow( table ) )
        {
            # --- which country and age group ---
            cntr.id  <-  table[ icntr, ] $ CNTR_ID
            if ( cntr.id != last.cntr.id )
            {
                country       <-  base.map[]  ==  cntr.id
                last.cntr.id  <-  cntr.id
            }

            age.idx  <-  table[ icntr, ] $ AGE_ID
            if ( age.idx != last.age.idx )
            {
                if ( sub.grid.filled )
                {
                    grid  <-  addLayer( grid, grid.val )
                    grid  <-  addLayer( grid, grid.lo  )
                    grid  <-  addLayer( grid, grid.hi  )
                }
                grid.val  <-  raster(
                                  ncol = ncol( base.map ),
                                  nrow = nrow( base.map ),
                                  xmn  = xmin( base.map ),
                                  xmx  = xmax( base.map ),
                                  ymn  = ymin( base.map ),
                                  ymx  = ymax( base.map )
                              )
                values( grid.val ) <- 0

                grid.lo   <-  raster(
                                  ncol = ncol( base.map ),
                                  nrow = nrow( base.map ),
                                  xmn  = xmin( base.map ),
                                  xmx  = xmax( base.map ),
                                  ymn  = ymin( base.map ),
                                  ymx  = ymax( base.map )
                              )
                values( grid.lo ) <- 0

                grid.hi   <-  raster(
                                  ncol = ncol( base.map ),
                                  nrow = nrow( base.map ),
                                  xmn  = xmin( base.map ),
                                  xmx  = xmax( base.map ),
                                  ymn  = ymin( base.map ),
                                  ymx  = ymax( base.map )
                              )
                values( grid.hi ) <- 0


                last.age.idx     <-  age.idx
                sub.grid.filled  <-  TRUE
            }

            # --- fill the grid ---
            grid.val[ country ]    <-  table[ icntr, ] $ VAL
            grid.lo [ country ]    <-  table[ icntr, ] $ LO
            grid.hi [ country ]    <-  table[ icntr, ] $ HI

        }
        if ( sub.grid.filled )
        {
            grid  <-  addLayer( grid, grid.val )
            grid  <-  addLayer( grid, grid.lo  )
            grid  <-  addLayer( grid, grid.hi  )
        }

        # --- store the vector ---
        writeRaster(
                grid,
                filename  = file,
                format    = "CDF",
                zname     = "layer index by age class identifier and value type",
                zunit     = "layer_index",
                overwrite = TRUE
        )
        set.global.attributes( file )

        # --- elapsed time to build up the grid ---
        elapsed <-  proc.time() - ptm
        print( sprintf( 'Computing grid with base incidence by country and age group - end (elapsed time: %s; file: %s)', elapsed[ 'elapsed' ], file ) )
    }

    # --- set layers name ---
    types.seq <-  c( 'VAL', 'LO', 'HI' )
    age.last  <-  nlayers( grid ) / 3
    age.seq   <-  seq( from = 25, to = 25 + 5 * ( age.last - 1 ), by = 5 )
    names     <-  c()
    for( age.class in age.seq )
        for( type in types.seq )
        {
            names <- c( names, sprintf( 'Age.Class.%d_Type.%s', age.class, type ) )
        }
    names( grid )  <-  names


    # --- return the stacked layers ---
    return( grid )
}
set.global.attributes <- function( file )
{
    # open the netCDF
    nc  <-  nc_open( file, write = T )

    # add global attributes
    ncatt_put(
        nc,
        0,
        "description",
        "Given an age group class as: 25, 30, 25, .. and a value type: median: 1, lower: 2, upper: 3; to get the layer needed apply the function: 3 *( <age group class> - 25 ) / 5 + <value type>, to get the index layer."
    )

    # close the file, writing data to disk
    nc_close( nc )
}

# ------------------------------------------------------------

#' Given an age class and a value type,
#' this function returns the index to get the layer from
#' the stack returned by function
#' \code{raster.base.incidences.by.ages()}.
#'
#' @param age_grp age class,
#'                belongig to set: 25, 30, 35, ...
#' @param type    value type,
#'                belonging the set:
#'                    1 = median
#'                    2 = lower
#'                    3 = upper
#'
#' @return the layer index;
#'
index.by.agr_grp.type <- function(
                             age_grp,
                             type
                         )
{
    3 * age_grp / 5  + type  - 15
}

# ------------------------------------------------------------

#' Given an age class identifier and a value type,
#' this function returns the index to get the layer from
#' the stack returned by function
#' \code{raster.base.incidences.by.ages()}.
#'
#' @param age_id  age class identifier,
#'                belongig to set: 1, 2, 3, ...
#' @param type    value type,
#'                belonging the set:
#'                    1 = median
#'                    2 = lower
#'                    3 = upper
#'
#' @return the layer index;
#'
index.by.agr_id.type <- function(
                             age_id,
                             type
                         )
{
    3 * ( age_id - 1 )  +  type
}
