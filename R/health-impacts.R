# -*- mode: R -*-

library( 'raster' )
library( 'tidyverse' )

source( 'health-base-incidences.R' )
source( 'health-age-country.R' )
source( 'health-error-propagation.R' )
source( 'rrate.R' )
source( 'fasst-write.R' )


#' Health impacts from high resolution FASST grid maps;
#'
#' Function constraints:
#' \itemize{
#'      \item output directory defined by formal parameter \code{dir.root.out} must exist;
#' }
#'
#' @param project       project name;
#' @param model         model name;
#' @param version       current version;
#' @param dir.root.out  output directory root (absolute path or
#'                      relative to working directory);
#' @param config        named list with paths configuration;
#'                      the list should define fields listed
#'                      in file: fasst-config.R;
#'

health.impact <- function(
                   project,
                   model,
                   version,
                   dir.root.out,
                   config
                 )
{
    # check input parameters
    if ( ! dir.exists( dir.root.out ) )
    {
        stop( sprintf( "output directory '%s' does not exist (current working directory: '%s').", dir.root.out, getwd() ) )
    }

    # set working directory
    dir.home <- getwd()
    setwd( dir.root.out )
    print( sprintf( "Working directory is now: '%s'", getwd() ) )

    # internal configuration
    dir.output  <- file.path( '.', project )
    dir.tables  <- file.path( dir.output, 'tables' )
    dir.netcdf  <- file.path( dir.output, 'ncdf' )


    # ----------------------------------------------------------------------
    # ------------------------------- block 2 ------------------------------
    # load population, base mortality data and risk rate function parameters
    # ----------------------------------------------------------------------

    # note: available population totals from SSP: 2000, 2010, 2020, 2030, ..., 2100
    # available base mortalities and fraction of pop <5yr and >30yr: 2005 2010 2015 2030 2050.
    # If other years are needed, an additional interpolation is carried out on the latter.
    # For years > 2040 mortality stats for 2040 are used.
    # For years < 2005, mortality stats for 2005 are used.

    # population country totals
    pop <- read_csv( config $ files $ in.file.pop.country,
                     skip = 1,
                     col_types = config $ tmpls $ pop.type,
                     col_names = config $ tmpls $ pop.name
                     )

    # generate dataframe with unique country codes and names from POP file
    cntr <- unique(
                subset(
                        x = pop,
                        select = c( CNTR_ID, CNTR_NAME, CNTR_ISO3 )
                      )
                  )

    # read country identification gridmap (Ciesin GPW v4)
    cntrgrid <- raster( config $ files $ in.file.cntrgrid )                 # here we don't flip rows!

    # increase both the area and the resolution
    hrcntrcode <- disaggregate(
                        merge(
                                raster( ncol = 1440, nrow = 720, xmn = -180, xmx = 180, ymn = -90, ymx = 90 ),
                                cntrgrid
                                ),
                        fact = 2
                        )
    hrcntrcode[ ! is.finite( hrcntrcode ) ]  <-  0

    # files with base mortality rates (per 100k population)
    copd       <- read_csv( config $ files $ in.file.copd,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    lc         <- read_csv( config $ files $ in.file.lc,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    lri        <- read_csv( config $ files $ in.file.lri,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    ihd        <- read_csv( config $ files $ in.file.ihd,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    stroke     <- read_csv( config $ files $ in.file.stroke,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )
    dmt2       <- read_csv( config $ files $ in.file.dmt2,
                            skip = 1,
                            col_types = config $ tmpls $ mort.type,
                            col_names = config $ tmpls $ mort.name
                            )

    # read the fitting parameters for the Burnett IER functions for all CODs and age classes
    rr         <- read_csv( config $ files $ in.file.rr,
                            skip = 1,
                            col_types = config $ tmpls $ rr.type,
                            col_names = config $ tmpls $ rr.name
                            )

    copd_med   <- rr $ RRMED[ ( rr $ COD == 'COPD' ) & ( rr $ AGE == 99 ) ]
    copd_lo    <- rr $ RRLO[  ( rr $ COD == 'COPD' ) & ( rr $ AGE == 99 ) ]
    copd_hi    <- rr $ RRHI[  ( rr $ COD == 'COPD' ) & ( rr $ AGE == 99 ) ]

    lri_med    <- rr $ RRMED[ ( rr $ COD == 'LRI' ) & ( rr $ AGE == 99 ) ]
    lri_lo     <- rr $ RRLO[  ( rr $ COD == 'LRI' ) & ( rr $ AGE == 99 ) ]
    lri_hi     <- rr $ RRHI[  ( rr $ COD == 'LRI' ) & ( rr $ AGE == 99 ) ]

    lc_med     <- rr $ RRMED[ ( rr $ COD == 'LC' ) & ( rr $ AGE == 99 ) ]
    lc_lo      <- rr $ RRLO[  ( rr $ COD == 'LC' ) & ( rr $ AGE == 99 ) ]
    lc_hi      <- rr $ RRHI[  ( rr $ COD == 'LC' ) & ( rr $ AGE == 99 ) ]

    dt2_med    <- rr $ RRMED[ ( rr $ COD == 'DT2' ) & ( rr $ AGE == 99 ) ]
    dt2_lo     <- rr $ RRLO[  ( rr $ COD == 'DT2' ) & ( rr $ AGE == 99 ) ]
    dt2_hi     <- rr $ RRHI[  ( rr $ COD == 'DT2' ) & ( rr $ AGE == 99 ) ]

    # extract the appropriate RR parameters for each COD and assign to each variable - for easier tracking.
    ihd_med    <- matrix( nrow = 4, data = rr $ RRMED[ rr $ COD == 'IHD' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    ihd_lo     <- matrix( nrow = 4, data = rr $ RRLO[  rr $ COD == 'IHD' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    ihd_hi     <- matrix( nrow = 4, data = rr $ RRHI[  rr $ COD == 'IHD' & rr $ AGE %in% config $ model $ AGE_GRP ] )

    stroke_med <- matrix( nrow = 4, data = rr $ RRMED[ rr $ COD == 'STROKE' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    stroke_lo  <- matrix( nrow = 4, data = rr $ RRLO[  rr $ COD == 'STROKE' & rr $ AGE %in% config $ model $ AGE_GRP ] )
    stroke_hi  <- matrix( nrow = 4, data = rr $ RRHI[  rr $ COD == 'STROKE' & rr $ AGE %in% config $ model $ AGE_GRP ] )


    # Ozone RRs with log-lin ER function
    beta_copd_tu  <- log( c( 1.14, 1.08, 1.21 ) ) / 10     # new TURNER!, for new exposure metric annual mean of daily 8h max!
    beta_copd_gbd <- log( c( 1.06, 1.02, 1.10 ) ) / 10     # new GBD2017!, for new exposure metric 6-month mean of daily 8h max!


    # *** file 'country_mask_0.5x0.5_v3.sav' is loaded but NOT used;

    # high resolution lon lat dimensions
    img          <- ncol( hrcntrcode )
    jmg          <- nrow( hrcntrcode )
    scale        <- img / ( xmax( hrcntrcode ) - xmin( hrcntrcode ) )
# --not-used--    hr_lats <- ( ( ( 0:( ( ymax( hrcntrcode ) - ymin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + ymin( hrcntrcode ) )
# --not-used--    hr_lons <- ( ( ( 0:( ( xmax( hrcntrcode ) - xmin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + xmin( hrcntrcode ) )

    # med resolution lon lat dimensions
    imed         <- 720   # img / 4 ?
    jmed         <- 360   # jmg / 4 ?
    scale        <- img / ( xmax( hrcntrcode ) - xmin( hrcntrcode ) )
    mr_lats      <- ( ( ( 0:( ( ymax( hrcntrcode ) - ymin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + ymin( hrcntrcode ) )
    mr_lons      <- ( ( ( 0:( ( xmax( hrcntrcode ) - xmin( hrcntrcode ) ) * scale - 1 ) ) + 0.5 ) / scale + xmin( hrcntrcode ) )

    # lon-lat arrays
# --not-used-remove--    hr_grid_tot  <- array( 0, c( jmg, img ) )
# --not-used-remove--    hr_grid_tot1 <- array( 0, c( jmg, img ) )    # intermediate placeholders needed when interpolating
# --not-used-remove--    hr_grid_tot2 <- array( 0, c( jmg, img ) )    # intermediate placeholders needed when interpolating


    # ----------------------------------------------------------------------
    # ------------------------------- block 3 ------------------------------
    # ---- START LOOP WITH SCENARIO ANALYSIS  - EACH LOOP = 1 SCENARIO -----
    # ----------------------------------------------------------------------

    for ( scen  in  config $ file $ scenarios $ name )
        for ( year  in  config $ file $ scenarios $ year )
        {
            print( sprintf( "Scenario name: '%s' - Year: %d", scen, year ) )

            # restore HIGH RESOLUTION (HIRES) population map(s) and interpolate if needed
            population.map <- get.population.map(
                                        scen,
                                        year,
                                        config $ model $ ssp_yrs,
                                        config $ file  $ in.tmpl.pop.map
                              )

            hr_grid_tot    <- merge(
                                        raster( ncol = img, nrow = jmg, xmn = -180, xmx = 180, ymn = -90, ymx = 90 ),
                                        population.map
                              )
            hr_grid_tot[ is.na( hr_grid_tot[] ) ] <- 0

            # identify grids with valid population data
            hr_grid_mask                 <- ( hr_grid_tot > 0 ) & ( hr_grid_tot <  population.map @ file @ nodatavalue )

            scenpop                      <-  raster(
                                                 ncol = ncol( hr_grid_tot ),
                                                 nrow = nrow( hr_grid_tot ),
                                                 xmn  = xmin( hr_grid_tot ),
                                                 xmx  = xmax( hr_grid_tot ),
                                                 ymn  = ymin( hr_grid_tot ),
                                                 ymx  = ymax( hr_grid_tot )
                                             )
            values( scenpop )            <-  0
            scenpop[ hr_grid_mask ]      <-  hr_grid_tot[ hr_grid_mask ]

            scenpopmask                  <-  raster(
                                                 ncol = ncol( hr_grid_tot ),
                                                 nrow = nrow( hr_grid_tot ),
                                                 xmn  = xmin( hr_grid_tot ),
                                                 xmx  = xmax( hr_grid_tot ),
                                                 ymn  = ymin( hr_grid_tot ),
                                                 ymx  = ymax( hr_grid_tot )
                                             )
            values( scenpopmask )        <-  0
            scenpopmask[ hr_grid_mask ]  <-  1

            # read the scenario input file (high resolution grid map)
            infile <- get.file.name.population(
                                config $ file $ in.tmpl.scenario,
                                scen,
                                year
                      )
            print( sprintf( "Processing file '%s'", normalizePath( infile, winslash = '/' ) ) )

            # calculate attributable fractions AF = 1-1/RR for central values, low and high confidence interval bound
            print( sprintf( "Calculate AFs @ %s", format( Sys.time(), "%c" ) ) )

            sc_hires      <- raster( infile, varname = 'TOT_PM_35' )  # extract total pm from SC structure and load into SC_HIRES variable
            sc_anth_hires <- raster( infile, varname = 'ANT_PM_35' )  # extract anthropogenic pm from SC structure and load into SC_HIRES variable
            sc_adm8h      <- raster( infile, varname = 'ADM8h' )
            sc_sdm8h      <- raster( infile, varname = 'SDM8h' )

            af_copd  <- compute.attributable.functions(             # ALL AGES >25
                             sc_hires,
                             copd_med,
                             copd_lo,
                             copd_hi
                        )

            af_lc    <- compute.attributable.functions(             # ALL AGES >25
                             sc_hires,
                             lc_med,
                             lc_lo,
                             lc_hi
                        )

            af_lri   <- compute.attributable.functions(            # ALL AGES
                             sc_hires,
                             lri_med,
                             lri_lo,
                             lri_hi
                        )

            af_dmt2  <- compute.attributable.functions(           # ALL AGES >25
                           sc_hires,
                           dt2_med,
                           dt2_lo,
                           dt2_hi
                        )


            # in the IDL source, these variables are defined and updated but never used:
            #  - SIG_MIN_AF_IHD
            #  - SIG_MAX_AF_IHD
            #  - SIG_MIN_AF_STROKE
            #  - SIG_MAX_AF_STROKE
            # moreover, variable SIG_MAX_AF_STROKE is updated in the original loop
            # but in the last assignement onutside the loop it is updated
            # using variable: SIG_MIN_AF_STROKE (line: 346);

            # layers containing the AFs, depends on PM fields
            print( sprintf( "Loop on AFs (%d age classes) - begin", length( config $ model $ AGE_GRP ) ) )
            ptm <- proc.time()
            af_ihd_grid     <-  brick()
            af_stroke_grid  <-  brick()
            for ( iage  in  seq_along( config $ model $ AGE_GRP ) )
            {
                print( sprintf( 'Age %d: %d', iage, config $ model $ AGE_GRP[ iage ] ) )

                # --- IHD block ---
                af_ihd_grid     <-  addLayer( af_ihd_grid,    1 - 1 / rrate( ihd_med[ , iage ], sc_hires ) )
                af_ihd_grid     <-  addLayer( af_ihd_grid,    1 - 1 / rrate( ihd_lo [ , iage ], sc_hires ) )
                af_ihd_grid     <-  addLayer( af_ihd_grid,    1 - 1 / rrate( ihd_hi [ , iage ], sc_hires ) )

                # --- STROKE block ---
                af_stroke_grid  <-  addLayer( af_stroke_grid, 1 - 1 / rrate( stroke_med[ , iage ], sc_hires ) )
                af_stroke_grid  <-  addLayer( af_stroke_grid, 1 - 1 / rrate( stroke_lo [ , iage ], sc_hires ) )
                af_stroke_grid  <-  addLayer( af_stroke_grid, 1 - 1 / rrate( stroke_hi [ , iage ], sc_hires ) )
            }
            print(
                sprintf(
                    "Loop on AFs (%d age classes) - end (elapsed time: %s)",
                    length( config $ model $ AGE_GRP ),
                    ( proc.time() - ptm )[ 'elapsed' ]
                )
            )

            # ---------------------------------------------------------------------------------
            # ------------------------------------ block 3a -----------------------------------
            # ------------------------- BASE INCIDENCES (MED,LO,UP) ---------------------------
            # ---------------------------------------------------------------------------------

            cntr.sliced        <- slice.countries.list( cntr )

            mrate_copd.table   <- get.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.copd,   scen, year ),
                                              year,
                                              cntr.sliced,
                                              copd
                                  )

            mrate_lc.table     <- get.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.lc,     scen, year ),
                                              year,
                                              cntr.sliced,
                                              lc
                                  )

            mrate_lri.table    <- get.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.lri,    scen, year ),
                                              year,
                                              cntr.sliced,
                                              lri
                                  )

            mrate_dmt2.table   <- get.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.dmt2,   scen, year ),
                                              year,
                                              cntr.sliced,
                                              dmt2
                                  )

            mrate_ihd.table    <- get.base.incidences.by.ages(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.ihd,    scen, year ),
                                              year,
                                              cntr.sliced,
                                              length( config $ model $ AGE_GRP ),
                                              ihd
                                  )

            mrate_stroke.table <- get.base.incidences.by.ages(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.stroke, scen, year ),
                                              year,
                                              cntr.sliced,
                                              length( config $ model $ AGE_GRP ),
                                              stroke
                                  )

            # --- create raster images ---
            mrate_copd         <- raster.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.copd,   scen, year ),
                                              hrcntrcode,
                                              mrate_copd.table
                                  )

            mrate_lc           <- raster.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.lc,     scen, year ),
                                              hrcntrcode,
                                              mrate_lc.table
                                  )

            mrate_lri          <- raster.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.lri,    scen, year ),
                                              hrcntrcode,
                                              mrate_lri.table
                                  )

            mrate_dmt2         <- raster.base.incidences(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.dmt2,   scen, year ),
                                              hrcntrcode,
                                              mrate_dmt2.table
                                  )

            mrate_ihd          <- raster.base.incidences.by.ages(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.ihd,    scen, year ),
                                              hrcntrcode,
                                              length( config $ model $ AGE_GRP ),
                                              mrate_ihd.table
                                  )

            mrate_stroke       <- raster.base.incidences.by.ages(
                                              get.file.name.population( config $ file $ in.tmpl.mrate.stroke, scen, year ),
                                              hrcntrcode,
                                              length( config $ model $ AGE_GRP ),
                                              mrate_stroke.table
                                  )


            # ---------------------------------------------------------------------------------
            # ------------------------------------ block 3b -----------------------------------
            # ------------- RETRIEVE AGE STRUCTURE PER COUNTRY FROM UN2017 REVISION -----------
            # ---------------------------------------------------------------------------------

            pop.age.tbl  <- get.age.structure(
                                get.file.name.population( config $ file $ in.tmpl.pop_age_fr, scen, year ),
                                year,
                                cntr.sliced,
                                pop
                            )

            pop_age_fr   <- raster.age.structure(
                                get.file.name.population( config $ file $ in.tmpl.pop_age_fr, scen, year ),
                                hrcntrcode,
                                pop.age.tbl
                            )

            # CALCULATE APPROPRIATE AGE FRACTIONS
            frac_copd    <- sum.raster.age.structure(
                                pop_age_fr,
                                config $ model $ C1,
                                config $ model $ C2
                            )

            frac_lc      <- sum.raster.age.structure(
                                pop_age_fr,
                                config $ model $ L1,
                                config $ model $ L2
                            )

            frac_lri     <- sum.raster.age.structure(
                                pop_age_fr,
                                config $ model $ LR1,
                                config $ model $ LR2
                            )

            frac_dmt2    <- sum.raster.age.structure(
                                pop_age_fr,
                                config $ model $ DM1,
                                config $ model $ DM2
                            )

            frac_o3      <- sum.raster.age.structure(
                                pop_age_fr,
                                config $ model $ O1,
                                config $ model $ O2
                            )

            # NUMBER OF SPECIFIC 5YR AGE CLASSES FOR IHD AND STROKE
            ncl_ihd      <- config $ model $ IH2  -  config $ model $ IH1  + 1   # SAME FOR STROKE

            # At this point we have global grid maps of AF, POP and MORTALITY RATES,
            # both for total as for age classes
            # LRI: <5YR, COPD AND LC:>30 YR IHD AND STROKE: AGE CLASSES FOR 25+
            print( sprintf( "End of age fractions. CALCULATE DMORT @ %s", format( Sys.time(), "%c" ) ) )


            # ---------------------------------------------------------------------------------
            # ------------------------------------ block 4 ------------------------------------
            # ------------------ CALCULATE DMORT = AF*MRATE*POP*AGEFRAC/100K ------------------
            # ---------------------------------------------------------------------------------

            # --- calculate central values of total mortalities ---

            # GBD2016: from class 15 to 75-79  GBD2017: all age
            dmort_copd  <-  ( af_copd $ grid )[[ 1 ]]  *  mrate_copd[[ 1 ]]  *  frac_copd  *  scenpop  /  1.e5

            # GBD2016: from class 15-19 to 75-79  GBD2017: all age
            dmort_lc    <-  ( af_lc $ grid )[[ 1 ]]    *  mrate_lc[[ 1 ]]    *  frac_lc    *  scenpop  /  1.e5

            # GBD2016: from class 0-4 to 75-79  GBD2017: all age
            dmort_lri   <-  ( af_lri $ grid )[[ 1 ]]   *  mrate_lri[[ 1 ]]   *  frac_lri   *  scenpop  /  1.E5

            # GBD2016: from class 15-19 to 75-79  GBD2017: all age
            dmort_dmt2  <-  ( af_dmt2 $ grid )[[ 1 ]]  *  mrate_dmt2[[ 1 ]]  *  frac_dmt2  *  scenpop  /  1.e5


            # GBD2016: ONLY 10 CLASSES; GBD2017:15 CLASSES
            dmort_ihd    <- brick()           # stack of only median values - 15 layers
            dmort_stroke <- brick()           # stack of only median values - 15 layers
            for ( icl in c( 1:ncl_ihd ) )
            {
                idx           <-  index.by.agr_id.type( 1, icl )

                # GBD2016: from class 25-29 to 75-79  GBD2017: all > 25
                dmort_ihd     <-  addLayer(
                                      dmort_ihd,
                                      af_ihd_grid[[ idx ]]      *
                                      mrate_ihd[[   idx ]]      *
                                      scenpop                   *
                                      pop_age_fr[[ icl + 5 ]]   /
                                      1.e5
                                  )

                # GBD2016: from class 25-29 to 75-79  GBD2017: all > 25
                dmort_stroke  <-  addLayer(
                                      dmort_stroke,
                                      af_stroke_grid[[ idx ]]   *
                                      mrate_stroke[[   idx ]]   *
                                      scenpop                   *
                                      pop_age_fr[[ icl + 5 ]]   /
                                      1.e5
                                  )
            }

            # error propagation at grid cell level from uncertainty on AF and mrate:
            # sig_dmort / mort = sqrt( ( sig_AF / AF )^2 + ( sig_mrate / mrate )^2 )
            sig_copd    <-  get.af.mrate.error.propagation(
                                dmort_copd,
                                af_copd,
                                mrate_copd
                            )

            sig_lc      <-  get.af.mrate.error.propagation(
                                dmort_lc,
                                af_lc,
                                mrate_lc
                            )

            sig_lri     <-  get.af.mrate.error.propagation(
                                dmort_lri,
                                af_lri,
                                mrate_lri
                            )

            sig_dmt2    <-  get.af.mrate.error.propagation(
                                dmort_dmt2,
                                af_dmt2,
                                mrate_dmt2
                            )

            sig_ihd     <-  get.af.mrate.error.propagation.by.age(
                                dmort_ihd,
                                af_ihd_grid,
                                mrate_ihd
                            )

            sig_stroke  <-  get.af.mrate.error.propagation.by.age(
                                dmort_stroke,
                                af_stroke_grid,
                                mrate_stroke
                            )

            sig_all     <-  list(
                                sig_min  =  sig_copd   $ sig_min  +
                                            sig_lc     $ sig_min  +
                                            sig_lri    $ sig_min  +
                                            sig_dmt2   $ sig_min  +
                                            sig_ihd    $ sig_min  +
                                            sig_stroke $ sig_min,
                                sig_max  =  sig_copd   $ sig_max  +
                                            sig_lc     $ sig_max  +
                                            sig_lri    $ sig_max  +
                                            sig_dmt2   $ sig_max  +
                                            sig_ihd    $ sig_max  +
                                            sig_stroke $ sig_max
                            )

            # --- sum mortality values ---
            dmort_ihd_all     <-  calc( dmort_ihd,    sum )
            dmort_stroke_all  <-  calc( dmort_stroke, sum )

            all_mort          <-  dmort_lc            +
                                  dmort_lri           +
                                  dmort_copd          +
                                  dmort_dmt2          +
                                  dmort_ihd_all       +
                                  dmort_stroke_all

            # CONSTRUCT LAYERS WITH LOWER AND UPPER BOUNDARIES
            dmort_copd        <-  brick(
                                      dmort_copd,
                                      dmort_copd - sig_copd $ sig_min,
                                      dmort_copd + sig_copd $ sig_max
                                  )
            dmort_lc          <-  brick(
                                      dmort_lc,
                                      dmort_lc   - sig_lc   $ sig_min,
                                      dmort_lc   + sig_lc   $ sig_max
                                  )
            dmort_lri         <-  brick(
                                      dmort_lri,
                                      dmort_lri  - sig_lri  $ sig_min,
                                      dmort_lri  + sig_lri  $ sig_max
                                  )
            dmort_dmt2        <-  brick(
                                      dmort_dmt2,
                                      dmort_dmt2 - sig_dmt2 $ sig_min,
                                      dmort_dmt2 + sig_dmt2 $ sig_max
                                  )

            dmort_ihd_all     <-  brick(
                                      dmort_ihd_all,
                                      dmort_ihd_all     -  sig_ihd    $ sig_min,
                                      dmort_ihd_all     +  sig_ihd    $ sig_max
                                  )
            dmort_stroke_all  <-  brick(
                                      dmort_stroke_all,
                                      dmort_stroke_all  -  sig_stroke $ sig_min,
                                      dmort_stroke_all  +  sig_stroke $ sig_max
                                  )


            print( 'TOTAL MORTALITIES AMBIENT PM PER COD:' )
            print( fasst.print.row.mortalities( 'COPD',    dmort_copd ) )
            print( fasst.print.row.mortalities( 'LC',      dmort_lc ) )
            print( fasst.print.row.mortalities( 'LRI',     dmort_lri ) )
            print( fasst.print.row.mortalities( 'DMT2',    dmort_dmt2 ) )
            print( fasst.print.row.mortalities( 'IHD',     dmort_ihd_all ) )
            print( fasst.print.row.mortalities( 'STROKE',  dmort_stroke_all ) )


            adm8h_adm8thr_threshold  <-  ( ( sc_adm8h - config $ model $ ADM8THR ) > 0 )

            dmort_o3_tu     <- mrate_copd[[ 1 ]]  *
                               frac_o3            *
                               scenpop            *
                               scenpopmask        *
                               (
                                   1 - exp( -beta_copd_tu[ 1 ] * adm8h_adm8thr_threshold )
                               )                  /
                               1.e5

            dmort_o3_gbd    <- mrate_copd[[ 1 ]]  *
                               frac_o3            *
                               scenpop            *
                               scenpopmask        *
                               (
                                   1 - exp( -beta_copd_gbd[ 1 ] * ( ( sc_sdm8h - config $ model $ SDM8THR ) > 0 ) )
                               )                  /
                               1.e5


            sig_copd_min    <-  ( mrate_copd[[ 1 ]] - mrate_copd[[ 2 ]] ) / mrate_copd[[ 1 ]]
            sig_copd_max    <-  ( mrate_copd[[ 3 ]] - mrate_copd[[ 1 ]] ) / mrate_copd[[ 1 ]]

            sig_af_tu_min   <-  (
                                  ( 1 - exp( -beta_copd_tu[ 1 ] * adm8h_adm8thr_threshold ) ) -
                                  ( 1 - exp( -beta_copd_tu[ 2 ] * adm8h_adm8thr_threshold ) )
                                )                                                               /
                                (
                                  1 - exp( -beta_copd_tu[ 1 ] * adm8h_adm8thr_threshold )
                                )
            sig_af_tu_max   <-  (
                                  ( 1 - exp( -beta_copd_tu[ 3 ] * adm8h_adm8thr_threshold ) ) -
                                  ( 1 - exp( -beta_copd_tu[ 1 ] * adm8h_adm8thr_threshold ) )
                                )                                                               /
                                (
                                  1 - exp( -beta_copd_tu[ 1 ] * adm8h_adm8thr_threshold )
                                )

            sig_af_gbd_min  <-  (
                                  ( 1 - exp( -beta_copd_gbd[ 1 ] * adm8h_adm8thr_threshold ) ) -
                                  ( 1 - exp( -beta_copd_gbd[ 2 ] * adm8h_adm8thr_threshold ) )
                                )                                                                /
                                (
                                  1 - exp( -beta_copd_gbd[ 1 ] * adm8h_adm8thr_threshold )
                                )
            sig_af_gbd_max  <-  (
                                  ( 1 - exp( -beta_copd_gbd[ 3 ] * adm8h_adm8thr_threshold ) ) -
                                  ( 1 - exp( -beta_copd_gbd[ 1 ] * adm8h_adm8thr_threshold ) )
                                )                                                                /
                                (
                                  1 - exp( -beta_copd_gbd[ 1 ] * adm8h_adm8thr_threshold )
                                )


            sig_o3_tu_min   <-  dmort_o3_tu * sqrt( sig_copd_min ^ 2  +  sig_af_tu_min ^ 2 )
            sig_o3_tu_min[ ! is.finite( sig_o3_tu_min ) ]  <- 0

            sig_o3_tu_max   <-  dmort_o3_tu * sqrt( sig_copd_max ^ 2  +  sig_af_tu_max ^ 2 )
            sig_o3_tu_max[ ! is.finite( sig_o3_tu_max ) ]  <- 0

            sig_o3_gbd_min  <-  dmort_o3_gbd * sqrt( sig_copd_min ^ 2  +  sig_af_gbd_min ^ 2 )
            sig_o3_gbd_min[ ! is.finite( sig_o3_gbd_min ) ]  <- 0

            sig_o3_gbd_max  <-  dmort_o3_gbd * sqrt( sig_copd_max ^ 2  +  sig_af_gbd_max ^ 2 )
            sig_o3_gbd_max[ ! is.finite( sig_o3_gbd_max ) ]  <- 0


            dmort_o3_gbd    <-  brick( dmort_o3_gbd )
            dmort_o3_gbd    <-  addLayer( dmort_o3_gbd, dmort_o3_gbd[[ 1 ]]  - sig_o3_gbd_min )
            dmort_o3_gbd    <-  addLayer( dmort_o3_gbd, dmort_o3_gbd[[ 1 ]]  + sig_o3_gbd_max )

            dmort_o3_tu     <-  brick( dmort_o3_tu )
            dmort_o3_tu     <-  addLayer( dmort_o3_tu, dmort_o3_tu[[ 1 ]]  - sig_o3_tu_min )
            dmort_o3_tu     <-  addLayer( dmort_o3_tu, dmort_o3_tu[[ 1 ]]  + sig_o3_tu_max )


            print( 'TOTAL MORTALITIES O3:' )
            print( fasst.print.row.mortalities( 'COPD GBD', dmort_o3_gbd ) )
            print( fasst.print.row.mortalities( 'COPD TUR', dmort_o3_tu ) )


            # ---------------------------------------------------------------------------------
            # ------------------------------------ block 5 ------------------------------------
            # ----------------- Pass to 0.5x0.5deg resolution for aggregation -----------------
            # ---------------------------------------------------------------------------------

            # --- make sum of 4x4 subgrids to pass from 7.5'x7.5' (=0.125x0.125deg) ---
            # --- to 0.5x0.5deg resolution                                          ---

            # aggregate subgrid cells
            popmed                 <-  aggregate( scenpop, fact = 4, fun = sum )

            mres_dmort_copd        <-  resolution.reduce( dmort_copd,       4L, max )
            mres_dmort_lc          <-  resolution.reduce( dmort_lc,         4L, max )
            mres_dmort_lri         <-  resolution.reduce( dmort_lri,        4L, max )
            mres_dmort_dmt2        <-  resolution.reduce( dmort_dmt2,       4L, max )
            mres_dmort_ihd         <-  resolution.reduce( dmort_ihd_all,    4L, max )
            mres_dmort_stroke      <-  resolution.reduce( dmort_stroke_all, 4L, max )
            mres_dmort_o3_tu       <-  resolution.reduce( dmort_o3_tu,      4L, max )
            mres_dmort_o3_gbd      <-  resolution.reduce( dmort_o3_gbd,     4L, max )


            mort_sc                <-  brick()
            mort_sc                <-  addLayer(
                                           mort_sc,
                                           mres_dmort_copd   [[ 1 ]]  +
                                           mres_dmort_lc     [[ 1 ]]  +
                                           mres_dmort_lri    [[ 1 ]]  +
                                           mres_dmort_dmt2   [[ 1 ]]  +
                                           mres_dmort_ihd    [[ 1 ]]  +
                                           mres_dmort_stroke [[ 1 ]]
                                       )
            mort_sc                <-  addLayer(
                                           mort_sc,
                                           mres_dmort_copd   [[ 2 ]]  +
                                           mres_dmort_lc     [[ 2 ]]  +
                                           mres_dmort_lri    [[ 2 ]]  +
                                           mres_dmort_dmt2   [[ 2 ]]  +
                                           mres_dmort_ihd    [[ 2 ]]  +
                                           mres_dmort_stroke [[ 2 ]]
                                       )
            mort_sc                <-  addLayer(
                                           mort_sc,
                                           mres_dmort_copd   [[ 3 ]]  +
                                           mres_dmort_lc     [[ 3 ]]  +
                                           mres_dmort_lri    [[ 3 ]]  +
                                           mres_dmort_dmt2   [[ 3 ]]  +
                                           mres_dmort_ihd    [[ 3 ]]  +
                                           mres_dmort_stroke [[ 3 ]]
                                       )

            print( 'TOTAL 0.5x0.5 GRID MORTALITIES AMBIENT PM PER COD:' )
            print( fasst.print.row.mortalities( 'COPD',                               mres_dmort_copd   ) )
            print( fasst.print.row.mortalities( 'LC',                                 mres_dmort_lc     ) )
            print( fasst.print.row.mortalities( 'LRI',                                mres_dmort_lri    ) )
            print( fasst.print.row.mortalities( 'DMT2',                               mres_dmort_dmt2   ) )
            print( fasst.print.row.mortalities( 'IHD',                                mres_dmort_ihd    ) )
            print( fasst.print.row.mortalities( 'STROKE',                             mres_dmort_stroke ) )
            print( fasst.print.row.mortalities( 'TOTAL 0.5x0.5 GRID MORTALITIES O3',  mort_sc           ) )
            print( fasst.print.row.mortalities( 'COPD GBD',                           mres_dmort_o3_gbd ) )
            print( fasst.print.row.mortalities( 'COPD TUR',                           mres_dmort_o3_tu  ) )



        }  # end of: for ( year  in  config $ file $ scenarios $ year )


    # write the output
    fasst.write(
             dir.tables,
             list(
                   project.name  = project,
                   model.name    = model,
                   model.version = version,
                   sDM8THR       = config $ model $ SDM8THR,
                   ADM8THR       = config $ model $ ADM8THR,
                   AGEFRAC_COPD  = config $ model $ agefrac_copd,
                   AGEFRAC_LC    = config $ model $ agefrac_lc ,
                   AGEFRAC_LRI   = config $ model $ agefrac_lri,
                   AGEFRAC_IHD   = config $ model $ agefrac_ihd,
                   AGEFRAC_O3    = config $ model $ agefrac_o3
             )
          )




    # as last return back to home
    setwd( dir.home )
}

# ------------------------------------------------------------

#' The function gets from NetCDF file the population map(s);
#' if needed, formal parameter \code{interpolation} set at TRUE,
#' an interpolation is carried out to create the map;
#'
#' Constraints:
#'      NetCDF files must define the following variables:
#'      \description{
#'          \item{2000total}
#'                      {for files with values about year 2000;}
#'          \item{<scenario>_<year>}
#'                      {for all other files;}
#'      }
#'
#' @param scenario         the scenario to use;
#' @param year             the year we are working on;
#' @param ssp_yrs          the available years;
#' @param netcdf.template  path template to NetCDF files;
#'                         path can be either absolute or relative the
#'                         working directory;
#'                         path template can contain the placeholders:
#'                         \describe{
#'                              {scenario} {the scenario name;}
#'                              {year}     {the year;}
#'                         }
#'
get.population.map <- function(
                                scenario,
                                year,
                                ssp_yrs,
                                netcdf.template
                      )
{
        # CHECK IF SSP POPULATION YEAR IS AVAILABLE, IF NOT:INTERPOLATE BETWEEN AVAILABLE YEARS
        intpol <- FALSE
        npop   <- max( ssp_yrs[ ssp_yrs <= year ] )
        jpop   <- min( ssp_yrs[ ssp_yrs >= year ] )

        if ( npop != jpop )
        {
            intpol <- TRUE
            fyr    <- ( year - npop ) / ( jpop - npop )
        }
        else
        {
            # *** this branch is not present in the original IDL prg
            fyr    <- 1
        }

        if ( ! intpol )
        {
                if ( year == 2000 )
                {
                        varname <- '2000total'
                } else {
                        varname <- paste( str_to_lower( scenario ), year, sep = '_' )
                }
                totfil <- get.file.name.population( netcdf.template, scenario, year )

                r      <- raster( totfil, varname = varname )
# ---                grid <- array(
# ---                                getValues( r ),                 # here we don't flip by rows!
# ---                                c( nrow( r ), ncol( r ) )
# ---                        )

        } else {

                if ( year == 2000 )
                {
                        varname <- '2000total'
                } else {
                        varname <- paste( str_to_lower( scenario ), npop, sep = '_' )
                }
                totfil <- get.file.name.population( netcdf.template, scenario, npop )

                r_n    <- raster( totfil, varname = varname )
# ---                grid_n <- array(
# ---                                getValues( r_n ),                 # here we don't flip by rows!
# ---                                c( nrow( r_n ), ncol( r_n ) )
# ---                          )

                if ( year == 2000 )
                {
                        varname <- '2000total'
                } else {
                        varname <- paste( str_to_lower( scenario ), jpop, sep = '_' )
                }
                totfil <- get.file.name.population( netcdf.template, scenario, jpop )

                r_j    <- raster( totfil, varname = varname )
# ---                grid_j <- array(
# ---                                getValues( r_j ),                 # here we don't flip rows!
# ---                                c( nrow( r_j ), ncol( r_j ) )
# ---                          )

                # interpolate years
                r <- r_n + fyr * ( r_j - r_n )
# ---                grid <- grid_n + fyr * ( grid_j - grid_n )

                # don't lose the no-data value
                r @ file @ nodatavalue = r_n @ file @ nodatavalue
        }
        return ( r )
}
get.file.name.population <- function(
                                path.remplate,
                                scene,
                                year
                            )
{
        pattern <- c( '\\$\\{scenario\\}' = scene, '\\$\\{year\\}' = year )

        str_replace_all( path.remplate, pattern )
}

# ------------------------------------------------------------

#' The function computes the attributable functions for
#' central values, low and high confidence interval bound;
#'
#' @param sc  total pm from SC structure;
#' @param med
#' @param lo
#' @param hi
#'
#' @return named list with fields:
#'         \describe{
#'              \item{grid}    {raster brick with central, low and high values;}
#'              \item{sig_min} {standard deviation;}
#'              \item{sig_max} {standard deviation;}
#'         }
#'

compute.attributable.functions <- function(
                                        sc,
                                        med,
                                        lo,
                                        hi
                                  )
{
        grid <- brick(
                        1 - 1 / rrate( med, sc ),
                        1 - 1 / rrate( lo,  sc ),
                        1 - 1 / rrate( hi,  sc )
                )
        names( grid ) <- c( 'central', 'low', 'high' )

        list(
                grid    = grid,

                sig_min = grid[[1]] *
                          sqrt(
                                (
                                        ( rrate( med, sc ) - rrate( lo, sc ) )
                                        /
                                        rrate( med, sc )
                                ) ^2
                          ),

                sig_max = grid[[1]] *
                          sqrt(
                                (
                                        ( rrate( med, sc ) - rrate( hi, sc ) )
                                        /
                                        rrate( med, sc)
                                ) ^2
                          )
        )
}

# ------------------------------------------------------------

#' Reduces the resolution of each raster in stack by
#' given factor using a given function;
#'
#' @param stack   the layers to reduce resolution;
#' @param factor  the reducing factor;
#'                aggregation factor expressed as number of cells in each
#'                direction (horizontally and vertically). Or two integers
#'                (horizontal and vertial aggregation factor). Default is 2.
#' @param how     function used to aggregate values (default = mean);
#'
#' @return stack with the same layers in \code{stack}, each layer
#'         reduced by factor \code{factor} using the function \code{how}.
#'

resolution.reduce  <- function(
                          stack,
                          factor,
                          how
                      )
{
    reduced  <- brick()
    for( ilayer in 1:nlayers( stack ) )
    {
        reduced  <- addLayer(
                        reduced,
                        aggregate(
                            stack[[ ilayer ]],
                            fact = factor,
                            fun  = how
                        )
                    )
    }
    names( reduced )  <-  names( stack )

    reduced
}
