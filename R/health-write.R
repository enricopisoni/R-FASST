# -*- mode: R -*-

#' writes the output in text file in tabular format;
#'
#' @param dir.output    the output directory; it can be defined either with
#'                      a full path or a relative path from the working
#'                      directory; directory will be created if it does not
#'                      exist; file will be overwritten if it already exists;
#' @param parameters    named list with following entries:
#'                      \describe{
#'                         \item{proname}         {the programme generating this file}
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
#'                      }
#'
health.write.header <- function(
                           dir.output,
                           parameters
                       )
{
    # prepare output directories
    dir.create( dir.output, recursive = TRUE, showWarnings = FALSE )

    # prepare file name
    file.name <- health.write.file.name(
                     dir.output,
                     parameters
                 )

    # create output file
    file <- file( file.name, 'wt' )

    # write headers
    writeLines( 'NEW GBD2017 IER', file )
    writeLines( paste( 'Programme:',   parameters $ proname, sep = ' ' ),                       file )
    writeLines( paste( 'Date of run:', format( Sys.time(), "%c" ), sep = ' ' ),                 file )
    writeLines( sprintf( '%26s %5.2f', 'GBD 6MDMA8H threshold = ',   parameters $ sDM8THR ),    file )
    writeLines( sprintf( '%26s %5.2f', 'TURNER ADMA8H threshold = ', parameters $ ADM8THR ),    file )
    writeLines( sprintf( '%18s %18s',  'IER MODEL:',                 parameters $ model.name ), file )
    writeLines( 'AGE CLASSES:', file )
    writeLines( sprintf( '%18s: %18s %18s', 'COPD',          parameters $ AGEFRAC_COPD[1], parameters $ AGEFRAC_COPD[2] ), file )
    writeLines( sprintf( '%18s: %18s %18s', 'LC',            parameters $ AGEFRAC_LC  [1], parameters $ AGEFRAC_LC  [2] ), file )
    writeLines( sprintf( '%18s: %18s %18s', 'LRI',           parameters $ AGEFRAC_LRI [1], parameters $ AGEFRAC_LRI [2] ), file )
    writeLines( sprintf( '%18s: %18s %18s', 'IHD + STROKE',  parameters $ AGEFRAC_IHD [1], parameters $ AGEFRAC_IHD [2] ), file )
    writeLines( sprintf( '%18s: %18s %18s', 'COPD O3',       parameters $ AGEFRAC_O3  [1], parameters $ AGEFRAC_O3  [2] ), file )
    writeLines(
        sprintf(
            '%15s %15s %6s %5s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s',
            'SCENARIO',
            'SSP',
            'YEAR',
            'ISO',
            'COUNTRY',
            'POP_CNT',
            'POPW_PM2.5_TOT_35%',
            'POPW_NAT_PM25_35%',
            'POPW_ADM8h',
            'POPW_SDMA8h',
            'MORT_PM_6COD_M',
            'MORT_PM_6COD_L',
            'MORT_PM_6COD_H',
            'MORT_PM_COPD_M',
            'MORT_PM_LC_M',
            'MORT_PM_LRI_M',
            'MORT_PM_DMT2_M',
            'MORT_PM_IHD_M',
            'MORT_PM_STROKE_M',
            'MORT_PM_COPD_L',
            'MORT_PM_LC_L',
            'MORT_PM_LRI_L',
            'MORT_PM_DMT2_L',
            'MORT_PM_IHD_L',
            'MORT_PM_STROKE_L',
            'MORT_PM_COPD_H',
            'MORT_PM_LC_H',
            'MORT_PM_LRI_H',
            'MORT_PM_DMT2_H',
            'MORT_PM_IHD_H',
            'MORT_PM_STROKE_H',
            'MORT_O3_COPD_GBD_M',
            'MORT_O3_COPD_GBD_L',
            'MORT_O3_COPD_GBD_H',
            'MORT_O3_COPD_TUR_M',
            'MORT_O3_COPD_TUR_L',
            'MORT_O3_COPD_TUR_H'
        ),
        file
    )

    # close the file
    close( file )
}

# ------------------------------------------------------------

#' Write country information.
#'
#' @param dir.output    the output directory; it can be defined either with
#'                      a full path or a relative path from the working
#'                      directory; directory will be created if it does not
#'                      exist;
#' @param parameters    named list with following entries:
#'                      \describe{
#'                         \item{project.name}            {the project name}
#'                         \item{model.name}              {the model name}
#'                         \item{model.version}           {the model version}
#'                         \item{SCENLAB}                 {}
#'                         \item{SSP}                     {}
#'                         \item{YEAR}                    {}
#'                         \item{CNTR_ISO}                {}
#'                         \item{CNTR_NM}                 {}
#'                         \item{POPCN}                   {}
#'                         \item{POP_PMTOT_35}            {}
#'                         \item{POP_NAT_35}              {}
#'                         \item{POP_ADMA8h}              {}
#'                         \item{POP_SDMA8h}              {}
#'                         \item{CTOT_MORT_MED_SC}        {}
#'                         \item{CTOT_MORT_LO_SC}         {}
#'                         \item{CTOT_MORT_HI_SC}         {}
#'                         \item{CTOT_COPD_MED}           {}
#'                         \item{CTOT_LC_MED}             {}
#'                         \item{CTOT_LRI_MED}            {}
#'                         \item{CTOT_DMT2_MED}           {}
#'                         \item{CTOT_IHD_MED}            {}
#'                         \item{CTOT_STROKE_MED}         {}
#'                         \item{CTOT_COPD_LO}            {}
#'                         \item{CTOT_LC_LO}              {}
#'                         \item{CTOT_LRI_LO}             {}
#'                         \item{CTOT_DMT2_LO}            {}
#'                         \item{CTOT_IHD_LO}             {}
#'                         \item{CTOT_STROKE_LO}          {}
#'                         \item{CTOT_COPD_HI}            {}
#'                         \item{CTOT_LC_HI}              {}
#'                         \item{CTOT_LRI_HI}             {}
#'                         \item{CTOT_DMT2_HI}            {}
#'                         \item{CTOT_IHD_HI}             {}
#'                         \item{CTOT_STROKE_HI}          {}
#'                         \item{CTOT_O3MORT_MED_SC_GBD}  {}
#'                         \item{CTOT_O3MORT_LO_SC_GBD}   {}
#'                         \item{CTOT_O3MORT_HI_SC_GBD}   {}
#'                         \item{CTOT_O3MORT_MED_SC_TU}   {}
#'                         \item{CTOT_O3MORT_LO_SC_TU}    {}
#'                         \item{CTOT_O3MORT_HI_SC_TU}    {}
#'                      }
#'
health.write.country <- function(
                            dir.output,
                            parameters
                        )
{
    # prepare file name
    file.name <- health.write.file.name(
                     dir.output,
                     parameters
                 )

    # line output for current scenario, year, country
    write(
        sprintf(
            '%15s %15s %6s %5s %20s %20f %20.2f %20.2f %20.2f %20.2f %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e %20.4e',
            parameters $ SCENLAB,
            parameters $ SSP,
            parameters $ YEAR,
            parameters $ CNTR_ISO,
            parameters $ CNTR_NM,
            parameters $ POPCN,
            parameters $ POP_PMTOT_35,
            parameters $ POP_NAT_35,
            parameters $ POP_ADMA8h,
            parameters $ POP_SDMA8h,
            parameters $ CTOT_MORT_MED_SC,
            parameters $ CTOT_MORT_LO_SC,
            parameters $ CTOT_MORT_HI_SC,
            parameters $ CTOT_COPD_MED,
            parameters $ CTOT_LC_MED,
            parameters $ CTOT_LRI_MED,
            parameters $ CTOT_DMT2_MED,
            parameters $ CTOT_IHD_MED,
            parameters $ CTOT_STROKE_MED,
            parameters $ CTOT_COPD_LO,
            parameters $ CTOT_LC_LO,
            parameters $ CTOT_LRI_LO,
            parameters $ CTOT_DMT2_LO,
            parameters $ CTOT_IHD_LO,
            parameters $ CTOT_STROKE_LO,
            parameters $ CTOT_COPD_HI,
            parameters $ CTOT_LC_HI,
            parameters $ CTOT_LRI_HI,
            parameters $ CTOT_DMT2_HI,
            parameters $ CTOT_IHD_HI,
            parameters $ CTOT_STROKE_HI,
            parameters $ CTOT_O3MORT_MED_SC_GBD,
            parameters $ CTOT_O3MORT_LO_SC_GBD,
            parameters $ CTOT_O3MORT_HI_SC_GBD,
            parameters $ CTOT_O3MORT_MED_SC_TU,
            parameters $ CTOT_O3MORT_LO_SC_TU,
            parameters $ CTOT_O3MORT_HI_SC_TU
        ),
        file   = file.name,
        append = TRUE
    )

}

# ------------------------------------------------------------

#' Creates the full path name.
#'
#' @param dir.output    the output directory; it can be defined either with
#'                      a full path or a relative path from the working
#'                      directory;
#' @param parameters    named list with following entries:
#'                      \describe{
#'                         \item{project.name}    {the project name}
#'                         \item{model.name}      {the model name}
#'                         \item{model.version}   {the model version}
#'                      }
#'
health.write.file.name <- function(
                              dir.output,
                              parameters
                          )
{
    file.path(
               dir.output,
               paste(
                    paste( 'ALLCNTRIES', parameters$project.name, parameters$model.name, parameters$model.version, sep = '_' ),
                    'TXT',
                    sep = '.'
               )
           )
}

# ------------------------------------------------------------

#' Given a three layers stack, it prints one summary row
#' with three values, each one as sum of all cells of a layer;
#'
#' @param item   text string describing the values;
#' @param layers three layers stack to print;
#'
#' @return single line text with:
#'           description, sum of all cells for each layer;
#'
health.print.row.mortalities <- function(
                                    item,
                                    layers
                                )
{
    l.1 <-  layers[[ 1 ]]
    l.2 <-  layers[[ 2 ]]
    l.3 <-  layers[[ 3 ]]

    sprintf(
        "%15s: %10.2f (%10.2f, %10.2f)",
        item,
        sum( l.1[] ),
        sum( l.2[] ),
        sum( l.3[] )
    )
}
