# -*- mode: R -*-


#' Compute error propagation at grid cell level from uncertainty on AF
#' and mrate;
#' sig_dmort / mort = sqrt( ( sig_AF / AF )^2 + ( sig_mrate / mrate )^2 )
#'
#' @param dmort  total mortalities
#' @param af     attributable fraction, as list with fields:
#'               \describe{
#'                    \item{grid}    {raster brick with central, low and high values;}
#'                    \item{sig_min} {standard deviation;}
#'                    \item{sig_max} {standard deviation;}
#'               }
#' @param mrate  three layers rasters stack, the layers define values:
#'               VAL, LO, HI
#'
#' @return named list with fields:
#'               \describe{
#'                    \item{sig_min} {standard deviation on lower bound;}
#'                    \item{sig_max} {standard deviation on higher bound;}
#'               }
#'
get.af.mrate.error.propagation <- function(
                                      dmort,
                                      af,
                                      mrate
                                  )
{
    sig_min  <-  dmort  *
                 sqrt(
                        ( af $ sig_min )^2  +
                        ( ( mrate[[ 1 ]] - mrate[[ 2 ]] ) / mrate[[ 1 ]] )^2
                 )
    sig_max  <-  dmort  *
                 sqrt(
                        ( af $ sig_max )^2  +
                        ( ( mrate[[ 3 ]] - mrate[[ 1 ]] ) / mrate[[ 1 ]] )^2
                 )

    sig_min[ ! is.finite( sig_min ) ]  <- 0
    sig_max[ ! is.finite( sig_max ) ]  <- 0

    list(
        sig_min  =  sig_min,
        sig_max  =  sig_max
    )
}

# ------------------------------------------------------------

#' Compute error propagation at grid cell level from uncertainty on AF
#' and mrate as a sum of age classes;
#' sig_dmort / mort = sqrt( ( sig_AF / AF )^2 + ( sig_mrate / mrate )^2 )
#'
#' @param dmort  total mortalities, as layers stack, one layer per age class;
#' @param af     attributable fraction, as layers stack, each three layers
#'               is an age class split in: median, lower and higher values;
#' @param mrate  mortality rate, as layers stack, each three layers
#'               is an age class split in: median, lower and higher values;
#'
#' @return named list with fields:
#'               \describe{
#'                    \item{sig_min} {standard deviation on lower bound;}
#'                    \item{sig_max} {standard deviation on higher bound;}
#'               }
#'
get.af.mrate.error.propagation.by.age <- function(
                                             dmort,
                                             af,
                                             mrate
                                         )
{
    # --- error for each age class ---
    idx      <-  index.by.agr_id.type( 1, 1 )
    idx.min  <-  index.by.agr_id.type( 1, 2 )
    idx.max  <-  index.by.agr_id.type( 1, 3 )

    sig_min  <-  error.propagation(
                     dmort[[ 1 ]],
                     af[[ idx ]],    af[[ idx.min ]],
                     mrate[[ idx ]], mrate[[ idx.min ]]
                 ) ^ 2
    sig_max  <-  error.propagation(
                     dmort[[ 1 ]],
                     af[[ idx ]],    af[[ idx.max ]],
                     mrate[[ idx ]], mrate[[ idx.max ]]
                 ) ^ 2

    for ( icl in c( 2:nlayers( dmort ) ) )
    {
        idx      <-  index.by.agr_id.type( icl, 1 )
        idx.min  <-  index.by.agr_id.type( icl, 2 )
        idx.max  <-  index.by.agr_id.type( icl, 3 )

        sig_min  <-  sig_min                                    +
                     error.propagation(
                         dmort[[ icl ]],
                         af[[ idx ]],    af[[ idx.min ]],
                         mrate[[ idx ]], mrate[[ idx.min ]]
                     ) ^ 2
        sig_max  <-  sig_max                                    +
                     error.propagation(
                         dmort[[ icl ]],
                         af[[ idx ]],    af[[ idx.max ]],
                         mrate[[ idx ]], mrate[[ idx.max ]]
                     ) ^ 2
    }

    # --- error on sum of age classes (for each grid cell) ---
    sig_min                            <-  sqrt( sig_min )
    sig_min[ ! is.finite( sig_min ) ]  <- 0

    sig_max                            <-  sqrt( sig_max )
    sig_max[ ! is.finite( sig_max ) ]  <- 0


    list(
        sig_min  =  sig_min,
        sig_max  =  sig_max
    )
}
error.propagation <- function(
                         dmort,
                         af.1,
                         af.2,
                         mrate.1,
                         mrate.2
                     )
{
    dmort                                         *
    sqrt(
        ( ( af.1 - af.2 ) / af.1 ) ^ 2
        +
        ( ( mrate.1 - mrate.2 ) / mrate.1 ) ^ 2
    )
}
