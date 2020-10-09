# -*- mode: R -*-

library( ncdf4 )


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

gridded.resolution.reduce  <- function(
                                  stack,
                                  factor,
                                  how
                              )
{
    if ( factor <= 1 )
    {
        reduced  <- stack
    } else {
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
    }
    reduced
}

# ------------------------------------------------------------

#' The function gets the raster with the smaller extension;
#'
#' @param r1 the first raster grid;
#' @param r2 the second raster grid;
#'
#' @return the raster with the smaller area extension;
#'
gridded.get.smallest.extention  <- function(
                                       r1,
                                       r2
                                   )
{
    ext.1  <-  extent( r1 )
    ext.2  <-  extent( r2 )

    if ( ext.1 > ext.2 ) r2 else r1
}

# ------------------------------------------------------------

#' The function resamples a ratser against a reference one;
#'
#' @param raster     the ratser to resample;
#' @param reference  the raster that \code{raster} should be resampled to;
#'
#' @return the raster resampled;
#'
gridded.resample  <-  function(
                          raster,
                          reference,
                          method     = "bilinear"
                      )
{
    resampled  <- raster
    res.ras    <-  res( raster )
    res.ref    <-  res( reference )
    if ( ! all( res.ras == res.ref ) )
    {
        ext.ras  <-  extent( raster )
        ext.ref  <-  extent( reference )
        if ( ! ( ext.ras == ext.ref ) )
        {
            resampled  <- resample( raster, reference, method )
        }
    }
    resampled
}

# ------------------------------------------------------------

#' stores raster in netCDF file;
#'
#' @param  file.name   output file name, without extension;
#' @param  parameters  named list with values to fill the output file,
#'                     fields are:
#'                      \describe{
#'                         \item{proname}           {the programme generating this file}
#'                         \item{scen}              {the scenario}
#'                         \item{year}              {the year}
#'                         \item{sdm8thr}           {}
#'                         \item{adm8thr}           {}
#'                         \item{londim}            {array with longitude values;}
#'                         \item{latdim}            {array with latitude values;}
#'                         \item{med_pmtot_35}      {}
#'                         \item{med_pmtot_ant_35}  {}
#'                         \item{mort_sc_med}       {}
#'                         \item{mres_dmort_o3_gbd} {}
#'                         \item{mres_dmort_o3_tu}  {}
#'                      }
#'
gridded.netcdf  <-  function(
                        file.name,
                        parameters
                    )
{
    dir.name   <-  dirname( file.name )
    dir.create( dir.name, recursive = TRUE, showWarnings = FALSE )

    name       <- paste( file.name, 'nc', sep = '.' )

    print( sprintf( "Creating: '%s'", normalizePath( name, winslash = '/', mustWork = FALSE ) ) )

    # define dimensions
    xid        <-  ncdim_def( 'lon', 'degrees_east',  as.double( parameters $ londim ) )
    yid        <-  ncdim_def( 'lat', 'degrees_north', as.double( parameters $ latdim ) )

    # define variables
    fillvalue  <-  9.96921E+36

    prtid1     <-  ncvar_def(
                       'PM',
                       'ug/m3',
                       list( xid, yid ),
                       fillvalue,
                       longname = 'scenario total PM2.5 AT 35%RH incl dust and SS',
                       prec     = 'float',
                       verbose  = FALSE
                   )
    prtid2     <-  ncvar_def(
                       'ANTH_PM',
                       'ug/m3',
                       list( xid, yid ),
                       fillvalue,
                       longname = 'scenario anthropogenic PM2.5 AT 35% RH',
                       prec     = 'float',
                       verbose  = FALSE
                   )
    mrtid1     <-  ncvar_def(
                       'GBD_PM_MORT',
                       '#',
                       list( xid, yid ),
                       fillvalue,
                       longname = 'Burnett Mortalities from total anthropogenic + natural PM2.5 AT 35% RH',
                       prec     = 'float',
                       verbose  = FALSE
                   )
    mrtid2     <-  ncvar_def(
                       'GBD_O3_MORT',
                       '#',
                       list( xid, yid ),
                       fillvalue,
                       longname = 'GBD2017 Mortalities from O3',
                       prec     = 'float',
                       verbose  = FALSE
                   )
    mrtid3     <-  ncvar_def(
                       'TUR_O3_MORT',
                       '#',
                       list( xid, yid ),
                       fillvalue,
                       longname = 'Turner/Malley Mortalities from O3',
                       prec     = 'float',
                       verbose  = FALSE
                   )

    # create netCDF file
    id         <-  nc_create(
                       name,
                       list( prtid1, prtid2, mrtid1, mrtid2, mrtid3 ),
                       force_v4 = TRUE
                   )

    # put variables
    ncvar_put( id, prtid1, values( flip( parameters $ med_pmtot_35,      direction='y' ) ) )
    ncvar_put( id, prtid2, values( flip( parameters $ med_pmtot_ant_35,  direction='y' ) ) )
    ncvar_put( id, mrtid1, values( flip( parameters $ mort_sc_med,       direction='y' ) ) )
    ncvar_put( id, mrtid2, values( flip( parameters $ mres_dmort_o3_gbd, direction='y' ) ) )
    ncvar_put( id, mrtid3, values( flip( parameters $ mres_dmort_o3_tu,  direction='y' ) ) )

    # describe variables
    ncatt_put( id, 'lon',  'standard_name',           'longtitude' )
    ncatt_put( id, 'lon',  'long_name',               'longtitude' )
    ncatt_put( id, 'lon',  'units',                   'degrees_east' )
    ncatt_put( id, 'lon',  'axis',                    'X' )
    ncatt_put( id, 'lon',  'bounds',                  'lon_bnds' )

    ncatt_put( id, 'lat',  'standard_name',           'latitude' )
    ncatt_put( id, 'lat',  'long_name',               'latitude' )
    ncatt_put( id, 'lat',  'units',                   'degrees_north' )
    ncatt_put( id, 'lat',  'axis',                    'Y' )
    ncatt_put( id, 'lat',  'bounds',                  'lat_bnds' )

    ncatt_put( id, prtid1, 'units',                   'ug/m3' )
    ncatt_put( id, prtid1, 'standard_name',           'PM' )
    ncatt_put( id, prtid1, 'long_name',               'scenario total PM2.5 AT 35%RH incl dust and SS' )

    ncatt_put( id, prtid2, 'units',                   'ug/m3' )
    ncatt_put( id, prtid2, 'standard_name',           'ANTH_PM' )
    ncatt_put( id, prtid2, 'long_name',               'scenario anthropogenic PM2.5 AT 35% RH' )

    ncatt_put( id, mrtid1, 'units',                   '#' )
    ncatt_put( id, mrtid1, 'standard_name',           'MORT_TOT_PM' )
    ncatt_put( id, mrtid1, 'long_name',               'Burnett Mortalities from total anthropogenic + natural PM2.5 AT 35% RH' )

    ncatt_put( id, mrtid2, 'units',                   '#' )
    ncatt_put( id, mrtid2, 'standard_name',           'MORT_O3' )
    ncatt_put( id, mrtid2, 'long_name',               'GBD2017 Mortalities from O3' )

    ncatt_put( id, mrtid3, 'units',                   '#' )
    ncatt_put( id, mrtid3, 'standard_name',           'MORT_O3' )
    ncatt_put( id, mrtid3, 'long_name',               'Turner/Malley Mortalities from O3' )

    # global attributes
    ncatt_put( id, 0,      'source',                  parameters $ proname )
    ncatt_put( id, 0,      'contact',                 'rita.van-dingenen@ec.europa.eu' )
    ncatt_put( id, 0,      'scenario',                parameters $ scen )
    ncatt_put( id, 0,      'year',                    sprintf( "%d", parameters $ year ) )
    ncatt_put( id, 0,      'GBD O3_threshold ppb',    sprintf( "%f", parameters $ sdm8thr ) )
    ncatt_put( id, 0,      'TURNER O3_threshold ppb', sprintf( "%f", parameters $ adm8thr ) )

    # close the file, writing data to disk
    nc_close( id )

    print( sprintf( "Written: '%s'", normalizePath( name, winslash = '/', mustWork = TRUE ) ) )
}
