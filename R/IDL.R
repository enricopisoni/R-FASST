# -*- mode: R -*-

#' Porting to R of some IDL functions.
#'


#' This is only a simple adaptation to this very project
#' of IDL function CONGRID.
#' In particular, this porting shrinks only raster layers and
#' it works only on 2D raster objects.
#'
#' @param arr   RasterLayer object to reduce in size;
#' @param fact  the aggregation factor;
#'
congrid.aggregate <- function( arr, fact )
{
    if ( nlayers( arr ) > 1 ) {
        stop( 'congrid.aggregate: present version of function works only on RasterLayer objects.' )
    }
    if ( fact <=1 ) {
        stop( 'congrid.aggregate: aggregation factor must be greater than 1.' )
    }


    # IDL function CONGRID uses, to shrink a grid like the ones this
    # project uses, the IDL function POLY_2D
    # (described in: https://www.harrisgeospatial.com/docs/POLY_2D.html);
    # for this project, we resolved the polynomial equation of first grade;
    # supposing we have an aggregtion factor for x axis (fact_x) and another
    # one for the y axis (fact_y), we got the equations:
    #   x' = fact_x * x
    #   y' = fact_y * y
    # but, the aggregate function, of package raster, gives the opportunity
    # to work on a subset of the whole grid: a grid of fact_x * fact_y cells
    # as vector (v_cells); hence, the above equations are simply reduced
    # to get only the first element of vector v_cells;
    # but, because to properly store the raster layer in a netCDF file we must
    # flip the raster lines, we cannot get the first item of vector but
    # the item at index: fact_x * ( fact_y - 1 )

    # at present we simplified the function call and hence we have
    # the same shrink factor for both the x axis and the y axis;
    fact_x    <-  fact
    fact_y    <-  fact

    idx       <-  fact_x * ( fact_y - 1 ) + 1

    congrid.shrink  <- function( cells, ... )
    {
        cells[ idx ]
    }


    # apply the aggregation to the layer;
    
    aggregate(
        arr,
        fact = fact,
        fun  = congrid.shrink
    )

}
