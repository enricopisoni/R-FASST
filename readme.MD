# FASST #
This code is the porting in R of the FASST 'impact module' (Van Dingenen et al., 2018), and computes mortality due to O3 and PM air pollution implementing the 'Burnett Integrated Exposure-Response Functions' (Burnett et al., 2020, as in the Global Burden of Disease 2017). For O3, mortality is also computed using the Turner/Malley approach.  

The steps implemented in the code are as follows:

* input data are loaded (i.e. population gridded data from 2000 to 2100, base mortality rates per country, country masks, risk rate analytical function parameters, air pollution concentrations)
* Attributable Fractions (AF) are computed for each of the 6 considered CODs (...), as $AF = 1-\frac{1}{RR}$, using Relative Risks (RR) and PM concentration in grid cell
* Mortality is computed as $AF * BaseMortality * Population * AgeFraction$
* Mortalities are summed up (sum for the considered CODs) and saved as both grids and country totals.

The FASST 'impact module' has been here generalized during the porting in R, to be able to compute health impact not only from FASST output, but using output from any Chemical Transport Model. The example provided with the code i.e. works with the EMEP air quality model output.  

Finally, the code can produce output (in terms of gridded mortality) both aggregating results using a given 'reduction factor' (i.e. a 'reduction factor' of 5 could change the output spatial resolution from 0.1 to 0.5 degrees), or keeping the final results at 0.125 degrees (the highest available spatial resolution). 
In case the concentrations as input to the R-FASST comes from a different (than FASST) model, the code performs the following steps:

* Item 1 population are country ID (at 0..125 degrees resolution) are kept as reference
* Item 2 concentration grids (as from the Chemical Transport Model) are resampled to the population grid
* all the available rasters (population, concentrations,...) are cropped to the 'smaller' domain (i.e. the concentration one, in case it covers only Europe)
* if the reduction factor is different than 1, all the rasters are downscaled following the choosen reduction factor
* the final results are written


    



## Input data ##

## Output data ##
The output are structured in:
* 'ncdf' directory: containing a netcdf file, 

## How to run the code ##
Input files located in directory `Files/INPUT/` are not prsent in GitHub repository due their size.
To get these file, please ask the project owner.

## References ##
* Van Dingenen et al., 2018. TM5-FASST: a global atmospheric source–receptor  model for rapid impact analysis of emission changes  on air quality and short-lived climate pollutants, Atmos. Chem. Phys., 18, 16173–16211.
* Burnett et al., 2020. Relative Risk Functions for Estimating Excess Mortality Attributable to Outdoor PM2.5 Air Pollution: Evolution and State‐of‐the‐Ar, Atmosphere 2020, 11, 58.
