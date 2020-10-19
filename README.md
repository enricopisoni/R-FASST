# FASST #
This code is the porting in R of the FASST 'impact module' (Van Dingenen et al., 2018). The code computes mortality due to O3 and PM air pollution exposure. It implements  the 'Burnett Integrated Exposure-Response Functions' (Burnett et al., 2020) following the methodology also used in the Global Burden of Disease 2017. For O3, mortality is also computed using the Turner/Malley approach.

The steps implemented in the code for the Burnett approach are, broadly speaking, the following ones:

* Input data are loaded (i.e. gridded population from 2000 to 2100, base mortality rates per country, country masks, risk rate analytical function parameters, air pollution concentrations, etc...);
* Attributable Fractions (AF) are computed for each of the considered Cause of Death (COD), with the equation AF = 1-1/RR", using Relative Risks (RR) and PM concentration in each grid cell;
* The considered CODs for O3 are related to respiratory disease, while for PM are related to:
  + chronic obstructive pulmonary disease (COPD);
  + lung cancer (LC);
  + acute lower respiratory airway infections (ALRIs);
  + ischemic heart disease (IHD);
  + stroke;
  + type 2 Diabetes Mellitus (DMT2);
* Mortality is then computed as: "AF * BaseMortality * Population * AgeFraction";
* Mortalities are summed up (sum on the considered CODs) and saved as both grids and country totals.

The FASST 'impact module' has been here generalized during the porting in R, to be able to compute health impact not only from FASST output, but also from output from any other Chemical Transport Model. The example provided with the R-FASST code works, e.g., with the EMEP air quality model output.

Finally, the code has been generalized to  produce output (in terms of gridded mortality) both aggregating results using a given 'reduction factor', or keeping the final results at 0.125 degrees (the highest available spatial resolution). In the case of the FASST output, the default reduction factor is 4 (to move from a spatial resolution of 0.125 to a spatial resolution of 0.5). In case the concentrations as input to the R-FASST comes from a different (than FASST) model, the code performs the following steps:

* Population data and country ID (by default at 0.125 degrees resolution) are kept as reference;
* Concentration grids (as from the Chemical Transport Model) are resampled to the population reference grid;
* All the available rasters (population, concentrations,...) are cropped to the 'smaller' available domain (i.e., if the concentration raster covers only Europe, this will become the computation domain);
* If the reduction factor is different than 1, all the rasters are downscaled following the choosen reduction factor;
* The final results are written.

## Input data ##
Default input data are available for the computation, as follows:

* Gridded population, from 1990 to 2100;
* Population per country, per age group, from 1990 to 2100;
* Base mortality per country, per age group, per Cause of Death, from 1990 to 2040;
* Risk rate analytical function parameters;
* Country identifier raster;
* Gridded air pollution concentrations, as output of an air quality model.

## Output data ##
The output are structured in:

* a `ncdf` directory: containing a netcdf file with gridded mortalities, computed for O3 and PM;
* a `tables` directory: containing the mortality values aggregated per country.

## How to run the code ##
The code can be run in different ways: using a batch file (on Windows), a shell fle (on Linux) or directly in R. In the case of the last option, the routine `fasst-4-sherpa` has to be run, providing as input:

* The name of the project (used to create the output name files);
* The name of the model used to compute mortalities (used to create the output name files);
* The version of the model used to compute mortalities (used to create the output name files);
* The name of the output directory, in which the 'configuration file' is located;
* The name of the 'configuration file'.

Two 'configuration files' are provided as default:

* `config-fasst.json`: the default, to be used with FASST output;
* `config-emep.json`: i.e. to be used with EMEP output.

The two key input to be modified in the json 'configuration file' are:

* `reduction.factor`: to define if the output file spatial resolusion has to be 0.125 degrees (`reduction.factor` = 1) or if the output needs to be aggregated (for FASST, the default `reduction.factor` = 4);
* `in.tmpl.scenario`: to define the name of the output of the air quality model (to be used as input to evaluate exposure and health impacts), and the variable to be used for the impacts evaluation.

Input files located in directory `Files/INPUT/` are not present in GitHub repository due their size.
To get these file, please ask the project owner.

## References ##
* Van Dingenen et al., 2018. TM5-FASST: a global atmospheric source–receptor  model for rapid impact analysis of emission changes  on air quality and short-lived climate pollutants, Atmos. Chem. Phys., 18, 16173–16211.
* Burnett et al., 2020. Relative Risk Functions for Estimating Excess Mortality Attributable to Outdoor PM2.5 Air Pollution: Evolution and State‐of‐the‐Ar, Atmosphere 2020, 11, 58.

## Contacts ##
Rita Van Dingenen, Alessandro Mascherpa, Enrico Pisoni, Claudio Belis
