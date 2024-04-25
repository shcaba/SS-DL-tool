
# The Stock Assessment Continuum Tool (previously Stock Synthesis Data-Limited Tool)

The Stock Assessment Continuum Tool was developed by Dr. Jason Cope (NWFSC - NOAA) and uses Stock Synthesis (SS3) (Methot and Wetzel 2013) to implement a variety of assessment configurations. This tool was initially built to implement several standard data-limited assessment methods within the SS3 modeling framework. Now, it has been expanded to be able to implement more complex and data-rich configurations. The tool builds Stock Synthesis input files for provided data and life history information. Under a unified framework, additional data and SS3 options can be added as it becomes available, either within the tool itself, if that feature would be useful to the wider community of stock assessment scientists, or options can be changed directly in the Stock Synthesis input files (example input files used by the tool found [here](https://github.com/shcaba/SS-DL-tool/tree/master/SSS_files/sssexample_BH)). It produces full plots and tables for each model run via the [{r4ss}](https://github.com/r4ss/r4ss) package and additional screen output for straightforward interpretation. The [Stock Synthesis team](https://github.com/nmfs-stock-synthesis) endorses this tool as a way to help users provide input data and model settings rather than changing the SS3 text-based input files directly.  
For more information on how to use the tool and on the concept of the stock assessment continuum, please see this [video](https://www.youtube.com/watch?v=NFJPoFJ9qyo).

THANK YOU to the following people who have contributed signigicantly to this application:  Luis Gustavo Cardoso, Eidi Kikuchi, Elizabeth Perl, [Brian Snouffer](https://www.upwell.solutions/), and Ben Williams. 

# Installing libraries 
```R
packages<-c("devtools","shiny","shinyjs","ggplot2","reshape2","dplyr","tidyr",
"Rcpp","rlist","viridis","shinyWidgets","shinyFiles","plyr","shinybusy",
"truncnorm","ggpubr","flextable","officer","gridExtra","wesanderson","data.table",
"adnuts","shinystan","shinyBS","gt","gtExtras","stringr","ggnewscale","msm",
"EnvStats","tmvtnorm","future","parallel","parallelly","fs","tools")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

Make sure the following packages are using the most recent versions:
library(devtools)
devtools::install_github("shcaba/SSS", build_vignettes = TRUE)

install.packages("remotes")
remotes::install_github("r4ss/r4ss")
remotes::install_github("chantelwetzel-noaa/HandyCode")
remotes::install_github("nwfsc-assess/nwfscDiag")

It is recommended to make sure all additional open windows of R or Rstudio 
(beside the one being used) are closed prior to updating libraries, and that one 
restarts Rstudio after all new installations. 
Many of the errors when running the SS-DL tool arise from keeping libraries updated 
or installed (especially r4ss).
```

# Troubleshooting installation

Some users run into issues with the intial installation.
The most common issues (and solutions) are 

1. Libraries do not properly install.  

**SOLUTIONS:** 
  - If you open the ui.r and server.r in RStudio, required libraries that are uninstalled should show up at the top of the text editor
  - If you run the app for the first time, look at the R console to make sure all libraries loaded properly. If some don't, you will need to install those libraries.
  - Pay particular attention to the libraries that are installed from GitHub. Those may not always install or update properly, so double check their presence. Some agencies do not allow the use of GitHub, thus they are unable to install from GitHub. If this is the case, you may be able to access the tar.gz file from the website noted in the installation error message. You can then use the install function to call that file to load the library. 
  - There are some libraries that may be installed, but need to be updated. If you have any other instances of R open, you will need to close all instances before updating libraries. I highly encourage keeping all libraries relatively up to date. I also encourage updating R every few months to a couple times a year. Some librararies may refuse to update (e.g., curl). If this is the case, uninstall the library and reinstall it, then restart R. Always restart R once a library is either installed or updated to make sure the most recent version is being used.
  - The app may open, but when you try a model run, it crashes. There still may be some libraries that have not been installed. Look at and around the error message in the R console to see if it refers to a missing library. 

2. Model folder locations
  - Downloading the SAC tool repo should place it somewhere on your local machine. If you have put this folder on the cloud, it will not run. Make sure the app folder is located in an accessible/writeable portion of your local machine.
   

# Running the SS-DL tool

Running the tool can be accomplished in the following way:

1. Access the repository [SS-DL-tool](https://github.com/shcaba/SS-DL-tool)

 - In "< > Code" Download the ZIP file
<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Imagem1.png" alt="JuveYell" width="650px">
</p>
</div>


2. Extract the folder **SS-DL-tool-master** and open the <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/ui.png" alt="JuveYell" width="70px"> or <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/server.png" alt="JuveYell" width="50px">files 

**Obs.** Before running the first time, open both `server.r` and `ui.r` files to make sure all packages are installed. If any package is not installed, RStudio will signal..

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Imagem2b.png" alt="JuveYell" width="600px">
</p>
</div>

in RStudio and push the "`Run App`" button 

I recommend using the "`Run External`" option within the "`Run App`" button 
(see small arrow in button to change options)

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Imagem2.png" alt="JuveYell" width="600px">
</p>
</div>


3. The Shiny will open and you can start the example

![Imagem3b](https://github.com/shcaba/SS-DL-tool/blob/master/images/Imagem3b.png)

# Model inputs and outputs

## Model input
The SAC tool allows for a variety of model specification depending on the data that are available. Model inputs can be based on 
- Manual input.
- Using a .rds file. The .rds file is produced by each model (input.rds) in the "Scenario_input" file, and it is a bookmark of all values from the given model scenario. This can be read into the tool in the "Upload bookmarked inputs" window at the top of the side panel, just above the data inputs. It can be named anything as long as the extension is .rds (i.e., it does not have to be named input.rds). You will find examples of these files for a variety of model specifications using the example data files in the "Bookmark inputs" folder within the "Example data files" folder.
  - CO.rds: Catch-only model
  - LO.rds: Length-only model
  - LO_recs_badj.red: Length-only with bias-adjusted recruitment estimation
  - CL.rds: Catch and length model
  - CL_estLH.rds: Catch and length model using the estimate life history set-up
- Use an existing SS3 model. Clicking this button assumes inputs have been modified by the user directly in the SS3 files. This allows running any SS3 model outside of relying on the app GUI to input model specifications, though there remain some additional options to add to the model if desired.

## Model output
Each model folder contains 
- All of the created SS3 files associated with the model. The "Report.sso" file is particularly useful when considering model results.
- The "Scenario_input" folder that contains the bookmarked model specifications (input.rds file) and a copy of the data files used.
- An optional "plots" folder that includes all of r4ss diagnostic and results plots from the model (HIGHL RECOMMENDED)
- An option "tables" folder that includes a variety of summary tables. It is recommended to run this when you have a final model in order to save processing time.

# Running different model types
# Length-only models

These are akin to LBSPR (Hordyk et al. 2015) and LIME (Rudd and Thorson 2017). Both styles of length-only models can be performed in this tool (determined by the estimation of recruitment), and include a choice between estimating F (this attempts to estimate F based on the length compositions) or constant catch approaches (for stock status only; the estimated F values will not be useful on an absolute scale). These length-only models are still in development, but have been applied many times and provide a solid option for situations with only length, whether a snapshot (1 year) or repeated samples (many years). 

# Example SS-LO Constant Catch

Using constant catch assumes the same catch in all years in order to fit the length composition data (similar to LBSPR, but the model integrates the fit of each year, not each year separately)

It provides a long-term average response (F) to estimating stock status, and thus useful with non-continuous years of sampling.

## Import the Length composition

For the example, import the length composition file "`Lengths.csv`" that is in the "Example data files" folder of the "SS-DL-tool-master".

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Imagem4a.png" alt="JuveYell" width="400px">
</p>
</div>


**Lengths.csv file setup**

The csv file must be formatted as follows: 
Year,Month,Fleet,Sex,Nsamps,(Vector of length bins associated with the length data).
As in the example:

|Year|Month|Fleet|Sex|Nsamps|20|25|30|35|40|45|50|55|60|
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
|1993|7	|1	|1|	59|	0|	4|	14|	9|	35|	28|	11|	5|	0|
|1994|7	|1	|2|	73|	0|	2|	15|	35|	40|	32|	20|	8|	3|
|1993|7	|2	|0|	42|	1|	8|	24|	25|	15|	10|	2|	0|	0|
|...|

**Fleet** = sequential number for each fleet     
**Sex** = 0 means combined male and female    
**Sex** = 1 means female only   
**Sex** = 2 means male only     


If the file is filled in correctly, the compositions referring to the fleets and sexes will be plotted in the "Data and Parameters".


![Imagem4c](https://github.com/shcaba/SS-DL-tool/blob/master/images/Imagem4c.png)
 

In the first fleet the data are separated by sex 
(sex = 1 for females; sex = 2 for males). 
In the second fleet the sexes are grouped (set sex = 0)

"Nsamps" is the effective sample size of the biological compositions. Effective sample size is used to provide a measure of independent biological sampling units. When fish are sampled they often are not independent of each other (e.g., similar ages or sizes may congregate together). This is why the number of sample individuals is rarely a good estimate of effective (i.e., independent) sample size. While it is hard to know the exact number of independent samples, using sampling units such as tows, hauls, trips or other aggregated units may give a better approximation of independence. It is recommended then to not use the total number of fish as the inputted effective sample size, but to identify a different sampling unit. Input effective sample size sets the relative weights of each sample strata (e.g., year, fleet, sex), and thus should be chosen thoughtfully.  

## Approach and data weighting

1. Setup for the **Constant Catch** approach

2. **Weight fleet lengths by relative catch**
The relative catch contribution needs specification with multiple length-only fleets
Example: Two fleets, with fleet 2 catching 2 times the amount as fleet 1, the entry would be 1,2.
Each entry will be relative to the highest value.

3. **Data-weighting**
Data weighting balances information content of biological data with model structure
Data weighting balances across factors (e.g, fleets, sex, etc.)
The default value is equally weighting among fleets based on input effective sample size inputs
If using an existing model, chose 'None' to maintain the same weighting as the existing model or choose one of the other weighting options.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/SetupConstantC.png" alt="JuveYell" width="500px">
</p>
</div>

## Life history inputs

To fill in the life history data use the example of what is in the repository <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/LH.png" alt="JuveYell" width="400px">

If you do not have separate sex data, it is possible to use only female information.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/LHb.png" alt="JuveYell" width="500px">
</p>
</div>

The parameters will be plotted in the "Data and Parameters"

![LHc](https://github.com/shcaba/SS-DL-tool/blob/master/images/LHc.png)


### Stock-recruitment parameters
Enter the Steepness value

Recruitment can also be estimated

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/SR.png" alt="JuveYell" width="400px">
</p>
</div>

## Selectivity

Enter parameter and phase values for each fleet and survey.

Example using 50% selectivity with two fleets: Inputs could be 35,40 and 2,2 for starting values and phases respectively.

The phase input indicates estimated parameters. To fix the parameter, set the phase value to a negative number.

If using a mix of logistic and dome-shaped selectivities, select dome-shaped and fix (i.e., use a negative phase) the provided default values (10000,0.0001,0.9999 for the additonal parameters, respectively) to achieve a logistic shape for any given fleet.

Starting selectivity parameters inputs are also in the repository<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/LH.png" alt="JuveYell" width="400px">

![Sel](https://github.com/shcaba/SS-DL-tool/blob/master/images/Sel.png)

## Additional SS options

A list of additional options can be used in the model, for our example we will select the option “`Use only one growth type (default is 5)`”

The number of platoons within a growth type/morph allows exploration of size-dependent survivorship. A value of 1 will not create additional platoons. 

Odd-numbered values (i.e., 3, 5) will break the overall morph into that number of platoons creating a smaller, larger, and mean growth platoon. The higher the number of platoons the slower SS will run. 


<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/addOP.png" alt="JuveYell" width="400px">
</p>
</div> 

## Final step - Run the model

Choose a name for the scenario and the repository

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Run.png" alt="JuveYell" width="400px">
</p>
</div>

If all the input data are in agreement the following image will appear  <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Runb.png" alt="JuveYell" width="70px">

## Examining the results

After running the model the results will be displayed in your browser with a header for each type of output. 

![Resultsa](https://github.com/shcaba/SS-DL-tool/blob/master/images/Resultsa.png)

The results and plot files will also be saved in your directory. You can always recover these html plots by going into the "plots" folder of the chosen run and selecting any of the .html files.

<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Results.png" alt="JuveYell" width="450px">

If you have not selected a directory, the folder containing the template files will be in the "SS-DL-tool-master > Scenarios > plots"

### Length composition adjustment

It is important to check the fit of the data in the models. 

In this example be sure to check the fit to the length compositions.

![LenComp](https://github.com/shcaba/SS-DL-tool/blob/master/images/LenComp.png)

Also check to see if the selectivity estimates make sense in the "Sel" tab.
Always check to see if the selectivity patterns being applied in the model make sense before looking at model outputs as they can have large influences on those outputs.

![Selb](https://github.com/shcaba/SS-DL-tool/blob/master/images/Selb.png)


### Stock status

![Stockstatus](https://github.com/shcaba/SS-DL-tool/blob/master/images/Stockstatus.png)

The values used to generate the plots can be obtained from the directory <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/timeseries.png" alt="JuveYell" width="500px">

# Example SS-LO Estimate F 

Fishing rate can also be directly estimated from the length composition with no assumption in catches (similar to the LIME approach).

This approach is a more variable response to estimating stock status, and is best used with continuous years of contemporary data. The values of F can jump around, so take care in interpreting the F values. Multiple F values are provided in the model outputs.

## Setup and Running

Let's use exactly the same settings as in the previous example (**SS-LO Constant Catch**). 

We will only change the Setup to the **Estimate F** approach.

After setting the other parameters of "***Life history***", "***Recruitment***", "***Selectivity***" and "***Additional SS options***" in the same way as the SS-LO Constant Catch example, just run the model.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/SetupEstimateF.png" alt="JuveYell" width="350px">
</p>
</div>

## Examining the results

As with the previous example, before analyzing the results, be sure to check the fit for the length compositions and also check that the selectivity estimates make sense.

### Stock status

![Stokstatus2](https://github.com/shcaba/SS-DL-tool/blob/master/images/Stockstatus2.png)

The F-based approach often results in the very last reported year go to drop to zero or very low biomass.  Beware of this behavior and report the depletion value in the year of the last catch.


In our example, the year of the last catch is 2020 and the **Fraction Unfished** for that year is **0.38**

# Example Length + catch-based (SS-CL) 

Used for estimation o biomass , relative biomass, SPR and F and capture limits

## Setup and Running

Let's use exactly the same settings as in the previous example (SS-LO). 

We will only include the catch series and additional life history parameters.


![Catch](https://github.com/shcaba/SS-DL-tool/blob/master/images/Catch.png)


Data can be found in the repository <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/catches.png" alt="JuveYell" width="450px">


You can use names for the sequential fleets if you want. 
The csv file must be formatted as follows: 

|Year|Commercial|Recreational|
|----|----|----|
|1892|0.001663661|	0|
|1893|0.001663661|	0|
|1894|0.001663661|	0|
|...|


For the catch and length models (SS-CL) it is necessary to include the length-weight relationship W=aL^b and Length-fecundity relationship F=aL^b parameters. These values are also in the `"Example data files"` folder > `"Life history.xlsx"`

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/LH_SS-CL.png" alt="JuveYell" width="700px">
</p>
</div>


**Obs. Estimating Life history inputs**

We provide the fixed life history option example, which mostly what should be done. But life history parameters can also be estimated in the SS-DL tool, and Rudd et al. (2021) give guidance on when that might happen in SS-DL models.


For example, you can estimate natural mortality internally in the model by setting **Yes** in the "**Estimate parameters?**" setting   <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/EstimateLH.png" alt="JuveYell" width="100px">. Not all parameters need be estimated. To fix other parameters, such as Linf, just turn the phase to negative phase (e.g., -1)

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/EstimateM.png" alt="JuveYell" width="500px">
</p>
</div>


However for this example we will use **all life history parameters in a fixed way**. After setting the other parameters of "***Recruitment***", "***Selectivity***" and "***Additional SS options***" in the same way as the SS-LO example, just run the model.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/runSS-CL.png" alt="JuveYell" width="400px">
</p>
</div>

## Examining the results

As with the previous example, before analyzing the results, be sure to check the fit for the length compositions and also check that the selectivity estimates make sense.

![timeseries-SS-CL](https://github.com/shcaba/SS-DL-tool/blob/master/images/timeseries-SS-CL.png)

The values used to generate the plots can be obtained from the directory <img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/timeseries.png" alt="JuveYell" width="500px">


# Likelihood profiles

Likelihood profiles are a powerful way to understand the information content of data and sensitivity of models to parameter uncertainty.

A likelihood profile fixes a chosen parameter to a specified set of values in a reference model. The reference model will maintain estimation of any other parameters estimated in the reference model.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Likelihood.png" alt="JuveYell" width="700px">
</p>
</div>

For example, Steepness (***h***) could be profiled over the value 0.5 to 0.9 at steps of 0.05. This creates 10 model runs that fix ***h*** to different values while keeping all other specifications the same as the reference model (e.g., SS-CL_Example_V01).

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/Likelihood2.png" alt="JuveYell" width="300px">
</p>
</div>

For each model run, the likelihood value and derived outputs are retained for analysis.

Any likelihood values >1.96 units from the minimum value are identify as models statistically less supported by the data.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/parameter_panel_SR_BH_steep.png" alt="JuveYell" width="500px">
</p>
</div>

Plots with the profiled parameter values compared to the likelihood values and derived model outputs indicate how much information is contained in the model for the parameter and how sensitive the model is to parameters values resulting non-statistically different models.

The contribution of each likelihood component to the overall likelihood profile can also be explored. This can show what data types are informing what parameter values.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/piner_panel_SR_BH_steep.png" alt="JuveYell" width="500px">
</p>
</div>

# Sensitivity comparison plots

Comparing models derived outputs offers insight into how changing data or model specification change model outputs.


<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/comparision.png" alt="JuveYell" width="700px">
</p>
</div>

To make these comparisons, choose first the directory containing models, then the models to compare.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/comparisiondir.png" alt="JuveYell" width="400px">
</p>
</div>

A complete compliment of comparison plots are saved in the chosen directory in a folder labeled



<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/comparisionplot.png" alt="JuveYell" width="600px">
</p>
</div>

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/shcaba/SS-DL-tool/blob/master/images/comparisionplot1.png" alt="JuveYell" width="600px">
</p>
</div>


|   |	SS-CL_V01|	SS-LO_Constant_C_V01|	SS-LO_Estimate_F_V01|
|----|----|----|----|
|SB<sub>0</sub> / SB<sub>2020</sub>|	0.47|	0.35|	0.39|

# References

  Cope J.M. (2020). The Stock Synthesis Data-limited Tool (SS-DL tool). https://github.com/shcaba/SS-DL-tool#the-stock-synthesis-data-limited-tool-ss-dl-tool (Accessed 01 May 2023).

  Hordyk, A., Ono, K., Valencia, S., Loneragan, N., & Prince, J. (2015). A novel length-based empirical estimation method of spawning potential ratio (SPR), and tests of its performance, for small-scale, data-poor fisheries. ICES Journal of Marine Science, 72(1), 217-231.

  Methot Jr, R. D., & Wetzel, C. R. (2013). Stock synthesis: a biological and statistical framework for fish stock assessment and fishery management. Fisheries Research, 142, 86-99.

  Rudd, M. B., & Thorson, J. T. (2017). Accounting for variable recruitment and fishing mortality in length-based stock assessments for data-limited fisheries. Canadian Journal of Fisheries and Aquatic Sciences, 75(7), 1019-1035.

  Rudd, M. B., Cope, J. M., Wetzel, C. R., & Hastie, J. (2021). Catch and length models in the stock synthesis framework: expanded application to data-moderate stocks. Frontiers in Marine Science, 8, 663554.
