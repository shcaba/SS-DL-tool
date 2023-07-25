https://rpubs.com/SS-DL-tool/LO_CL

# The Stock Synthesis Data-limited Tool (SS-DL tool)

The SS-DL tool was developed by Dr. Jason Cope (NWFSC - NOAA) and use Stock Synthesis (Methot and Wetzel 2013) to implement several standard data-limited assessment methods all in one modeling framework. Under a unified modeling framework, additional data can be added as it becomes available. The tool builds Stock Synthesis files for provided data and life history information. It produces full plots and tables for each model run via the r4ss package and additional screen output for straightforward interpretation

# Installing libraries 
```R
packages<-c("devtools","shiny","shinyjs","ggplot2","reshape2","dplyr",
"tidyr","rlist","viridis","shinyWidgets","shinyFiles","plyr","shinybusy",
"truncnorm","ggpubr","flextable","officer","gridExtra","wesanderson",
"data.table","adnuts","shinystan")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
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

# Running the SS-DL tool

Running the tool can be accomplished inthe following way:

1. Access the repository [SS-DL-tool](https://github.com/shcaba/SS-DL-tool)

 - In "< > Code" Download the ZIP file
<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/36aee7f9-2188-40e6-8fac-0892a75f4ec8" alt="JuveYell" width="650px">
</p>
</div>


2. Extract the folder **SS-DL-tool-master** and open the <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/f959b660-8cee-48a7-a54b-42d2c2f91065" alt="JuveYell" width="70px"> or <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/8e4e9532-7275-4f14-b9ff-d49babd79d52" alt="JuveYell" width="50px">files 

**Obs.** Before running the first time, open both `server.r` and `ui.r` files to make sure all packages are installed. If any package is not installed, RStudio will signal..

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/6fb5fa85-f00d-413c-bc35-9aa7c38ad53e" alt="JuveYell" width="600px">
</p>
</div>

in RStudio and push the "`Run App`" button 

I recommend using the "`Run External`" option within the "`Run App`" button 
(see small arrow in button to change options)

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/94312609-9610-4baa-9c8d-0d5775999ea4" alt="JuveYell" width="600px">
</p>
</div>


3. Shiny will open and you can start the example

![Imagem3b](https://github.com/EidiK/SS-DL_example/assets/102596784/0340d3e8-7740-403c-981d-55f3228f9d7c)

# Length-only models

These are akin to LBSPR (Hordyk et al. 2015) and LIME (Rudd and Thorson 2017). Both styles of length-only models can be performed in this tool (determined by the estimation of recruitment), and include a choice between estimating F (recommended) or constant catch approaches (for stock status only; the estimated F values will not be useful on an absolute scale).

# Example SS-LO Constant Catch

Using constant catch assumes the same catch in all years in order to fit the length composition data (similar to LBSPR, but the model integrates the fit of each year, not each year separately)

It provides a long-term average response (F) to estimating stock status, and thus useful with non-continous years of sampling.

## Import the Length composition

For the example, import the Length composition file "`Lengths.csv`" that is in the "Example data files" folder of the "SS-DL-tool-master".

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/101c0440-8d07-4ad9-8966-6a68c732d218" alt="JuveYell" width="400px">
</p>
</div>


**Lengths.csv file setup**

The csv file must be formatted as follows: 
Year,Month,Fleet,Sex,Nsamps,(Vector of length bins associated with the length data).
As in the example:

|Year|Month|Fleet|Sex|Nsamps|20|25|30|35|40|45|50|55|60|
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
|1993|7	|1	|1|	59|	0|	2|	7|	3|	15|	23|	7|	2|	0|
|1994|7	|1	|2|	73|	0|	1|	9|	18|	17|	14|	9|	4|	1|
|1993|7	|2	|0|	42|	1|	4|	11|	10|	6|	9|	1|	0|	0|
|...|

**Fleet** = sequential number for each fleet     
**Sex** = 0 means combined male and female    
**Sex** = 1 means female only   
**Sex** = 2 means male only     


If the file is filled in correctly, the compositions referring to the fleets and sexes will be plotted in the "Data and Parameters"


![Imagem4c](https://github.com/EidiK/SS-DL_example/assets/102596784/bad87ed1-d70e-4c79-aa02-d23aeb145564)
 

. In the first fleet the data are separated by sex 
(sex = 1 for females; sex = 2 for males)
. In the second fleet the sexes are grouped (set sex = 0)

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
If using an existing model, chose 'None' to maintain the same weighting as the existing model

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/38ae1c57-146a-452f-a9bf-8415bd7d4ec4" alt="JuveYell" width="500px">
</p>
</div>

## Life history inputs

To fill in the life history data use the example of what is in the repository <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/d8f174f7-ddfa-4490-9b2d-80fccbdec9d3" alt="JuveYell" width="400px">

If you do not have separate sex data, it is possible to use only female information.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/ec4c211c-7575-483e-8c0b-51ab80f9c86f" alt="JuveYell" width="500px">
</p>
</div>

The parameters will be plotted in the "Data and Parameters"

![LHc](https://github.com/EidiK/SS-DL_example/assets/102596784/45145f5a-4995-4f83-9a3b-88bc32c272ad)


### Stock-recruitment parameters
Enter the Steepness value

Recruitment can also be estimated

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/165cf58d-92cd-4b4f-9460-be229c1359a5" alt="JuveYell" width="400px">
</p>
</div>

## Selectivity

Enter parameter and phase values for each fleet and survey.

Example using 50% selectivity with two fleets: Inputs could be 35,40 and 2,2 for starting values and phases respectively.

The phase input indicates estimated parameters. To fix the parameter, set the phase value to a negative number.

If using a mix of logistic and dome-shaped selectivities, select dome-shaped and fix (i.e., use a negative phase) the provided default values (10000,0.0001,0.9999 for the additonal parameters, respectively) to achieve a logistic shape for any given fleet.

The selectivity data is also in the repository<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/d8f174f7-ddfa-4490-9b2d-80fccbdec9d3" alt="JuveYell" width="400px">

![Sel](https://github.com/EidiK/SS-DL_example/assets/102596784/5b52d140-6261-4957-8dce-ddcb084f0dfa)

## Additional SS options

A list of additional options can be used in the model, for our example we will select the option “`Use only one growth type (default is 5)`”

The number of platoons within a growth type/morph allows exploration of size-dependent survivorship. A value of 1 will not create additional platoons. 

Odd-numbered values (i.e., 3, 5) will break the overall morph into that number of platoons creating a smaller, larger, and mean growth platoon. The higher the number of platoons the slower SS will run. 


<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/baf4e626-94c9-4ee4-b0ee-5c280b77b6ff" alt="JuveYell" width="400px">
</p>
</div> 

## Fininal step - Run the model

Choose a name for the scenario and the repository

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/51794a83-bbae-435c-ab4c-138e8e845458" alt="JuveYell" width="400px">
</p>
</div>

If all the input data are in agreement the following image will appear  <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/8cfd3fe2-b83d-4ff5-a48e-ad9e0ed37baf" alt="JuveYell" width="70px">

## Examining the results

After running the model the results will be displayed in your browser with a header for each type of output. 

![Resultsa](https://github.com/EidiK/SS-DL_example/assets/102596784/c6e28b06-c725-4c9b-8ef6-931085ef5a98)

The results and plot files will also be saved in your directory.

If you have not selected a directory, the folder containing the template files will be in the "SS-DL-tool-master > Scenarios"
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/89dc3944-b862-47da-b0c6-a30a8b904a9d" alt="JuveYell" width="450px">

### Length composition adjustment

It is important to check the fit of the data in the models. 

Be sure to check the fit to the length compositions.

![LenComp](https://github.com/EidiK/SS-DL_example/assets/102596784/240d3b7d-fb32-4a40-a003-1cd565eabb64)

Also check to see if the selectivity estimates make sense in the "Sel" tab.
Always check to see if the selectivity patterns being applied in the model make sense before looking at model outputs as they can have large influences on those outputs.

![Selb](https://github.com/EidiK/SS-DL_example/assets/102596784/cdb1771b-d4f2-4dfa-bbdc-075d1f234ab5)


### Stock status

![Stokstatus](https://github.com/EidiK/SS-DL_example/assets/102596784/7cfb3d94-0457-4872-8406-e79617f9c14b)

The values used to generate the plots can be obtained from the directory <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/8f29af5e-5ea4-4548-aa38-6261f5a76c91" alt="JuveYell" width="500px">

# Example SS-LO Estimate F 

Fishing rate can also be directly estimated from the length composition with no assumption in catches (similar to the LIME approach).

This approach is a more variable reponse to estimating stock status, and is best used with continuous years of contemporary data. 

## Setup and Running

Let's use exactly the same settings as in the previous example (**SS-LO Constant Catch**). 

We will only change the Setup to the **Estimate F** approach.

After setting the other parameters of "***Life history***", "***Recruitment***", "***Selectivity***" and "***Additional SS options***" in the same way as the SS-LO Constant Catch example, just run the model.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/54a6af59-d387-49de-ac45-fa0ec51f4c13" alt="JuveYell" width="350px">
</p>
</div>

## Examining the results

As with the previous example, before analyzing the results, be sure to check the fit for the length compositions and also check that the selectivity estimates make sense.

### Stock status

![Stokstatus2](https://github.com/EidiK/SS-DL_example/assets/102596784/84979634-7abc-4104-9ec4-634eb4894828)

The F-based approach often does is it makes the very last reported year go to zero or very low.  So just beware that little issue and report the depletion value in the year of the last catch.


The values used to generate the plots can be obtained from the directory <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/8f29af5e-5ea4-4548-aa38-6261f5a76c91" alt="JuveYell" width="500px">

In our example, the year of the last catch is 2020 and the **Fraction Unfished** for that year is **0.38**

# Example Length + catch-based (SS-CL) 

Used for estimation o biomass , relative biomass, SPR and F and capture limits

## Setup and Running

Let's use exactly the same settings as in the previous example (SS-LO). 

We will only include the catch series and additional life history parameters.


![Catch](https://github.com/EidiK/SS-DL_example/assets/102596784/edc3254c-d2a9-40dd-ad49-35e516065c30)


Data can be found in the repository <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/4b055769-ef01-4723-9ffc-fa86e9f78d03" alt="JuveYell" width="450px">


You can use names for the sequential fleets if you want. 
The csv file must be formatted as follows: 

|Year|Commercial|Recreational|
|----|----|----|
|1892|0.001663661|	0|
|1893|0.001663661|	0|
|1894|0.001663661|	0|
|...|


For the catch and length models (SS-CL) it is necessary to include the Length-weight relationship W=aL^b and Length-fecundity relationship F=aL^b parameters. These values are also in the `"Example data files"` folder > `"Life history.xlsx"`

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/12f5e0c7-dd28-4712-ae4b-86db67f659a5" alt="JuveYell" width="700px">
</p>
</div>


**Obs. Estimating Life history imputs**

We provide the fixed life history option example, which mostly what should be done. But life history parameters can also be estimated in the SS-DL tool, and Rudd et al. (2021) give guidance on when that might happen in SS-DL models.


For example, you can estimate natural mortality internally in the model by setting **Yes** in the "**Estimate parameters?**" setting   <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/e0b45e4c-958d-4dca-a6eb-0b07adc54a5f" alt="JuveYell" width="100px">. Not all parameters need be estimated. To fix other parameters, such as Linf, just turn the phase to negative phase (e.g., -1)

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/5383a64f-589b-41f2-9f48-c907d00fc4ed" alt="JuveYell" width="500px">
</p>
</div>


However for this example we will use **all life history parameters in a fixed way**. After setting the other parameters of "***Recruitment***", "***Selectivity***" and "***Additional SS options***" in the same way as the SS-LO example, just run the model.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/a77b8b39-0dd7-4ed6-8bde-a99adc105f2e" alt="JuveYell" width="400px">
</p>
</div>

## Examining the results

As with the previous example, before analyzing the results, be sure to check the fit for the length compositions and also check that the selectivity estimates make sense.

![timeseries-SS-CL](https://github.com/EidiK/SS-DL_example/assets/102596784/513b7eaf-034a-4672-afd9-b4033cd297ff)

The values used to generate the plots can be obtained from the directory <img src="https://github.com/EidiK/SS-DL_example/assets/102596784/8f29af5e-5ea4-4548-aa38-6261f5a76c91" alt="JuveYell" width="500px">


# Likelihood profiles

Likelihood profiles are a powerful way to understand the information content of data and sensitivity of models to parameter uncertainty.

A likelihood profile fixes a chosen parameter to a specified set of values in a reference model. The reference model will maintain estimation of any other parameters estimated in the reference model.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/f4090c10-76f4-4b87-92c9-013f829d0b62" alt="JuveYell" width="700px">
</p>
</div>

For example, Steepness (***h***) could be profiled over the value 0.5 to 0.9 at steps of 0.05. This creates 10 model runs that fix ***h*** to different values while keeping all other specifications the same as the reference model (e.g., SS-CL_Example_V01).

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/71aadf7e-7a7e-4fd5-a7a1-e3229ebabeae" alt="JuveYell" width="300px">
</p>
</div>

For each model run, the likelihood value and derived outputs are retained for analysis.

Any likelihood values >1.96 units from the minimum value are identify as models statistically less supported by the data.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/6e04c416-7c69-47a0-a7c2-6b62e380c227" alt="JuveYell" width="500px">
</p>
</div>

Plots with the profiled parameter values compared to the likelihood values and derived model outputs indicate how much information is contained in the model for the parameter and how sensitive the model is to parameters values resulting non-statistically different models.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/a7ad12b0-a48a-4f7e-9ed3-724bbb1af792" alt="JuveYell" width="500px">
</p>
</div>

# Sensitivity comparison plots

Comparing models offers insight into how changing data or model specification change model outputs.


<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/1f142171-8f6a-4069-a80f-6db6cbcd5f44)
792" alt="JuveYell" width="700px">
</p>
</div>

To make these comparisons, choose first the directory containing models, then the models to compare.

<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/739635c3-72a4-440b-906a-2996a8a7a2fc" alt="JuveYell" width="400px">
</p>
</div>

A complete compliment of comparison plots are saved in the chosen directory in a folder labeled



<div>
<p style = 'text-align:center;'>
<img src="https://github.com/EidiK/SS-DL_example/assets/102596784/b76a7d12-d861-4c07-892c-286233f3e3b1" alt="JuveYell" width="600px">
</p>
</div>

|   |	SS-CL_V01|	SS-LO_Constant_C_V01|	SS-LO_Estimate_F_V01|
|----|----|----|----|
|SB<sub>0</sub> / SB<sub>2020</sub>|	0.472717|	0.347567|	0.385021|

# References

  Cope J.M. (2020). The Stock Synthesis Data-limited Tool (SS-DL tool). https://github.com/shcaba/SS-DL-tool#the-stock-synthesis-data-limited-tool-ss-dl-tool (Accessed 01 May 2023).

  Hordyk, A., Ono, K., Valencia, S., Loneragan, N., & Prince, J. (2015). A novel length-based empirical estimation method of spawning potential ratio (SPR), and tests of its performance, for small-scale, data-poor fisheries. ICES Journal of Marine Science, 72(1), 217-231.

  Methot Jr, R. D., & Wetzel, C. R. (2013). Stock synthesis: a biological and statistical framework for fish stock assessment and fishery management. Fisheries Research, 142, 86-99.

  Rudd, M. B., & Thorson, J. T. (2017). Accounting for variable recruitment and fishing mortality in length-based stock assessments for data-limited fisheries. Canadian Journal of Fisheries and Aquatic Sciences, 75(7), 1019-1035.

  Rudd, M. B., Cope, J. M., Wetzel, C. R., & Hastie, J. (2021). Catch and length models in the stock synthesis framework: expanded application to data-moderate stocks. Frontiers in Marine Science, 8, 663554.
