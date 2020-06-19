# The Stock Synthesis Data-limited Tool (SS-DL tool)

The SS-DL tool uses Stock Synthesis (Methot and Wetzel 2013) to implement several common data-limited assessment methods all in one modelling framework. Under a unified modelling framework, additional data can be added as it becomes avaialble.
The tool builds Stock Synthesis files for provided data and life history information. It produces full plots and tables for each model run via the r4ss package, as well as additional screen output for easy interpretation.
<br></br>

## Installing libraries and running the SS-DL tool
```R
install.packages("devtools","shiny","shinyjs","r4ss","ggplot2","reshape2","dplyr","tidyr","rlist","viridis","sss")

Make sure the following package is using the most recent versions:
library(devtools)
devtools::install_github("shcaba/SSS", build_vignettes = TRUE)


Load the Shiny library
library(shiny)

Running the tool can be accomplished in any of the following ways:
1) runApp(ENTER HERE USER PATH TO FOLDER CONTAINING THE SS-DL files)
2) Open the server.r or ui.r files in RStudio and push the "Run App" button (top rigt corner of the source panel). 
	I recommend using the "Run External" option within the "Run App" button (see small arrow in button to change options)
3) runGitHub("SS-DL-tool", "shcaba",destdir=mydir) where mydir is the path you chose to obtain results.
```

## SS-DL tool features the following data-limited applications
* Length-only models. These are akin to LBSPR (Hordyk et al. 2015) and LIME (Rudd and Thorson ). Both styles of length-only models can be performed in this tool.
* Age-only models
* Catch only methods via the Simple Stock Synthesis approach (Cope 2013) *COMING SOON*
* Extended Simple Stock Syntehsis (XSSS; Cope et al. 2013; Cope et al. 2015; Wetzel and Punt 2013) *COMING SOON*
* Length + catch models 
* Catch and index (age-structured production) models *COMING SOON*
* Fully integrated stock assessments *COMING SOON*
<br></br>

## Some notable features of the tool
* The tool accepts multi-fleet, sex-specific data, as well as sex-specific life history parameters.
* It allows for estimation of dome-shaped, as well as logistic, for each fleet, with the ability to fix selectivity parameters instead of estimating them.
* Recruitment estimation is provided for models with length or age data.
* A jittering option is provided to allow for model stability evaluation and global  away from starting values.
* Simple model sensitivity explortaion
* Output plots are saved in the scenarion folder, as well as tables of model output summaries.
<br></br>

## Future features will include:
* Likelihood profile implementation
* Model sensitivity comparisons
* Bayesian uncertainty estimation
* Ensemble modelling
<br></br>

## General guidance using the SS DL tool
* Length and age samples need to be representative of the stock being measured. This usually means random samples covering the area of interest. Beware of serial depletion in lengths that may keep the length struture artificially high (i.e., hyperstability). 
* Selectivity starting parameters can be determined from the length compositions. Modes in catch are L95% starting values, and halfway between that value and the length at first capture is a good L50% starting values.
* Be sure to check the fit to the length compositions.
* Check for model convergence in the "Model output" tab. Also check to see if the selectivity estimates make sense.
* If convergence is an issue, try the "Jitter" feature. Once you find a converged model, you can increase the number of jitters you do to make sure that model is the best fit model.

