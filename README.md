# The Stock Synthesis Data-limited Tool (SS-DL tool)

The SS-DL tool uses Stock Synthesis (Methot and Wetzel 2013) to implement several common data-limited assessment methods all in one modelling framework. Under a unified modelling framework, additional data can be added as it becomes available. The tool builds Stock Synthesis files for provided data and life history information. It produces full plots and tables for each model run via the r4ss package, as well as additional screen output for easy interpretation.
<br></br>

## Installing libraries and running the SS-DL tool
```R
packages<-c("devtools","shiny","shinyjs","ggplot2","reshape2","dplyr","tidyr","rlist","viridis","shinyWidgets","shinyFiles","plyr","shinybusy","truncnorm","ggpubr","flextable","officer","gridExtra")

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

It is recommended to make sure all additional open windows of R or Rstudio (beside the one being used) are closed prior to updating libraries, and that one restarts Rstudio after all new installations. Many of the errors when running the SS-DL tool arise from keeping libraries updated or installed (especially r4ss).

Running the tool can be accomplished in any of the following ways:
1) shiny::runApp(ENTER HERE USER PATH TO FOLDER CONTAINING THE SS-DL files)
2) Open the server.r or ui.r files in RStudio and push the "Run App" button (top rigt corner of the source panel). 
	I recommend using the "Run External" option within the "Run App" button (see small arrow in button to change options)
3) runGitHub("SS-DL-tool", "shcaba",destdir=mydir) where mydir is the path you chose to obtain results.
```

## SS-DL tool features the following data-limited applications
* Length-only models. These are akin to LBSPR (Hordyk et al. 2015) and LIME (Rudd and Thorson 2017). Both styles of length-only models can be performed in this tool (determined by the estimation of recruitment), and include a choice between estimating F (recommended) or constant catch approaches (for stock status only; the estimated F values will not be useful on an absolute scale).
* Age-only models
* Catch only methods via the Simple Stock Synthesis approach (Cope 2013)
* Extended Simple Stock Syntehsis (XSSS; Cope et al. 2013; Cope et al. 2015; Wetzel and Punt 2013). MLE version is ready, AIS version is *COMING SOON*
* Length + catch models 
* Catch and index (age-structured production) models
* Fully integrated stock assessments
<br></br>

## Some notable features of the tool
* The tool accepts multi-fleet, sex-specific data, as well as sex-specific life history parameters.
* It allows for estimation of dome-shaped, as well as logistic, for each fleet, with the ability to fix selectivity parameters instead of estimating them.
* Recruitment estimation is provided for models with length or age data.
* Output plots are saved in the scenarion folder, as well as tables of model output summaries.
* A jittering option is provided to allow for model stability evaluation and global  away from starting values.
* Easy application of likelihood profiles across multiple parameters
* Sensitivity explortaion is easy to do (just change model specification and re-run) and compare among models (with plots and summaries determined by the user).f
* Ensemble modelling option to combine model outputs across models using weights determined by the user.
<br></br>

## Future features will include:
* Bayesian uncertainty estimation
* Ensemble modelling
<br></br>

## General guidance using the SS DL tool
* Length and age samples need to be representative of the stock being measured. This usually means random samples covering the area of interest. Beware of serial depletion in lengths that may keep the length struture artificially high (i.e., hyperstability). 
* When entering lengths or ages, the sex codes are as follows: 0=unknown; 1=female; 2=male
* Age sample treatment (either conditional age-at-length or marginal age compositions) is specified using the "Lbin_low" and "Lbin_hi" inputs. 
	* Ages conditioned on lengths: for a given row of ages, input the a length bin (or bins) for the low and high length bin input that the subsequent ages are assigned. Example would be Lbin_low = 18 and Lbin_hi = 18.  
	* Marginal ages: low and high length bins should be noted as -1 and -1. See example age file input.
* Selectivity starting parameters can be determined from the length compositions. Modes in catch are L95% starting values, and halfway between that value and the length at first capture is a good L50% starting values.
* Be sure to check the fit to the length compositions.
* Check for model convergence in the "Model output" tab. Also check to see if the selectivity estimates make sense.
* If convergence is an issue, try the "Jitter" feature. Once you find a converged model, you can increase the number of jitters you do to make sure that model is the best fit model.
* Always check to see if the selectivity patterns being applied in the model make sense before looking at model outputs as they can have large influences on those outputs.

