# The Stock Synthesis Data-limited Tool (SS-DL tool)

The SS-DL tool uses Stock Synthesis (Methot and Wetzel 2013) to implement several common data-limited assessment methods all in one modelling framework. Under a unified modelling framework, additional data can be added as it becomes avaialble.
The tool builds Stock Synthesis files for provided data and life history information. It produces full plots and tables for each model run via the r4ss package, as well as additional screen output for easy interpretation.

SS-DL tool features the following data-limited applications
*Length-only models. These are akin to LBSPR (Hordyk et al. 2015) and LIME (Rudd and Thorson ). Both styles of length-only models can be performed in this tool.
*Age-only models
*Catch only methods via the Simple Stock Synthesis approach (Cope 2013)
*Extended Simple Stock Syntehsis (XSSS; Cope et al. 2013; Cope et al. 2015; Wetzel and Punt 2013)
*Length + catch models 
*Age-structured production models
*Fully integrated stock assessments


Some notable features of the tool
*The tool accepts multi-fleet, sex-specific data, as well as sex-specific life history parameters.
*It allows for estimation of dome-shaped, as well as logistic, for each fleet, with the ability to fix selectivity parameters instead of estimating them.
*Recruitment estimation is provided for models with length or age data.
*A jittering option is provided to allow for model stability evaluation and global  away from starting values.
*Simple model sensitivity explortaion

Future features will include:
*Likelihood profile implementation
*Model sensitivity comparisons
*Bayesian uncertainty estimation

