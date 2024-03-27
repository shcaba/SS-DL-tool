# Install R packages
packages<-c("devtools","shiny","shinyjs","ggplot2","reshape2","dplyr",
"tidyr","Rcpp","rlist","viridis","shinyWidgets","shinyFiles","plyr","shinybusy",
"truncnorm","ggpubr","flextable","officer","gridExtra","wesanderson",
"data.table","adnuts","shinystan","shinyBS","gt","gtExtras","stringr","ggnewscale")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Make sure the following packages are using the most recent versions:
library(devtools)
devtools::install_github("shcaba/SSS", build_vignettes = TRUE)

install.packages("remotes")
remotes::install_github("r4ss/r4ss", ref = "try_fix_jitter_dir_issue")
remotes::install_github("chantelwetzel-noaa/HandyCode")
remotes::install_github("nwfsc-assess/nwfscDiag")