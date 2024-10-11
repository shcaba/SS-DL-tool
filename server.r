require(shiny)
require(shinyjs)
require(r4ss)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(data.table)
require(tidyr)
require(rlist)
require(viridis)
#require(sss)
require(shinyWidgets)
require(shinyFiles)
require(HandyCode)
require(nwfscDiag)
require(shinybusy)
require(truncnorm)
require(flextable)
require(officer)
require(gridExtra)
require(ggpubr)
require(grid)
require(wesanderson)
require(adnuts)
require(shinystan)
require(gt)
require(gtExtras)
require(stringr)
require(ggnewscale)
require(future)
require(parallel)
require(parallelly)
require(fs)
require(tools)
#require(SSMSE)
#require(geomtextpath)

#require(paletteer)
#require(RColorBrewer)
#require(ggthemes)
#devtools::load_all("C:/Users/Jason.Cope/Documents/Github/nwfscDiag")

source('Functions.r',local = FALSE)
source('SSS.r',local = FALSE)

theme_report <- function(base_size = 11) {

  half_line <- base_size/2
  
  theme_light(base_size = base_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = unit(0.9, "lines"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)
    )
}
theme_set(theme_report())


shinyServer(function(input, output,session) {
  useShinyjs()

 
theme_report <- function(base_size = 11) { 
 
  half_line <- base_size/2 
   
  theme_light(base_size = base_size) + 
    theme( 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.ticks.length = unit(half_line / 2.2, "pt"), 
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x = element_text(colour = "black"), 
      strip.text.y = element_text(colour = "black"), 
      panel.border = element_rect(fill = NA), 
      legend.key.size = unit(0.9, "lines"), 
      legend.key = element_rect(colour = NA, fill = NA), 
      legend.background = element_rect(colour = NA, fill = NA) 
    ) 
} 
theme_set(theme_report()) 

#################
### FUNCTIONS ###
#################
saveInputs <- function(input, bookmarkPath, bookmarkURL, session){
  session$doBookmark()
  bookmarkInputPath <- file.path(dirname(bookmarkPath), bookmarkURL, "input.rds")
 
  if(!dir.exists(dirname(file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds")))) {
    dir.create(dirname(file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds")), recursive = TRUE)
  }
 
  file.copy(from = bookmarkInputPath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds"), overwrite=TRUE)

  #save files that are being used for model run
  if(!is.null(rv.Lt$data)){
    file.copy(from = input$file1$datapath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", input$file1$name), overwrite=TRUE)
  }
  if(!is.null(rv.Ct$data)){
    file.copy(from = input$file2$datapath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", input$file2$name), overwrite=TRUE)
  }
  if(!is.null(rv.Age$data)){
    file.copy(from = input$file3$datapath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", input$file3$name), overwrite=TRUE)
  }
  if(!is.null(rv.Index$data)){
    file.copy(from = input$file4$datapath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", input$file4$name), overwrite=TRUE)
  }
 
  dir_delete(bookmarkPath) #delete bookmark created from session$doBookmark as we are just using it to create the query string and grab the filepath
}


########## Clear data files and plots ############
  rv.Lt <- reactiveValues(data = NULL,clear = FALSE)
  rv.Age <- reactiveValues(data = NULL,clear = FALSE)
  rv.Ct <- reactiveValues(data = NULL,clear = FALSE)
  rv.Index <- reactiveValues(data = NULL,clear = FALSE)
  rv.AgeErr <- reactiveValues(data = NULL,clear = FALSE)
    

########
#Reset catches
  observe({
    req(input$file2)
    req(!rv.Ct$clear)
    rv.Ct$data <- fread(input$file2$datapath,check.names=FALSE,data.table=FALSE)
    #L <- readLines(input$file2$datapath, n = 1)
    #if(grepl(";", L)) {rv.Ct$data <- read.csv2(input$file2$datapath,check.names=FALSE)}
  })

  observeEvent(input$file2, {
    rv.Ct$clear <- FALSE
  }, priority = 1000)

  observeEvent(input$reset_ct, {
    rv.Ct$data <- NULL
    rv.Ct$clear <- TRUE
    reset('file2')
  }, priority = 1000)

#Reset lengths
  observe({
    req(input$file1)
    req(!rv.Lt$clear)
    rv.Lt$data <- fread(input$file1$datapath,check.names=FALSE,data.table=FALSE)
    #L <- readLines(input$file1$datapath, n = 1)
    #rv.Lt$data <- read.csv(input$file1$datapath,check.names=FALSE)
    #if(grepl(";", L)) {rv.Lt$data <- read.csv2(input$file1$datapath,check.names=FALSE)}
  })

  observeEvent(input$file1, {
    rv.Lt$clear <- FALSE
  }, priority = 1000)

  observeEvent(input$reset_lt, {
    rv.Lt$data <- NULL
    rv.Lt$clear <- TRUE
    reset('file1')
  }, priority = 1000)

#Reset ages
  observe({
    req(input$file3)
    req(!rv.Age$clear)
    rv.Age$data <- fread(input$file3$datapath,check.names=FALSE,data.table=FALSE)
    #L <- readLines(input$file3$datapath, n = 1)
    #if(grepl(";", L)) {rv.Age$data <- read.csv2(input$file3$datapath,check.names=FALSE)}
  })

  observeEvent(input$file3, {
    rv.Age$clear <- FALSE
  }, priority = 1000)

  observeEvent(input$reset_age, {
    rv.Age$data <- NULL
    rv.Age$clear <- TRUE
    reset('file3')
  }, priority = 1000)

#Reset ageing error
  observe({
    req(input$file33)
    req(!rv.AgeErr$clear)
    rv.AgeErr$data <- fread(input$file33$datapath,check.names=FALSE,header=FALSE,data.table=FALSE)
    #L <- readLines(input$file33$datapath, n = 1)
    #if(grepl(";", L)) {rv.AgeErr$data <- read.csv2(input$file33$datapath,check.names=FALSE,header=FALSE)}
  })

  observeEvent(input$file33, {
    rv.AgeErr$clear <- FALSE
     if(!input$Ageing_error_choice){
    rv.AgeErr$data <- NULL
    rv.AgeErr$clear <- TRUE
    reset('file33')}
  }, priority = 1000)

# #  if(!is.null(input$Ageing_error_choice)){       
#   observeEvent(input$file33, {
#     if(!input$Ageing_error_choice){
#     rv.AgeErr$data <- NULL
#     rv.AgeErr$clear <- TRUE
#     reset('file33') #}
#   }, priority = 1000)
 # }


#Reset index
  observe({
    req(input$file4)
    req(!rv.Index$clear)
    rv.Index$data <- fread(input$file4$datapath,check.names=FALSE,data.table=FALSE)
    #L <- readLines(input$file4$datapath, n = 1)
    #rv.Index$data <- read.csv(input$file4$datapath,check.names=FALSE)
    #if(grepl(";", L)) {rv.Index$data <- read.csv2(input$file4$datapath,check.names=FALSE,header=FALSE)}
  })

  observeEvent(input$file4, {
    rv.Index$clear <- FALSE
  }, priority = 1000)

  observeEvent(input$reset_index, {
    rv.Index$data <- NULL
    rv.Index$clear <- TRUE
    reset('file4')
  }, priority = 1000)

#Throw an error if fleets are not consecutively represented in all loaded data sets.
observeEvent(req(any(!is.null(rv.Ct$data),!is.null(rv.Lt$data),!is.null(rv.Age$data),!is.null(rv.Index$data))),{
  ct.flt<-lt.flt<-age.flt<-index.flt<-NA
  if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
  if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
  if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
  if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]}
  
  fleets.no.negs<-unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0] #remove any negative fleets
  if(length(fleets.no.negs)!=length(seq(1:max(fleets.no.negs))))
   {
     sendSweetAlert(
        session = session,
        title = "Model Warning",
        text = "Non-consecutive fleet numbering. Check all data sets (e.g., catch, lengths, ages, indices) to make sure all fleets from 1 to the maximum fleet number are found when considered across all data sets. For instance, if you have 3 total fleets, there should not be a fleet number > 3 (e.g., 1,2,4). All fleets are not expected in each data file, just across all data files.",
        type = "warning")      
   }
})

#######

   
 
  # observeEvent(input$reset_lt, {
  #   rv.Lt$data <- NULL
  #   shinyjs::reset('file1')
  # })

  # # observeEvent(input$reset_lt, {
		# # output$Ltplot<-renderPlot({
		# # rv.Lt$data <- NULL
		# # if (is.null(rv.Lt$data)) return(NULL)  
		# # })
  # # })

  # observeEvent(input$reset_age, {
  #   rv.Age$data <- NULL
  #   shinyjs::reset('file3')
  # })

  # observeEvent(input$reset_ct, {
  #   rv.Ct$data <- NULL
  #     shinyjs::reset('file2')
  # })


#####################################################
onclick("est_LHparms",id="panel_SS_est")


observe({
shinyjs::show("Data_panel")
shinyjs::show("Bookmark_panel")
hideTab(inputId = "tabs", target = "11")
  })


#To get the ObserveEvent to work, each statement in req needs to be unique.
#This explains the workaround of ((as.numeric(input$tabs)*x)/x)<4, where x is the unique type of assessment being run
#This input allows other tabs to have different side panels.

#Switch back to data from different tabs
observeEvent(req(((as.numeric(input$tabs)*99)/99)<4), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::show("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status")

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")

        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")

        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")
        
        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")
        
        shinyjs::hide("Modeff_panel")
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
#        hideTab(inputId = "tabs", target = "3")
#        hideTab(inputId = "tabs", target = "4")
#        hideTab(inputId = "tabs", target = "5")
#        hideTab(inputId = "tabs", target = "6")
  })

#Reset when all things are clicked off
observeEvent(req(((as.numeric(input$tabs)*1)/1)<4&is.null(rv.Lt$data)&is.null(rv.Ct$data)&is.null(rv.Age$data)&is.null(rv.Index$data)&any(is.null(input$user_model),!input$user_model)), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::show("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status")

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")

        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")

        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")
        
        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")
  
        shinyjs::hide("Modeff_panel")
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
  })

#User chosen model
observeEvent(req(!is.null(input$user_model)&input$user_model), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::show("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::show("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status")

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::show("panel_SS_jitter")

        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("panel_advanced_SS")
        shinyjs::show("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")
        
        shinyjs::show("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::show("run_SS")

        shinyjs::hide("Modeff_panel")
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        #shinyjs::show("tab_sss")
        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")

})

#SSS panels
observeEvent(req(((as.numeric(input$tabs)*1)/1)<4&is.null(rv.Lt$data)&!is.null(rv.Ct$data)&is.null(rv.Age$data)&is.null(rv.Index$data)&any(is.null(input$user_model),!input$user_model)), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::show("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::show("panel_SS_stock_status")

        shinyjs::show("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::show("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")

        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::show("panel_Mod_dims")

        shinyjs::show("panel_SSS_reps")

        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::show("panel_advanced_SSS")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")
        
        shinyjs::show("SaveSession_panel")

        shinyjs::show("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::hide("Modeff_panel")
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        #shinyjs::show("tab_sss")
        showTab(inputId = "tabs", target = "11")
        hideTab(inputId = "tabs", target = "2")

})

#SS-LO panels
observeEvent(req(((as.numeric(input$tabs)*2)/2)<4&all(!is.null(c(rv.Lt$data,rv.Age$data)),is.null(rv.Ct$data))&any(is.null(input$user_model),!input$user_model)), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::show("panel_Ct_F_LO")
        shinyjs::show("panel_data_wt_lt")
        if(length(unique(rv.Lt$data[,3]))>1|length(unique(rv.Age$data[,3]))>1){shinyjs::show("panel_ct_wt_LO")}
        if(length(unique(rv.Lt$data[,3]))==1|length(unique(rv.Age$data[,3]))==1){shinyjs::hide("panel_ct_wt_LO")}
        #if(input$Ct_F_LO_select){shinyjs::show("panel_ct_wt_LO")}
        #if(input$Ct_F_LO_select==NULL){shinyjs::hide("panel_ct_wt_LO")}
        shinyjs::hide("panel_SSS")
        shinyjs::show("panel_SSLO_LH")
        shinyjs::show("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")
        shinyjs::hide("Male_parms_est")
        shinyjs::hide("Male_parms_est_noWL")

        shinyjs::hide("panel_SS_stock_status")

        shinyjs::hide("panel_SSS_prod")
        shinyjs::show("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::show("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::show("panel_SS_recdevs")

        shinyjs::show("panel_SS_jitter")

        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::show("panel_Mod_dims")

        shinyjs::show("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")
   
        shinyjs::show("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::show("run_SS")

        shinyjs::hide("Modeff_panel")
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
       # showTab(inputId = "tabs", target = "3")
       # showTab(inputId = "tabs", target = "4")
       # hideTab(inputId = "tabs", target = "5")
       # showTab(inputId = "tabs", target = "6")
  })	


#SS-CL fixed parameters
observeEvent(req(((as.numeric(input$tabs)*3)/3)<4&all(any(input$est_parms==FALSE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data)),all(!is.null(rv.Index$data),!is.null(rv.Ct$data))))&any(is.null(input$user_model),!input$user_model)), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::show("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        if(any(!is.null(rv.Lt$data),!is.null(rv.Age$data))){shinyjs::show("panel_data_wt_lt")}
        else (shinyjs::hide("panel_data_wt_lt"))
        shinyjs::hide("panel_ct_wt_LO")
         
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::show("panel_SS_LH_fixed_est_tog")
        shinyjs::show("panel_SS_LH_fixed")
        shinyjs::show("panel_SS_fixed")
        shinyjs::show("panel_SS_fixed_ltwtfec")
        shinyjs::show("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::show("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::show("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::show("panel_SS_recdevs")

        shinyjs::show("panel_SS_jitter")        
   
        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::show("panel_Mod_dims")

        shinyjs::show("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::show("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::show("run_SS")

        shinyjs::hide("Modeff_panel")  
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

       #shinyjs::hide(selector = "#navbar li a[data-value=11]")
       hideTab(inputId = "tabs", target = "11")
       showTab(inputId = "tabs", target = "2")
      # show(selector = '#hello li a[data-value="2"]')
       #show(selector = '#hello li a[data-value="2"]')
       # showTab(inputId = "tabs", target = "2")
       # showTab(inputId = "tabs", target = "3")
       # showTab(inputId = "tabs", target = "4")
       # showTab(inputId = "tabs", target = "5")
       # showTab(inputId = "tabs", target = "6")
        shinyjs::hide("panel_SSLO_LH")
   })

#SS-CL with parameter estimates
observeEvent(req(((as.numeric(input$tabs)*4)/4)<4&all(input$est_parms==TRUE,any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data)),all(!is.null(rv.Index$data),!is.null(rv.Ct$data))))&any(is.null(input$user_model),!input$user_model)), {
        shinyjs::show("Bookmark_panel")
        shinyjs::show("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::show("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        if(any(!is.null(rv.Lt$data),!is.null(rv.Age$data))){shinyjs::show("panel_data_wt_lt")}
        else (shinyjs::hide("panel_data_wt_lt"))
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::show("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::show("panel_SS_LH_est")
        shinyjs::show("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::show("panel_SS_prod_est")

        shinyjs::show("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::show("panel_SS_recdevs")

        shinyjs::show("panel_SS_jitter")        
   
        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::show("panel_Mod_dims")
        
        shinyjs::show("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::show("SaveSession_panel")
        
        shinyjs::hide("run_SSS")
        shinyjs::show("run_SS")

        shinyjs::hide("Modeff_panel")  
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
        # showTab(inputId = "tabs", target = "3")
        # showTab(inputId = "tabs", target = "4")
        # showTab(inputId = "tabs", target = "5")
        # showTab(inputId = "tabs", target = "6")
   })

#Model Efficiency
observeEvent(req((as.numeric(input$tabs)*12/12)==12), {
        shinyjs::hide("Bookmark_panel")
        shinyjs::hide("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::show("Modeff_panel")  

        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
        # showTab(inputId = "tabs", target = "3")
        # showTab(inputId = "tabs", target = "4")
        # showTab(inputId = "tabs", target = "5")
        # showTab(inputId = "tabs", target = "6")
   })

#Profiles
observeEvent(req((as.numeric(input$tabs)*4/4)==4), {
        shinyjs::hide("Bookmark_panel")
        shinyjs::hide("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::hide("Modeff_panel")  
        shinyjs::show("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
        # showTab(inputId = "tabs", target = "3")
        # showTab(inputId = "tabs", target = "4")
        # showTab(inputId = "tabs", target = "5")
        # showTab(inputId = "tabs", target = "6")
   })

#Retrospecitves
observeEvent(req((as.numeric(input$tabs)*5/5)==5), {
        shinyjs::hide("Bookmark_panel")
        shinyjs::hide("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::hide("Modeff_panel")  
        shinyjs::hide("Profile_panel")
        shinyjs::show("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
        # showTab(inputId = "tabs", target = "3")
        # showTab(inputId = "tabs", target = "4")
        # showTab(inputId = "tabs", target = "5")
        # showTab(inputId = "tabs", target = "6")
   })

#Sensitivities
observeEvent(req((as.numeric(input$tabs)*6/6)==6), {
        shinyjs::hide("Bookmark_panel")
        shinyjs::hide("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::hide("Modeff_panel")  
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::show("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
        # showTab(inputId = "tabs", target = "3")
        # showTab(inputId = "tabs", target = "4")
        # showTab(inputId = "tabs", target = "5")
        # showTab(inputId = "tabs", target = "6")
   })

#Ensembles
observeEvent(req((as.numeric(input$tabs)*7/7)==7), {
        shinyjs::hide("Bookmark_panel")
        shinyjs::hide("Data_panel")
        shinyjs::hide("Existing_files")
        shinyjs::hide("panel_eqct")
        shinyjs::hide("panel_Ct_F_LO")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_fixed_ltwtfec")
        shinyjs::hide("Male_parms_fix")
        shinyjs::hide("Male_parms_fix_noWL")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")
        shinyjs::hide("panel_selectivity_sss")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("panel_advanced_SS")
        shinyjs::hide("panel_advanced_user_SS")
        shinyjs::hide("panel_advanced_SSS")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("SaveSession_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::hide("Modeff_panel")  
        shinyjs::hide("Profile_panel")
        shinyjs::hide("Retro_panel")
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::show("Ensemble_panel")

        hideTab(inputId = "tabs", target = "11")
        showTab(inputId = "tabs", target = "2")
        # showTab(inputId = "tabs", target = "3")
        # showTab(inputId = "tabs", target = "4")
        # showTab(inputId = "tabs", target = "5")
        # showTab(inputId = "tabs", target = "6")
   })


########################################

#############################
######### UI INPUTS #########
#############################

# User activated pop-up parameter values ---------------
#Model dimensions
output$Model_dims1 <- renderUI({ 
        inFile1 = rv.Lt$data 
        inFile2 = rv.Ct$data 
        inFile3 = rv.Age$data 
        #No file inputs
        if (is.null(inFile1) & is.null(inFile2) & is.null(inFile3)) return(NULL) 
        #If have lengths and/or ages, but no catches 
        if (any(!is.null(inFile1), !is.null(inFile3))& is.null(inFile2)){ 
              styr.in =  min(inFile1[,1],inFile3[,1]) 
              endyr.in = max(inFile1[,1],inFile3[,1])
  #            if(!(anyNA(c(Linf(), k_vbgf(),t0_vbgf())))& input$Ct_F_LO_select=="Constant Catch"){ 
  #              styr.in = min(inFile1[,1],inFile3[,1])-round(VBGF.age(Linf(), k_vbgf(), t0_vbgf(), Linf()*0.95)) 
  #            }
          }
          #If have catches
          if (!is.null(inFile2)){
            styr.in<-min(inFile2[,1])
            endyr.in<-max(inFile2[,1])
          }
          #If lengths or ages with catches     
          if (!is.null(inFile1) &!is.null(inFile2)|!is.null(inFile3) &!is.null(inFile2)){
            styr.in<-min(inFile1[,1],inFile2[,1],inFile3[,1])
            endyr.in<-max(inFile1[,1],inFile2[,1],inFile3[,1])
          }     
            fluidRow(column(width=4, numericInput("styr", "Starting year",  
                                                  value=styr.in, min=1, max=10000, step=1)), 
                    column(width=4, numericInput("endyr","Ending year",  
                                                 value=endyr.in, min=1, max=10000, step=1)))            
             

        # if (!is.null(inFile2)){            
        #       fluidRow(column(width=4, numericInput("styr", "Starting year",  
        #                                            value=min(inFile2[,1]), min=1, max=10000, step=1)), 
        #                 column(width=4, numericInput("endyr", "Ending year",  
        #                                         value=max(inFile2[,1]), min=1, max=10000, step=1)))            
        #     }
  #         print(styr.in)
  #         print(endyr.in)
             
    })


output$Model_dims2 <- renderUI({ 
        Ct.data = rv.Ct$data
#        if (is.null(Ct.data)) return(NULL) 
        if (!is.null(Ct.data)){            
              fluidRow(column(width=4, numericInput("styr", "Starting year",  
                                                   value=min(Ct.data[,1]), min=1, max=10000, step=1)), 
                        column(width=4, numericInput("endyr", "Ending year",  
                                                value=max(Ct.data[,1]), min=1, max=10000, step=1)))            
          }  
    }) 


# output$Female_parms_inputs_label <- reactive({
# if(!is.null(input$file1))
# 	{
# 		(output$Female_parms_inputs_label<- renderUI({
# 			      fluidRow(column(width=6,numericInput("Nages","Max. age", value=NA,min=1, max=1000, step=1)),
# 		             column(width=6,numericInput("M_f", "Natural mortality", value=NA,min=0, max=10000, step=0.01)))    
# 			      }))
# 		}
# })

output$Wt_fleet_Ct <- renderUI({ 
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]}    
      fluidRow(column(width=10,textInput("Wt_fleet_Ct","Relative catch values",value=paste(rep(1,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))))
})

output$Eq_Ct_fleet <- renderUI({
      fluidRow(column(width=10,textInput("Eq_Ct_fleet","Equilibrium catch by fleet",value=paste(rep(0,ncol(rv.Ct$data)-1),collapse=","))))
	}) 


#Load life history values via csv
output$LH_load_file <- renderUI({ 
    if(!is.null(input$LH_in_file)){       
      if(input$LH_in_file){
      fluidRow(column(width=12,fileInput('file14', 'Life history values file',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv'
                           )
       )))        
      }
    } 
  }) 

#Male life history parameters 
output$Male_parms_inputs_label <- renderUI({ 
	if(input$male_parms){ 
   		 h5(em("Male")) 
   			}		 
		}) 

output$Male_parms_inputs1 <- renderUI({ 
	if(input$male_parms){ 
    fluidRow(column(width=6, numericInput("M_m", "Natural mortality",  
                                          value=NA, min=0, max=10000, step=0.01)), 
            column(width=6, numericInput("Linf_m", "Asymptotic size (Linf)",  
                                         value=NA, min=0, max=10000, step=0.01)))     
		} 
	}) 

output$Male_parms_inputs2 <- renderUI({ 
	if(input$male_parms){ 
    fluidRow(column(width=6, numericInput("k_m", "Growth coefficient k",  
                                         value=NA, min=0, max=10000, step=0.01)), 
             column(width=6, numericInput("t0_m", "Age at length 0 (t0)",   
                                        value=NA, min=0, max=10000, step=0.01)))     
    	} 
	}) 

output$Male_parms_inputs3 <- renderUI({ 
	if(input$male_parms){ 
    fluidRow(column(width=6, textInput("CV_lt_m", "CV at length (young then old)", value="0.1,0.1"))) 
    	} 
	})  

output$Male_parms_inputs4 <- renderUI({ 
	if(input$male_parms){ 
      fluidRow(column(width=6, numericInput("WLa_m", "Weight-length alpha",  
                                            value=0.00001, min=0, max=10000, step=0.000000001)), 
               column(width=6, numericInput("WLb_m", "Weight-length beta",  
                                            value=3, min=0, max=10000, step=0.01)))     
    	} 
	}) 
 

output$Male_parms_inputs_label_fix <- renderUI({ 
  if(input$male_parms_fix){ 
       h5(em("Male")) 
        }    
    }) 

output$Male_parms_inputs1_fix <- renderUI({ 
  if(input$male_parms_fix){ 
    fluidRow(column(width=6, numericInput("M_m_fix", "Natural mortality",  
                                          value=NA, min=0, max=10000, step=0.01)), 
            column(width=6, numericInput("Linf_m_fix", "Asymptotic size (Linf)",  
                                         value=NA, min=0, max=10000, step=0.01)))     
    } 
  }) 

output$Male_parms_inputs2_fix <- renderUI({ 
  if(input$male_parms_fix){ 
    fluidRow(column(width=6, numericInput("k_m_fix", "Growth coefficient k",  
                                         value=NA, min=0, max=10000, step=0.01)), 
             column(width=6, numericInput("t0_m_fix", "Age at length 0 (t0)",   
                                        value=NA, min=0, max=10000, step=0.01)))     
      } 
  }) 

output$Male_parms_inputs3_fix <- renderUI({ 
  if(input$male_parms_fix){ 
    fluidRow(column(width=6, textInput("CV_lt_m_fix", "CV at length (young then old)", value="0.1,0.1"))) 
      } 
  })  

output$Male_parms_inputs4_fix <- renderUI({ 
  if(input$male_parms_fix){ 
      fluidRow(column(width=6, numericInput("WLa_m_fix", "Weight-Length alpha",  
                                            value=0.00001, min=0, max=10000, step=0.000000001)), 
               column(width=6, numericInput("WLb_m_fix", "Weight-length beta",  
                                            value=3, min=0, max=10000, step=0.01)))     
      } 
  }) 
 

output$Male_parms_inputs_label_est <- renderUI({ 
  if(input$male_parms_est){ 
       h4(em("Male")) 
        }    
    }) 

output$Male_parms_inputs_M_est <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("M_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("M_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001),
        numericInput("M_m_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("M_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("skull-crossbones"), width = "300px",label="Natural mortality"
    )
  } 
}) 

output$Male_parms_inputs_space1 <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 
output$Male_parms_inputs_space2 <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 
output$Male_parms_inputs_space3 <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 

output$Male_parms_inputs_space4 <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 

output$Male_parms_inputs_space5 <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 

output$Male_parms_inputs_Growth_label <- renderUI({
  if(input$male_parms_est){ 
    h5(strong("Growth")) 
    }
}) 

output$Male_parms_inputs_Linf_est <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("Linf_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("Linf_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001),
        numericInput("Linf_m_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("Linf_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("infinity"), width = "300px",label="Linf: Asymptotic size"
      )
  } 
}) 

output$Male_parms_inputs_k_est <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("k_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("k_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001),
        numericInput("k_m_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("k_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("ruler-horizontal"), width = "300px",label="k: VB growth coefficient"
          )
  } 
}) 

output$Male_parms_inputs_t0_est <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("t0_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("t0_m_mean", "Mean", value=0,min=-100, max=100, step=0.001),
        numericInput("t0_m_SD", "SD", value=0,min=0, max=100, step=0.001),
        numericInput("t0_m_phase", "Phase", value=-1,min=-999, max=100, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("baby-carriage"), width = "300px",label="t0: Age at size 0"
          )
  } 
}) 

output$Male_parms_inputs_CV_est_young <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("CV_lt_m_young_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("CV_lt_m_young_mean", "Mean", value=0.1,min=0, max=10000, step=0.001),
        numericInput("CV_lt_m_young_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("CV_lt_m_young_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("dice"), width = "300px",label="CV at length (young)"
          )
  } 
})

output$Male_parms_inputs_CV_est_old <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("CV_lt_m_old_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("CV_lt_m_old_mean", "Mean", value=0.1,min=0, max=10000, step=0.001),
        numericInput("CV_lt_m_old_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("CV_lt_m_old_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("dice"), width = "300px",label="CV at length (old)"
          )
  } 
})

output$Male_parms_inputs_WL_est <- renderUI({ 
  if(input$male_parms_est){ 
      fluidRow(column(width=6, numericInput("WLa_m_est", "Weight-length alpha",  
                                            value=0.00001, min=0, max=10000, step=0.000000001)), 
               column(width=6, numericInput("WLb_m_est", "Weight-length beta",  
                                            value=3, min=0, max=10000, step=0.01)))     
      } 
  }) 

  #h5(strong("M")),            
  #     fluidRow(column(width=4,style='padding:1px;',align="center", selectInput("M_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal"))),
  #             column(width=3,style='padding:2px;',align="center",numericInput("M_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001)),    
  #             column(width=3,style='padding:2px;',align="center",numericInput("M_m_SD", "SD", value=0,min=0, max=10000, step=0.001)),    
  #             column(width=2,style='padding:2px;',align="center",numericInput("M_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001)))    
  #   } 
  # }) 


# output$Male_parms_inputs_Linf_est <- renderUI({ 
#   if(input$male_parms_est){ 
#    #h5(strong("Linf")),            
#       fluidRow(column(width=4,style='padding:1px;',align="center",selectInput("Linf_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal"))),
#               column(width=3,style='padding:2px;',align="center",numericInput("Linf_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001)),    
#               column(width=3,style='padding:2px;',align="center",numericInput("Linf_m_SD", "SD", value=0,min=0, max=10000, step=0.001)),    
#               column(width=2,style='padding:2px;',align="center",numericInput("Linf_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001)))    
#        } 
#   }) 

# output$Male_parms_inputs_k_est <- renderUI({ 
#   if(input$male_parms_est){ 
#      #h5(strong("k")),            
#       fluidRow(column(width=4,style='padding:2px;',selectInput("k_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal"))),
#               column(width=3,style='padding:2px;',numericInput("k_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001)),    
#               column(width=3,style='padding:2px;',numericInput("k_m_SD", "SD", value=0,min=0, max=10000, step=0.001)),    
#               column(width=2,style='padding:2px;',align="center",numericInput("k_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001)))   
#        } 
#   }) 

# output$Male_parms_inputs_t0_est <- renderUI({ 
#   if(input$male_parms_est){ 
#   #h5(strong("t0")),            
#       fluidRow(column(width=4,style='padding:2px;',selectInput("t0_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal"))),
#               column(width=3,style='padding:2px;',numericInput("t0_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001)),    
#               column(width=3,style='padding:2px;',numericInput("t0_m_SD", "SD", value=0,min=0, max=10000, step=0.001)),    
#               column(width=2,style='padding:2px;',align="center",numericInput("t0_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001)))
#     } 
#   }) 

# output$Male_parms_inputs_CV_est <- renderUI({ 
#   if(input$male_parms_est){ 
#      #h5(strong("Length CV")),            
#       fluidRow(column(width=4,style='padding:2px;',selectInput("CV_lt_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal"))),
#               column(width=3,style='padding:2px;',numericInput("CV_lt_m_mean", "Mean", value=0.1,min=0, max=10000, step=0.001)),    
#               column(width=3,style='padding:2px;',numericInput("CV_lt_m_SD", "SD", value=0,min=0, max=10000, step=0.001)),    
#               column(width=2,style='padding:2px;',align="center",numericInput("CV_lt_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001)))
#     }   
#   }) 

#Male life history parameters
output$Male_parms_inputs_label_SSS<- renderUI({
	if(input$male_parms_SSS){
   		 h5(em("Male"))
   			}		
		})

output$Male_parms_inputs_M_SSS<- renderUI({
  if(input$male_parms_SSS){
       dropdownButton(
          selectInput("M_m_prior_sss","Prior type",c("lognormal","normal","uniform","no prior")),
          numericInput("M_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("M_m_SD_sss", "SD", value=0.44,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("skull-crossbones"), width = "300px",label="Natural mortality"
            )
    }
}) 

output$Male_parms_inputs_space1_SSS <- renderUI({
if(input$male_parms_SSS){ 
  br()
  } 
}) 
output$Male_parms_inputs_space2_SSS <- renderUI({
if(input$male_parms_SSS){ 
  br()
  } 
}) 
output$Male_parms_inputs_space3_SSS <- renderUI({
if(input$male_parms_SSS){ 
  br()
  } 
}) 
output$Male_parms_inputs_space4_SSS <- renderUI({
if(input$male_parms_SSS){ 
  br()
  } 
}) 

output$Male_parms_inputs_space5_SSS <- renderUI({
if(input$male_parms_SSS){ 
  br()
  } 
}) 

output$Male_parms_inputs_Growth_label_SSS <- renderUI({
  if(input$male_parms_SSS){ 
    h5(strong("Growth")) 
    }
}) 

output$Male_parms_inputs_Linf_SSS <- renderUI({ 
  if(input$male_parms_SSS){ 
        dropdownButton(
          selectInput("Linf_m_prior_sss","Prior type",c("no prior","normal")),
          numericInput("Linf_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("Linf_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("infinity"), width = "300px",label="Linf: Asymptotic size"
          )
    } 
}) 

output$Male_parms_inputs_k_SSS <- renderUI({ 
  if(input$male_parms_SSS){ 
    dropdownButton(
          selectInput("k_m_prior_sss","Prior type",c("no prior","normal")),
          numericInput("k_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("k_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("ruler-horizontal"), width = "300px",label="k: VB growth coefficient"
            )
    } 
}) 

output$Male_parms_inputs_t0_SSS <- renderUI({ 
  if(input$male_parms_SSS){ 
    dropdownButton(
          selectInput("t0_m_prior_sss","Prior type",c("no prior","normal")),
          numericInput("t0_m_mean_sss", "Mean", value=0,min=-100, max=100, step=0.001),
          numericInput("t0_m_SD_sss", "SD", value=0,min=0, max=1000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("baby-carriage"), width = "300px",label="t0: Age at size 0"
            )
    } 
}) 

output$Male_parms_inputs_CV_young_SSS <- renderUI({ 
  if(input$male_parms_SSS){ 
    dropdownButton(
          selectInput("CV_lt_m_young_prior_sss","Prior type",c("no prior")),
          numericInput("CV_lt_m_young_mean_sss", "Mean", value=0.1,min=0, max=10000, step=0.001),
          numericInput("CV_lt_m_young_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("dice"), width = "300px",label="CV at length"
            )
    } 
	})

output$Male_parms_inputs_CV_old_SSS <- renderUI({ 
  if(input$male_parms_SSS){ 
    dropdownButton(
          selectInput("CV_lt_m_old_prior_sss","Prior type",c("no prior")),
          numericInput("CV_lt_m_old_mean_sss", "Mean", value=0.1,min=0, max=10000, step=0.001),
          numericInput("CV_lt_m_old_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("dice"), width = "300px",label="CV at length"
            )
    } 
  })


output$Male_parms_inputs_WL_SSS<- renderUI({
	if(input$male_parms_SSS){
      fluidRow(column(width=6,numericInput("WLa_m_sss", "Weight-Length alpha", 
      										value=0.00001,min=0, max=10000, step=0.000000001)),
              column(width=6,numericInput("WLb_m_sss","Weight-length beta", 
              								value=3,min=0, max=10000, step=0.01)))    
    	}
	})

output$status_year<- renderUI({
fluidRow(column(width=6,numericInput("status_year", "Relative stock status year", value=input$endyr,min=1, max=3000, step=1)))
})

#Selectivity paramters
output$Sel_parms1 <- renderUI({ 
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]}    
    fluidRow(column(width=8, textInput("Sel50", "Length at 50% Selectivity",value=paste(rep(NA,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
            column(width=4, textInput("Sel50_phase", "Est. phase", value=paste(rep(3,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))))     
	}) 
 
output$Sel_parms2<- renderUI({ 
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]}
fluidRow(column(width=8, textInput("Selpeak", "Length at Peak Selectvity", value=paste(rep(NA,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
            	 column(width=4, textInput("Selpeak_phase", "Est. phase", value=paste(rep(3,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=",")))) 
	}) 
 
output$Sel_parms3 <- renderUI({ 
  		if(input$Sel_choice=="Dome-shaped"){ 	
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]}
    	fluidRow(column(width=8, textInput("PeakDesc", "Length at 1st declining selectivity",value=paste(rep("10000",length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
            	 column(width=4, textInput("PeakDesc_phase", "Est. phase",value=paste(rep(-3,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=",")))) 
 		} 
	}) 
 
output$Sel_parms4 <- renderUI({ 
 		if(input$Sel_choice=="Dome-shaped"){ 			 
	    ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]}
      fluidRow(column(width=8, textInput("LtPeakFinal", "Width of declining selectivity",value=paste(rep("0.0001",length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
	             column(width=4, textInput("LtPeakFinal_phase", "Est. phase",value=paste(rep(-3,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))))    			 
 		} 
	}) 
 
output$Sel_parms5 <- renderUI({ 
 		if(input$Sel_choice=="Dome-shaped"){
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]} 			 
    	fluidRow(column(width=8, textInput("FinalSel", "Selectivity at max bin size",value=paste(rep("0.99999",length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
            	column(width=4, textInput("FinalSel_phase", "Est. phase",paste(rep(-3,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=",")))) 
 		} 
	}) 

output$Sel_parms1_sss <- renderUI({ 
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]} 			 
    fluidRow(column(width=6, textInput("Sel50_sss", "Length at 50% Selectivity",value=paste(rep(NA,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
            column(width=6, textInput("Selpeak_sss", "Length at Peak Selectvity", value=paste(rep(NA,length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))))     
  }) 
 
 
output$Sel_parms2_sss <- renderUI({ 
      if(input$Sel_choice_sss=="Dome-shaped"){       
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]} 			 
      fluidRow(column(width=6, textInput("PeakDesc_sss", "Length at 1st declining selectivity",value=paste(rep("10000",length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=","))), 
               column(width=6, textInput("LtPeakFinal_sss", "Width of declining selectivity",value=paste(rep("0.0001",length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=",")))) 
    } 
  }) 
 
output$Sel_parms3_sss <- renderUI({ 
    if(input$Sel_choice_sss=="Dome-shaped"){       
      ct.flt<-lt.flt<-age.flt<-index.flt<-NA
      if(!is.null(rv.Ct$data)){ct.flt<-c(1:(ncol(rv.Ct$data)-1))}
      if(!is.null(rv.Lt$data)){lt.flt<-rv.Lt$data[,3]}
      if(!is.null(rv.Age$data)){age.flt<-rv.Age$data[,3]}
      if(!is.null(rv.Index$data)){index.flt<-rv.Index$data[,3]} 			 
      fluidRow(column(width=8, textInput("FinalSel_sss", "Selectivity at max bin size",value=paste(rep("0.99999",length(unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))[unique(na.omit(c(ct.flt,lt.flt,age.flt,index.flt)))>0])),collapse=",")))) 
    } 
  }) 
			
#Recruitment parameter inputs
output$Rec_options1 <- renderUI({ 
    if(input$rec_choice){ 
        fluidRow(column(width=6, numericInput("sigmaR", "Rec. varaibility (sR)",  
                                              value=0.5, min=0, max=10, step=0.01)))    
    	} 
	}) 
output$Rec_options2 <- renderUI({ 
    if(input$rec_choice){ 
          fluidRow(column(width=6, numericInput("Rdev_startyr", "Rec. devs. start year",  
                                                value=input$styr, min=1, max=10000, step=1)), 
                   column(width=6, numericInput("Rdev_endyr", "Rec. devs. end year",  
                                                value=input$endyr, min=1, max=10000, step=1)))     
    	} 
	}) 
 
output$Rec_options3 <- renderUI({ 
    if(input$biasC_choice){ 
          fluidRow(column(width=6, numericInput("NobiasC_early", "Early last year",  
                                                value=input$styr, min=1, max=10000, step=1)), 
                   column(width=6, numericInput("NobiasC_recent", "1st recent year",  
                                                value=input$endyr, min=1, max=10000, step=1)))     
    	} 
	}) 
 
output$Rec_options4 <- renderUI({ 
    if(input$biasC_choice){ 
          fluidRow(column(width=6, numericInput("BiasC_startyr", "Start year",  
                                                value=input$styr, min=1, max=10000, step=1)), 
                   column(width=6, numericInput("BiasC_endyr", "End year",  
                                                value=input$endyr, min=1, max=10000, step=1)))     
    	} 
	}) 
 
output$Rec_options5 <- renderUI({ 
    if(input$biasC_choice){ 
          fluidRow(column(width=6, numericInput("BiasC","Maximum bias adjustment", value=1,min=0, max=1, step=0.001))) 
    	} 
	})  

output$Rec_options6 <- renderUI({ 
    if(input$rec_choice){ 
          fluidRow(column(width=6, selectInput("RecDevChoice","Recruit deviation option",c("1: Devs sum to zero","2: Simple deviations","3: deviation vector","4: option 3 plus penalties"),selected="3: deviation vector"))) 
    	} 
	})  


#Jitter value
output$Jitter_value <- renderUI({ 
    if(input$jitter_choice){ 
        fluidRow(column(width=6, numericInput("jitter_fraction", "Jitter value",  
                                             value=0.01, min=0, max=10, step=0.001)), 
        	       column(width=6, numericInput("Njitter", "# of jitters",  
        	                                   value=0, min=1, max=10000, step=1)))    
    	} 
	}) 

#Choose reference points
output$RP_selection1<- renderUI({ 
    if(input$RP_choices){ 
        fluidRow(column(width=6, numericInput("SPR_target", "SPR target",  
                                              value=0.5, min=0, max=1, step=0.001)), 
        	       column(width=6, numericInput("B_target", "Biomass target", 
        	                                    value=0.4, min=0, max=1, step=0.001)))    
    	} 
	}) 


output$RP_selection2<- renderUI({ 
    if(input$RP_choices){ 
        fluidRow(column(width=6,selectInput("CR_Ct_F","Control rule type",
            c("1: Catch fxn of SSB, buffer on F",
              "2: F fxn of SSB, buffer on F",
              "3: Catch fxn of SSB, buffer on catch",
              "4: F fxn of SSB, buffer on catch"))),
          #column(width=4, numericInput("CR_Ct_F", "Control rule type",  
           #                                   value=1, min=0, max=1, step=0.001)), 
        	       column(width=3, numericInput("slope_hi", "Upper ratio value",  
        	                                    value=0.4, min=0, max=1, step=0.001)),    
                 column(width=3, numericInput("slope_low", "Lower ratio value",  
                                              value=0.1, min=0, max=1, step=0.001)))
    	} 
	}) 
 
output$Forecasts<- renderUI({ 
    if(input$Forecast_choice){ 
        fluidRow(column(width=6, numericInput("forecast_num", "# of forecast years",  
                                              value=2, min=1, max=1000, step=1)), 
        	       column(width=6, textInput("forecast_buffer", "Control rule buffer", value="1")))    
    	} 
	}) 


output$AdvancedSS_nohess<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "no_hess", label = "Turn off variance estimation (speeds up runs)",
        shape = "round", outline = TRUE, status = "info",value=TRUE))) 
      # } 
  }) 

output$AdvancedSS_nohess_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "no_hess_user", label = "Turn off Hessian (speeds up runs, but no variance estimation)",
        shape = "round", outline = TRUE, status = "info",value=TRUE))) 
      # } 
  }) 
  
output$AdvancedSS_addcomms<- renderUI({ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "add_comms", 
        label = "Add additional SS3 run commands",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

  output$AdvancedSS_addcomms_comms <- renderUI({ 
    if(!is.null(input$add_comms)){       
      if(input$add_comms){
      fluidRow(column(width=12, textInput("add_comms_in", "Enter additional run commands", value="")))        
  }
    }
}) 

output$AdvancedSS_addcomms_user<- renderUI({ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "add_comms_user", 
        label = "Add additional SS3 run commands",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

  output$AdvancedSS_addcomms_comms_user <- renderUI({ 
    if(!is.null(input$add_comms_user)){       
      if(input$add_comms_user){
      fluidRow(column(width=12, textInput("add_comms_in_user", "Enter additional run commands", value="")))        
  }
    }
}) 

output$AdvancedSS_noplots<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "no_plots_tables", label = "Turn off plots",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_plots_RP<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "plot_RPs", label = "Add RPs to plots?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_plots_RP_inputs<- renderUI({ 
    #if(!is.null(input$plot_RPs)){       
     # if(input$plot_RPs){
      textInput("plot_RPs_inputs", "Plot target and limit reference points", value="0.4,0.25")        
     # }
    #} 
  }) 

output$AdvancedSS_noplots_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "no_plots_tables", label = "Turn off plots",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_plots_RP_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "plot_RPs", label = "Add RPs to plots?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_plots_RP_inputs_user<- renderUI({ 
    #if(!is.null(input$plot_RPs_user)){       
    #  if(input$plot_RPs_user){
      textInput("plot_RPs_inputs", "Plot target and limit reference points", value="0.4,0.25")        
     # }
    #} 
  }) 

output$AdvancedSS_noestabs<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "no_tables", label = "No exectutive summary tables",
        shape = "round", outline = TRUE, status = "info",value=TRUE))) 
      # } 
  }) 

output$AdvancedSS_noestabs_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "no_tables", label = "No exectutive summary tables",
        shape = "round", outline = TRUE, status = "info",value=TRUE))) 
      # } 
  }) 

output$AdvancedSS_par<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_par", label = "Use par file (i.e., parameter file from previous run)?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_par_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_par", label = "Use par file (i.e., parameter file from previous run)?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_phase0<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_phase0", label = "Turn off estimation of all parameters (phase = 0)?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_phase0_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_phase0", label = "Turn off estimation of all parameters (phase = 0)?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_datanew<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_datanew", label = "Use the data_echo.ss_new file?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_datanew_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_datanew", label = "Use the data_echo.ss_new file?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_controlnew<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_controlnew", label = "Use the control.ss_new file?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_controlnew_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_controlnew", label = "Use the control.ss_new file?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_forecastnew<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_forecastnew", label = "Use the forecast.ss_new file?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_forecastnew_user<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "use_forecastnew", label = "Use the forecast.ss_new file?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_GT1<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "GT1", label = "Use only one growth type (default is 5)",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_GT5_SSS<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "GT5", label = "Use 5 growth types (default is 1)",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_Sex3options<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "Sex3options", label = "Maintain sex ratio in biological compositions",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_Sex3<- renderUI({ 
    if(!is.null(input$Sex3options)){       
      if(input$Sex3options){       
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "Sex3", label = "Length compositions",
        shape = "curve", inline = TRUE, status = "success",animation="smooth"))) 
      # } 
    }
   }
  }) 


output$AdvancedSS_AgeSex3<- renderUI({ 
    if(!is.null(input$Sex3options)){       
      if(input$Sex3options){       
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "AgeSex3", label = "Age compositions",
        shape = "curve", inline = TRUE, status = "success",animation="smooth"))) 
      # } 
    }
   }
  }) 

output$AdvancedSS_Indexvar<- renderUI({ 
    # if(input$advance_ss_click){ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "Indexvar", label = "Estimate additional variance on each abundance index?",
        shape = "round", outline = TRUE, status = "info"))) 
      # } 
  }) 

output$AdvancedSS_ageerror<- renderUI({ 
        fluidRow(column(width=12, prettyCheckbox(
        inputId = "Ageing_error_choice", label = "Add custom ageing error matrices?",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

output$AdvancedSS_ageerror_in <- renderUI({ 
    if(!is.null(input$Ageing_error_choice)){       
      if(input$Ageing_error_choice){
      #h4(strong("Choose data file")),
      fluidRow(column(width=12,fileInput('file33', 'Ageing error file',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv'
                           )
       )))        
      }
    } 
  }) 

output$AdvancedSS_Ctunits<- renderUI({ 
        fluidRow(column(width=12, prettyCheckbox(
        inputId = "Ct_units_choice", label = "Specify non-biomass catch units for each fleet.",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

output$AdvancedSS_Ctunitsfleets <- renderUI({ 
    if(!is.null(input$Ct_units_choice)){       
      if(input$Ct_units_choice){
      fluidRow(column(width=12, textInput("fleet_ct_units", "Enter catch units for each fleet", value="")))        
      }
    } 
  }) 

output$AdvancedSS_Ctunits_SSS<- renderUI({ 
        fluidRow(column(width=12, prettyCheckbox(
        inputId = "Ct_units_choice_SSS", label = "Specify catch units for each fleet?",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

output$AdvancedSS_Ctunitsfleets_SSS<- renderUI({ 
    if(!is.null(input$Ct_units_choice_SSS)){       
      if(input$Ct_units_choice_SSS){
      fluidRow(column(width=12, textInput("fleet_ct_units_SSS", "Enter catch units for each fleet (1=biomass; 2=numbers)", value="")))        
      }
    } 
  }) 

output$AdvancedSS_retro_choice<- renderUI({ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "Retro_choice", label = "Do retrospective runs? Input minus from current year",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

output$AdvancedSS_retro_years <- renderUI({ 
    if(!is.null(input$Retro_choice)){       
    if(input$Retro_choice){       
      fluidRow(column(width=6, numericInput("first_retro_year", "1st retro year",  
                                              value=-1, min=-1, max=-500, step=-1)), 
              column(width=6, numericInput("final_retro_year", "Last retro year",  
                                              value=-10, min=-1, max=-500, step=-1)))
      }
    } 
  }) 

output$AdvancedSS_retro_choice_user <- renderUI({ 
        fluidRow(column(width=6, prettyCheckbox(
        inputId = "Retro_choice", label = "Do retrospective runs? Input minus from current year",
        shape = "round", outline = TRUE, status = "info"))) 
  }) 

output$AdvancedSS_retro_years_user <- renderUI({ 
    if(!is.null(input$Retro_choice)){       
    if(input$Retro_choice){       
      fluidRow(column(width=6, numericInput("first_retro_year", "1st retro year",  
                                              value=-1, min=-1, max=-500, step=-1)), 
              column(width=6, numericInput("final_retro_year", "Last retro year",  
                                              value=-10, min=-1, max=-500, step=-1)))
      }
    } 
  }) 

output$AdvancedSS_Ltbin <- renderUI({ 
    # if(input$advance_ss_click){       
      if(!is.null(rv.Lt$data)){bin.step<-as.numeric(colnames(rv.Lt$data)[7])-as.numeric(colnames(rv.Lt$data)[6])}
      if(is.null(rv.Lt$data)){bin.step<-2}
      fluidRow(column(width=4, numericInput("lt_bin_size", "bin size",  
                                              value=bin.step, min=0, max=10000, step=1)), 
              column(width=4, numericInput("lt_min_bin", "minimum bin",  
                                              value=4, min=0, max=10000, step=0.01)), 
              column(width=4, numericInput("lt_max_bin", "maximum bin",  
                                              value=2*(round((Linf()+(Linf()*0.2326))/2))+2, min=0, max=10000, step=0.01))) 
    # } 
  }) 

output$AdvancedSSS_Nages <- renderUI({ 
      fluidRow(column(width=6, numericInput("Nages_in_sss", "Plus group age",  
                                              value=Nages(), min=0, max=10000, step=0.5))) 
  }) 

output$AdvancedSS_Nages <- renderUI({ 
      fluidRow(column(width=6, numericInput("Nages_in", "Plus group age",  
                                              value=Nages(), min=0, max=10000, step=0.5))) 
  }) 


output$Profile_multi_values <- renderUI({ 
    #if(!is.null(input$multi_profile)){       
    #  if(input$multi_profile){
      #h4(strong("Choose data file")),
      fluidRow(column(width=12,fileInput('file_multi_profile', 'Profile input values',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv'
                           )
       )))        
    #   }
    # } 
  }) 

output$scenario_description_text <- renderUI({ 
    if(input$scenario_description){ 
        fluidRow(column(width=12, textAreaInput("scenario_description_input", strong("Write a description of your scenario in the text box below. This will go in the comment section at the top of your control and data files."), value="")))
    	}
	})

#  roots <- getVolumes()()  

###############################################
###############################################

###############################################
################# PARAMETERS ##################
###############################################

FleetNs<-reactive({
    if(all(c(is.null(rv.Ct$data[,2],rv.Lt$data[,3],rv.Age$data[,3],rv.Index$data[,3])))) return(NULL)
    fleetnum<-rep(1,max(rv.Lt$data[,3],rv.Age$data[,3],rv.Index$data[,3]))
    FleetNs<-paste(as.character(fleetnum), collapse=",")
    #print(FleetNs)
    FleetNs
  })


Nages<-reactive({
    Nages<-NA
    if(all(c(is.null(input$M_f),is.null(input$M_f_fix),is.null(input$M_f_mean),is.null(input$M_f_mean_sss),is.null(rv.Age$data)))) return(NULL)
    if(!is.na(input$M_f)) {Nages<-ceiling(5.4/input$M_f)}
    if(!is.na(input$M_f_fix)) {Nages<-ceiling(5.4/input$M_f_fix)}
    if(!is.na(input$M_f_mean)) {Nages<-ceiling(5.4/input$M_f_mean)}
    if(!is.na(input$M_f_mean_sss)) {Nages<-ceiling(5.4/input$M_f_mean_sss)} 
    if(!is.null(rv.Age$data))
    {
     Nages_in<-max(as.numeric(colnames(rv.Age$data[,9:ncol(rv.Age$data)])))
     if(!is.na(Nages)&Nages_in>Nages){Nages<-Nages_in}
     if(is.na(Nages)){Nages<-Nages_in}
    }
    Nages
  })

M_f_in<-reactive({
    M_f_in<-NA
    if(all(c(is.null(input$M_f),is.null(input$M_f_fix),is.null(input$M_f_mean),is.null(input$M_f_mean_sss)))) return(NULL)
    if(!is.na(input$M_f)) {M_f_in<-input$M_f}
    if(!is.na(input$M_f_fix)) {M_f_in<-input$M_f_fix}
    if(!is.na(input$M_f_mean)) {M_f_in<-input$M_f_mean}
    if(!is.na(input$M_f_mean_sss)) {M_f_in<-input$M_f_mean_sss}
    M_f_in
  })

M_m_in<-reactive({
    M_m_in<-NA
    if(all(c(is.null(input$M_m),is.null(input$M_m_fix),is.null(input$M_m_mean),is.null(input$M_m_mean_sss)))) return(NULL)
    if(any(input$male_parms&!is.na(input$M_m))) {M_m_in<-input$M_m}
    if(any(input$male_parms_fix&!is.na(input$M_m_fix))) {M_m_in<-input$M_m_fix}
    if(any(input$male_parms_est&!is.na(input$M_m_mean))) {M_m_in<-input$M_m_mean}
    if(any(input$male_parms_SSS&!is.na(input$M_m_mean_sss))) {M_m_in<-input$M_m_mean_sss}
    M_m_in
  })


Linf<-reactive({
    Linf<-NA
    if(all(c(is.null(input$Linf_f),is.null(input$Linf_f_fix),is.null(input$Linf_f_mean),is.null(input$Linf_f_mean_sss)))) return(NULL)
    if(!is.na(input$Linf_f)) {Linf<-input$Linf_f}
    if(!is.na(input$Linf_f_fix)) {Linf<-input$Linf_f_fix}
    if(!is.na(input$Linf_f_mean)) {Linf<-input$Linf_f_mean}
    if(!is.na(input$Linf_f_mean_sss)) {Linf<-input$Linf_f_mean_sss}
    Linf
  })

Linf_m_in<-reactive({
    Linf_m_in<-NA
    #if(all(c(is.null(input$Linf_m),is.null(input$Linf_m_fix),is.null(input$Linf_m_mean),is.null(input$Linf_m_mean_sss)))) return(NULL)
    if(any(input$male_parms&!is.na(input$Linf_m))) {Linf_m_in<-input$Linf_m}
    if(any(input$male_parms_fix&!is.na(input$Linf_m_fix))) {Linf_m_in<-input$Linf_m_fix}
    if(any(input$male_parms_est&!is.na(input$Linf_m_mean))) {Linf_m_in<-input$Linf_m_mean}
    if(any(input$male_parms_SSS&!is.na(input$Linf_m_mean_sss))) {Linf_m_in<-input$Linf_m_mean_sss}
    Linf_m_in
  })


k_vbgf<-reactive({
    k_vbgf<-NA
    if(all(c(is.null(input$k_f),is.null(input$k_f_fix),is.null(input$k_f_mean),is.null(input$k_f_mean_sss)))) return(NULL)
    if(!is.na(input$k_f)) {k_vbgf<-input$k_f}
    if(!is.na(input$k_f_fix)) {k_vbgf<-input$k_f_fix}
    if(!is.na(input$k_f_mean)) {k_vbgf<-input$k_f_mean}
    if(!is.na(input$k_f_mean_sss)) {k_vbgf<-input$k_f_mean_sss}
    k_vbgf
  })

#Process life history input for plots
k_vbgf_m_in<-reactive({
    k_vbgf_m_in<-NA
    if(all(c(is.null(input$k_m),is.null(input$k_m_fix),is.null(input$k_m_mean),is.null(input$k_m_mean_sss)))) return(NULL)
    if(any(input$male_parms&!is.na(input$k_m))) {k_vbgf_m_in<-input$k_m}
    if(any(input$male_parms_fix&!is.na(input$k_m_fix))) {k_vbgf_m_in<-input$k_m_fix}
    if(any(input$male_parms_est&!is.na(input$k_m_mean))) {k_vbgf_m_in<-input$k_m_mean}
    if(any(input$male_parms_SSS&!is.na(input$k_m_mean_sss))) {k_vbgf_m_in<-input$k_m_mean_sss}
    k_vbgf_m_in
  })

t0_vbgf<-reactive({
    t0_vbgf<-NA
    if(all(c(is.null(input$t0_f),is.null(input$t0_f_fix),is.null(input$t0_f_mean),is.null(input$t0_f_mean_sss)))) return(NULL)
    if(!is.na(input$t0_f)) {t0_vbgf<-input$t0_f}
    if(!is.na(input$t0_f_fix)) {t0_vbgf<-input$t0_f_fix}
    if(!is.na(input$t0_f_mean)) {t0_vbgf<-input$t0_f_mean}
    if(!is.na(input$t0_f_mean_sss)) {t0_vbgf<-input$t0_f_mean_sss}
    t0_vbgf
  })

t0_vbgf_m_in<-reactive({
    t0_vbgf_m_in<-NA
    if(all(c(is.null(input$t0_m),is.null(input$t0_m_fix),is.null(input$t0_m_mean),is.null(input$t0_m_mean_sss)))) return(NULL)
    if(any(input$male_parms&!is.na(input$t0_m))) {t0_vbgf_m_in<-input$t0_m}
    if(any(input$male_parms_fix&!is.na(input$t0_m_fix))) {t0_vbgf_m_in<-input$t0_m_fix}
    if(any(input$male_parms_est&!is.na(input$t0_m_mean))) {t0_vbgf_m_in<-input$t0_m_mean}
    if(any(input$male_parms_SSS&!is.na(input$t0_m_mean_sss))) {t0_vbgf_m_in<-input$t0_m_mean_sss}
    t0_vbgf_m_in
  })

L50<-reactive({
    L50<-NA
    if(all(c(is.null(input$L50_f),is.null(input$L50_f_fix),is.null(input$L50_f_est),is.null(input$L50_f_sss)))) return(NULL)
    if(!is.na(input$L50_f)) {L50<-input$L50_f}
    if(!is.na(input$L50_f_fix)) {L50<-input$L50_f_fix}
    if(!is.na(input$L50_f_est)) {L50<-input$L50_f_est}
    if(!is.na(input$L50_f_sss)) {L50<-input$L50_f_sss}
    L50
  })

L95<-reactive({
    L95<-NA
    if(all(c(is.null(input$L95_f),is.null(input$L95_f_fix),is.null(input$L95_f_est),is.null(input$L95_f_sss)))) return(NULL)
    if(!is.na(input$L95_f)) {L95<-input$L95_f}
    if(!is.na(input$L95_f_fix)) {L95<-input$L95_f_fix}
    if(!is.na(input$L95_f_est)) {L95<-input$L95_f_est}
    if(!is.na(input$L95_f_sss)) {L95<-input$L95_f_sss}
    L95
  })

LatAmax<-reactive({
    LatAmax<-NA
    if(all(c(is.null(M_f_in()),is.null(Linf()),is.null(k_vbgf()),is.null(t0_vbgf)))) return(NULL)
    if(all(c(!is.na(M_f_in()),!is.na(Linf()),!is.na(k_vbgf()),!is.na(t0_vbgf())))) {LatAmax<-VBGF(Linf(),k_vbgf(),t0_vbgf(),5.4/M_f_in())}
    LatAmax
  })

lt.bins.sels<-reactive({
  Lt.dat.sel<-rv.Lt$data
  lt.bins.freq<-Lt.dat.sel[,6:ncol(Lt.dat.sel)]
  lt.bins.out<-as.numeric(colnames(lt.bins.freq))
  Sel_mode.out<-mapply(function(x) lt.bins.out[as.numeric(lt.bins.freq[x,])==max(as.numeric(lt.bins.freq[x,]))][1],x=1:nrow(lt.bins.freq))
  Sel_init.out<-mapply(function(x) lt.bins.out[as.numeric(lt.bins.freq[x,])>0][1],x=1:nrow(lt.bins.freq))
  Sel_max.out<-mapply(function(x) max(lt.bins.out[lt.bins.freq[x,]>0]),x=1:nrow(lt.bins.freq))
  lt.bins.sel50<-lt.bins.sel95<-lt.bins.selmax<-Lt.dat.sel[,1:5]
  lt.bins.sel50$Sel<-(Sel_init.out+Sel_mode.out)/2
  lt.bins.sel50$SelType<-"Sel50"
  lt.bins.sel95$Sel<-Sel_mode.out
  lt.bins.sel95$SelType<-"Sel95"
  lt.bins.selmax$Sel<-Sel_max.out
  lt.bins.selmax$SelType<-"Last Bin>0"
  lt.bins.sels<-rbind(lt.bins.sel50,lt.bins.sel95,lt.bins.selmax)
  lt.bins.sels
 })

#############
### PLOTS ###
#############

##################
### CATCH PLOT ###
##################
 observeEvent(req(!is.null(rv.Ct$data)), {
      shinyjs::show(output$catch_plots_label<-renderText({"Removal history"}))
  })

  observeEvent(req(!is.null(rv.Ct$data)), {
  output$Ctplot_it<-renderUI({
  if(!is.null(rv.Ct$data))
  {
    output$Ctplot <- renderPlot({ 
    if (is.null(rv.Ct$data)) return(NULL) 
    rv.Ct$data %>%  
        pivot_longer(-1, names_to = "Fleet", values_to = "catch") %>%  
        ggplot(aes_string(names(.)[1], "catch", color = "Fleet")) +  
        geom_point() +  
        geom_line(lwd=1.5) + 
        ylab("Removals") + 
        xlab("Year") +  
        scale_color_viridis_d() 
      })
    plotOutput("Ctplot")
  }
    })
  })

##########################
### LENGTH COMPS PLOTS ###
########################## 
observeEvent(req(!is.null(rv.Lt$data)), {
      shinyjs::show(output$lt_comp_plots_label<-renderText({"Length compositions. Sex categories: unknown sex = 0, females = 1 and males = 2."}))
  })

observeEvent(req(!is.null(rv.Lt$data)), {
      shinyjs::show(output$lt_comp_sel_plots_label<-renderText({"Suggested selectivity starting values based on length compositions by year. Lines are loess smoothers. Sel95 is based on the mode of the composition; Sel50 is based on the midpoint of the mode and first size of recorded lengths. Changes in values over time could be indicative of changes in selectivity, recruitment events, sampling representativeness, or other factors, and should be thought through when specifying selectivity for each fleet. These plots are informative to the behavior of the ascending limb of selectivity. More information is needed to specify the possibility of a descending limb (and thus form of the selectivity)."}))
  })

observeEvent(req(!is.null(rv.Lt$data)), {
output$Ltplot_it<-renderUI({
if(!is.null(rv.Lt$data))
{  
  output$Ltplot<-renderPlot({ 
		  if (is.null(rv.Lt$data)) return(NULL) 
		  #  if(!is.null(rv.Ct$data))
      #  {
      #   fleetnames.ct<-colnames(rv.Ct$data)[-1]
      #   for(xx in 1:length(fleetnames.ct))
      #   {
      #     rv.Lt$data$Fleet[rv.Lt$data$Fleet==xx]<-fleetnames.ct[xx]
      #   }
      #  }
       
       Lt.dat.plot<-rv.Lt$data %>%  
		    rename_all(tolower) %>%  
		    dplyr::select(-nsamps) %>%  
		    pivot_longer(c(-year, -fleet, -sex)) %>%  
		    mutate(Year = factor(year),
		           name = as.numeric(gsub("[^0-9.-]", "", name)))
       Lt.dat.plot.Linf<-data.frame(year=1,expand.grid(unique(Lt.dat.plot$fleet),unique(Lt.dat.plot$sex)),name=00,value=00,Year=1,vline=-100,Linf.lab="Linf")
       Lt.dat.plot.L50<-data.frame(year=1,expand.grid(unique(Lt.dat.plot$fleet),unique(Lt.dat.plot$sex)),name=00,value=00,Year=1,vline=-100,Linf.lab="L50")
       Lt.dat.plot.LAmax<-data.frame(year=1,expand.grid(unique(Lt.dat.plot$fleet),unique(Lt.dat.plot$sex)),name=00,value=00,Year=1,vline=-100,Linf.lab="LtAmax")
       colnames(Lt.dat.plot.L50)<-colnames(Lt.dat.plot.Linf)<-colnames(Lt.dat.plot.LAmax)<-c("year","fleet","sex","name","value","Year","vline","Label")
       if(!is.na(Linf())){Lt.dat.plot.Linf$vline[Lt.dat.plot.Linf$sex==0|Lt.dat.plot.Linf$sex==1]<-Linf()}
       if(!is.na(Linf())){Lt.dat.plot.Linf$vline[Lt.dat.plot.Linf$sex==2]<-Linf_m_in()}
       if(!is.na(L50()))Lt.dat.plot.L50$vline[Lt.dat.plot.L50$sex==0|Lt.dat.plot.L50$sex==1]<-L50()
       if(!is.na(LatAmax()))Lt.dat.plot.LAmax$vline[Lt.dat.plot.LAmax$sex==0|Lt.dat.plot.L50$sex==1]<-LatAmax()
         
        ggplot(Lt.dat.plot) + 
		    geom_line(aes(name, value, color=Year)) + 
        #geom_col(position="dodge") + 
		    geom_vline(data=Lt.dat.plot.L50,
                    aes(xintercept = vline,linetype="L50"),
                    colour = "darkgreen",
                    na.rm = TRUE)+
                    scale_linetype_manual(name = NULL, values = 3)+
        new_scale("linetype") +
        geom_vline(data=Lt.dat.plot.Linf,
                    aes(xintercept = vline,linetype="Linf"),
                    colour = "#d26678",
                    na.rm = TRUE)+
                    scale_linetype_manual(name = NULL, values = 1)+
        new_scale("linetype") +
        geom_vline(data=Lt.dat.plot.LAmax,
                    aes(xintercept = vline,linetype="LatAmax"),
                    colour = "black",
                    na.rm = TRUE)+
                    scale_linetype_manual(name = NULL, values = 2)+
        facet_grid(sex~fleet, scales="free_y",labeller = label_both) + 
#        annotate("text",x=if_else(is.na(Linf()),-1,Linf())*1.05, y=5, label= "Linf",color="#d26678")+
#        annotate("text",x=if_else(is.na(Linf_m_in()),-1,Linf_m_in())*1.05, y=5, label= "Linf",color="blue")+
#        annotate("text",x=if_else(is.na(L50()),-1,L50())*1.1, y=5, label= "L50%",color="darkgreen")+
        xlab("Length bin") + 
		    ylab("Frequency") + 
		    scale_fill_viridis_d()+
        xlim(0,NA)
		}) 
      plotOutput("Ltplot")
    }
  })
	    output$Ltplot_it_sel<-renderUI({
    if(!is.null(rv.Lt$data))
    {  
       lt.bins.sels.in<-lt.bins.sels()
       lt.bins.sels.in$L50<--10
       lt.bins.sels.in$Linf<--10
       output$LtSelplot<-renderPlot({ 
        if (is.null(rv.Lt$data)) return(NULL)

          LtSelplot.out<-ggplot(lt.bins.sels.in)+ 
          geom_point(aes(Year,Sel,col=SelType))+
          guides(color = guide_legend(title = " ")) +
          geom_smooth(method=loess,se=FALSE,aes(Year,Sel,col=SelType))+		    
         facet_grid(~Fleet, scales="free_y",labeller = label_both)+
            xlab("Year") + 
            ylab("Size (cm)") + 
            scale_fill_viridis_d()+
            ylim(min(lt.bins.sels.in$Sel),NA)
 
#        if(!is.na(Linf())){
#         
#         lt.bins.sels.in$Linf[lt.bins.sels.in$Sex==0|lt.bins.sels.in$Sex==1]<-Linf()
#         lt.bins.sels.in$Linf[lt.bins.sels.in$Sex==2]<-Linf_m_in()
#         LtSelplot.out<-geom_hline(data=lt.bins.sels.in,
#                     aes(yintercept = Linf),
#                     colour = "darkgreen",
#                     na.rm = TRUE)
#  #                   scale_linetype_manual(name = NULL, values = 3)
#         }
#         if(!is.na(L50()))
#         {
#           lt.bins.sels.in$L50[lt.bins.sels.in$Sex==0|lt.bins.sels.in$Sex==1]<-L50()
#         geom_hline(data=lt.bins.sels.in,
#                     aes(yintercept = L50),
#                     colour = "#d26678",
#                     na.rm = TRUE)
#                     #scale_linetype_manual(name = NULL, values = 1)
#         }
 
#        new_scale("linetype") +
        
 
          #   if(!is.na(L50())){LtSelplot.out<-LtSelplot.out+geom_hline(yintercept = L50(),
          #           colour = "darkgreen",lty=1,linetype="dashed")+
          #           annotate("text", x = min(lt.bins.sels()$Year)+1, y = L50(), label = "Lmat50", hjust = 1)}
           return(LtSelplot.out)
      })
    }
      })
  })

# observeEvent(req(!is.null(input$file1)), {
# 		output$Ltplot<-renderPlot({
# 		inFile<- input$file1
# 		# if (is.null(inFile)) {
# 		# 	return(NULL) 
# 		# 	shinyjs::hide("Ltplot")} 
# 		# else{
# 		Lt.comp.data<-read.csv(inFile$datapath,check.names=FALSE)
# 		lt.dat.plot<-(Lt.comp.data)[,c(-4)]
# 		dat.gg<-melt(lt.dat.plot,id=colnames(lt.dat.plot)[1:3])
# 		colnames(dat.gg)<-c("year","fleet","sex","bin","ltnum")
# 		ggplot(dat.gg,aes(bin,ltnum,fill=factor(fleet)))+
# 					geom_col(color="white",position="dodge")+
# 		 			#geom_col(fill="#236192",color="white")+
# 					facet_wrap(~year,scales="free_y")+
# 					xlab("Length bin")+
# 					ylab("Frequency")+
# 					labs(fill="Fleet")+
# 					scale_fill_viridis(discrete=TRUE, option="viridis")
# 					#scale_x_discrete(breaks=c(1,5,10,20),labels=as.character(levels(dat.gg$bin))[c(1,5,10,20)])
# 					#scale_fill_brewer(palette = "BuPu")
# 		# }
# 		})
# 	})

#################
### AGE PLOTS ###
#################
observeEvent(req(!is.null(rv.Age$data)), {
    	shinyjs::show(output$marginal_age_comp_plots_label<-renderText({"Marginal age compositions. Sex categories: unknown sex = 0, females = 1 and males = 2."}))
  })

observeEvent(req(!is.null(rv.Age$data)), {
      shinyjs::show(output$conditional_age_comp_plots_label<-renderText({"Conditional age at length. Sex categories: unknown sex = 0, females = 1 and males = 2."}))
  })

  observeEvent(req(!is.null(rv.Age$data)), {
    marginal_ages<-subset(rv.Age$data,Lbin_hi<0)
    Cond_ages<-subset(rv.Age$data,Lbin_hi>=0)

  output$Ageplot_it_marginal<-renderUI({
  if(!is.null(rv.Age$data))
  {
        
    output$Ageplot_marginal<-renderPlot({ 
      #inFile_age <- rv.Age$data 
      # if (is.null(rv.Age$data)) return(NULL)  
      if (nrow(marginal_ages)==0) return(NULL)  
        
        # rv.Age$data %>%  
        marginal_ages %>%
        rename_all(tolower) %>%  
        dplyr::select(-nsamps,-lbin_hi) %>%  
        pivot_longer(c(-year, -fleet, -sex, -lbin_low)) %>%  
        mutate(Year = factor(year), 
               name = as.numeric(gsub("[^0-9.-]", "", name))) %>%  
        ggplot(aes(name, value, color=Year)) + 
        geom_line() + 
        # geom_col(position="dodge") + 
        #facet_wrap(sex~year, scales="free_y",ncol=5) + 
        facet_grid(sex~fleet, scales="free_y",labeller = label_both) +
        #scale_y_continuous(limits=c(0,max(colSums(rv.Age$data[-1,7:ncol(rv.Age$data)]))))+ 
        #scale_y_continuous(limits=c(0,20))+ 
        xlab("Age bin") + 
        ylab("Frequency") + 
        scale_fill_viridis_d() 
    })
  plotOutput("Ageplot_marginal")
  }
    })


  output$Ageplot_it_cond<-renderUI({
  if(!is.null(rv.Age$data))
  {
      output$Ageplot_conditional<-renderPlot({ 
      # if (is.null(rv.Age$data)) return(NULL)  
      if (nrow(Cond_ages)==0) return(NULL)  
        
        Cond_ages_plots<-reshape2::melt(Cond_ages[,c(1,3,4,7,9:ncol(Cond_ages))],id.vars=c("Year","Fleet","Sex","Lbin_hi"))
        Cond_ages_plots_pos<-subset(Cond_ages_plots,value>0)

        ggplot(Cond_ages_plots_pos,aes(x=as.numeric(variable),y=as.numeric(Lbin_hi),color=Year))+
        geom_point()+
        facet_grid(vars(Sex),vars(Fleet),labeller = label_both)+
        xlab("Age bin")+
        ylab("Length bin")
    })
  plotOutput("Ageplot_conditional")
}
    })

  })
# output$Ageplot <- renderPlot({ 
# 		inFile_age <- rv.Age$data 
# 		if (is.null(inFile_age)) return(NULL) 
# 		  rv.Age$data %>%  
# 		      pivot_longer(-1, names_to = "year", values_to = "ltnum") %>%  
# 		      rename(bin = Bins) %>%  
# 		  ggplot(aes(bin, ltnum)) + 
# 					geom_col(fill="#1D252D", color="white") + 
# 					facet_wrap(~year) + 
# 					xlab("Age bin") + 
# 					ylab("Frequency")			 
# 	}) 
 
##################
### INDEX PLOT ###
##################
 observeEvent(req(!is.null(rv.Index$data)), {
      shinyjs::show(output$index_plots_label<-renderText({"Indices of Abundance"}))
  })

  observeEvent(req(!is.null(rv.Index$data)), {
  output$Indexplot_it<-renderUI({
  if(!is.null(rv.Index$data))
    {
    output$Indexplot <- renderPlot({ 
      if (is.null(rv.Index$data)) return(NULL)     
        plot.Index<-rv.Index$data
        plot.Index[,3]<-as.factor(plot.Index[,3])
        plot.Index.zscore<-list()
        for(i in 1:length(unique(plot.Index$Fleet)))
        {
          plot.Index.temp<-plot.Index[plot.Index$Fleet %in% unique(plot.Index$Fleet)[i],]
          plot.Index.temp$Index<-(plot.Index.temp$Index-mean(plot.Index.temp$Index))/sd(plot.Index.temp$Index)
          plot.Index.zscore[[i]]<-plot.Index.temp
        }
          plot.Index.zs<-do.call("rbind", plot.Index.zscore)
        ggplot(plot.Index.zs,aes(x=Year,y=Index,group=Fleet, colour=Fleet)) +  
        geom_line(lwd=1.1) +
        geom_errorbar(aes(ymin=qlnorm(0.0275,log(Index),CV),ymax=qlnorm(0.975,log(Index),CV),group=Fleet),width=0,size=1)+ 
        geom_point(aes(colour=Fleet),size=4) +  
        ylab("Z-score") + 
        xlab("Year") +  
        scale_color_viridis_d() 
    })
      plotOutput("Indexplot")
  }
    })
  })


#####################
### Plot M by age ###
#####################
output$Mplot<-renderPlot({
      mf.in = M_f_in()+0.000000000000001 
			mm.in = M_f_in()+0.000000000000001 
 #      if(input$male_parms|input$male_parms_fix)
			#if(input$male_parms|input$male_parms_SSS|input$male_parms_fix|input$male_parms_est)
      if(!is.null(M_m_in()))
        { 
			     mm.in = M_m_in()+0.000000000000001
			  }		 
      if(any(is.na(c(mf.in, mm.in)))|any(is.null(c(mf.in, mm.in)))) return(NULL) 
      #if(any(is.na(c(mf.in, mm.in)))|any(is.null(c(mf.in, mm.in)))|all(is.null(rv.Ct$data),is.null(rv.Lt$data),is.null(rv.Age$data),is.null(rv.Index$data))) return(NULL) 
      Female_M = data.frame(Ages = 0:Nages(), PopN = exp(-mf.in * 0:Nages()), Sex="Female") 
			Male_M = data.frame(Ages = 0:Nages(), PopN=exp(-mm.in * 0:Nages()), Sex="Male") 
			M_sexes <- rbind(Female_M, Male_M) 
			Nage_4_plot <- grobTree(textGrob(paste0("Max age =", Nages()), x=0.1,  y=0.95, hjust=0,
      gp=gpar(col="darkblue", fontsize=12, fontface="italic")))
      ggplot(M_sexes,aes(Ages, PopN, color=Sex))+ 
					geom_line(aes(linetype=Sex), lwd=2)+ 
					ylab("Cohort decline by M")+
          annotation_custom(Nage_4_plot) 
		}) 

##############################
### Plot VBGF and maturity ###
##############################
output$VBGFplot<-renderPlot({ 
   	f_Linf = m_Linf = Linf() 
   	f_k = m_k = k_vbgf() 
   	f_t0 = m_t0 = t0_vbgf() 
	  f_L50 = L50() 
	  f_L95 = L95() 
	  maxage = Nages() 

  #if(any(input$male_parms,input$male_parms_SSS,input$male_parms_fix,input$male_parms_est))
  if(all(is.numeric(Linf_m_in()),is.numeric(k_vbgf_m_in()),is.numeric(t0_vbgf_m_in())))
      { 
          m_Linf = Linf_m_in() 
			   	m_k = k_vbgf_m_in() 
			   	m_t0 = t0_vbgf_m_in() 
			}		 
   if(any(is.na(c(f_Linf, f_k, f_t0)))=="FALSE"){ 
		
    vbgf_female = data.frame(Age = c(f_t0:Nages()),  
		                         Length = VBGF(f_Linf, f_k, f_t0, c(f_t0:Nages())), Sex="Female") 
    vbgf_male = data.frame(Age = f_t0:Nages(),  
                           Length=VBGF(m_Linf, m_k, m_t0, c(f_t0:Nages())), Sex="Male") 
      	rbind(vbgf_female,vbgf_male) %>%  
      	  ggplot(aes(Age, Length, color=Sex)) + 
      				geom_line(aes(linetype=Sex), lwd=2) -> vbgf.plot  
      
      if(any(is.na(c(f_L50, f_L95)))=="FALSE"){ 
        age.mat = data.frame(Age = VBGF.age(f_Linf, f_k, f_t0, c(f_L50, f_L95)), 
                             Length = c(f_L50, f_L95), Sex="Female") 
        vbgf.plot + 
        	geom_point(data = age.mat, aes(Age, Length), color = "darkorange", size=6) + 
        	geom_text(data = age.mat,label=c("Lmat50%", "Lmat95%"), 
        	          nudge_x = -0.1 * Nages(), color="black") -> vbgf.plot 
       } 
  	 vbgf.plot 
  	 } 
	}) 

########################
### Depletion Plot ###
########################

output$Dep_plot_title<-renderUI({
if(((as.numeric(input$tabs)*1)/1)<4&is.null(rv.Lt$data)&!is.null(rv.Ct$data)&is.null(rv.Age$data)&is.null(rv.Index$data)&any(is.null(input$user_model),!input$user_model)){
   h4("Relative Stock Status Prior")
 }
 })

output$Dep_plot_it<-renderUI({
if(((as.numeric(input$tabs)*1)/1)<4&is.null(rv.Lt$data)&!is.null(rv.Ct$data)&is.null(rv.Age$data)&is.null(rv.Index$data)&any(is.null(input$user_model),!input$user_model)){
if (is.na(input$Depl_mean_sss)) return(NULL) 
output$Depletion_plot <- renderPlot({ 
      if(!is.na(input$status_year)&!is.na(input$Depl_mean_sss))
        {     
          if(input$Depl_prior_sss=="beta"){dep.hist.sss<-data.frame(draws=rbeta.ab(100000,input$Depl_mean_sss,input$Depl_SD_sss,0,1))}
          if(input$Depl_prior_sss=="lognormal"){dep.hist.sss<-data.frame(draws=rlnorm(100000,log(input$Depl_mean_sss),input$Depl_SD_sss))}
          if(input$Depl_prior_sss=="truncated normal"){dep.hist.sss<-data.frame(draws=rtruncnorm(100000,0,1,input$Depl_mean_sss,input$Depl_SD_sss))}
          if(input$Depl_prior_sss=="uniform"){dep.hist.sss<-data.frame(draws=runif(100000,input$Depl_mean_sss,input$Depl_SD_sss))}
          if(input$Depl_prior_sss=="no prior"){NULL}
          Depletion_plot<-gghistogram(dep.hist.sss, x = "draws", fill = "purple")
          Depletion_plot    
        }
      })    
   plotOutput("Depletion_plot")
    }
  })

########################
### Selectivity Plot ###
########################
 # observeEvent(req(input$Sel50,input$Selpeak), {
 #      shinyjs::show(output$Sel_plots_label<-renderText({"Selectivity"}))
 #  })

#h4("Selectivity")

output$Selplot <- renderPlot({ 

if(!is.null(input$Sel50)&!is.null(input$Selpeak))
    {
        if(input$Sel_choice=="Logistic"&any(any(input$Sel50[1]=="",is.null(input$Sel50)),any(input$Selpeak[1]=="",is.null(input$Selpeak)))) return(NULL) 

    if(input$Sel_choice=="Logistic")
    {

      if(all(length(as.numeric(trimws(unlist(strsplit(input$Sel50,",")))))==length(as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))),
        all(input$Sel50!=""),
        all(!is.null(input$Sel50)),
        all(!str_detect(input$Sel50,"NA")),
        all(input$Selpeak!=""),
        all(!is.null(input$Selpeak)),
        all(!str_detect(input$Selpeak,"NA"))))
      {
       Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50,","))))
       Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))
       PeakDesc<-rep(10000,length(Selpeak))
       LtPeakFinal<-rep(0.0001,length(Selpeak))
       FinalSel<-rep(0.999,length(Selpeak))
      
      # if(input$Sel_choice=="Logistic")
      #   {
      #   }
      # if(input$Sel_choice=="Dome-shaped")
         # {
        #   PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))
        #   LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))
        #   FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))      
        # }
      
       #if(!is.null(rv.Ct$data))
       #{
       # fleetnames.ct<-colnames(rv.Ct$data)[-1]
       #}
       #if(!is.null(rv.Index$data))
       #{
       # fleetnames.index<-unique(rv.Ct$data$Labels)
       #}
       

       Sel.out<-doubleNorm24.sel(Sel50=Sel50[1],Selpeak=Selpeak[1],PeakDesc=PeakDesc[1],LtPeakFinal=LtPeakFinal[1],FinalSel=FinalSel[1])
       #Sel.out<-data.frame(Bin=Sel.out[,1],Sel=Sel.out[,2],Fleet=fleetnames.ct[1])
       Sel.out<-data.frame(Bin=Sel.out[,1],Sel=Sel.out[,2],Fleet="Fleet 1")
       if(length(Sel50)>1)
       {
        for(ii in 2:length(Sel50))
        {
        Sel.out.temp<-doubleNorm24.sel(Sel50=Sel50[ii],Selpeak=Selpeak[ii],PeakDesc=PeakDesc[ii],LtPeakFinal=LtPeakFinal[ii],FinalSel=FinalSel[ii])
#        Sel.out.temp<-data.frame(Bin=Sel.out.temp[,1],Sel=Sel.out.temp[,2],Fleet=fleetnames.ct[ii])
        Sel.out.temp<-data.frame(Bin=Sel.out.temp[,1],Sel=Sel.out.temp[,2],Fleet=paste0("Fleet ",ii))
        Sel.out<-rbind(Sel.out,Sel.out.temp)
        }
       }
        selplot.out<-ggplot(Sel.out,aes(Bin,Sel,colour=Fleet)) +  
          geom_line(lwd=1.5) + 
          ylab("Proportion Selected") + 
          xlab("Length Bins") +  
          scale_color_viridis_d() 
      }
    }
    
    if(input$Sel_choice=="Dome-shaped")
    {
        if(all(length(as.numeric(trimws(unlist(strsplit(input$Sel50,",")))))==length(as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))),
        length(as.numeric(trimws(unlist(strsplit(input$Sel50,",")))))==length(as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))),
        length(as.numeric(trimws(unlist(strsplit(input$Sel50,",")))))==length(as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))),
        length(as.numeric(trimws(unlist(strsplit(input$Sel50,",")))))==length(as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))),
        all(input$Sel50!=""),
        all(!is.null(input$Sel50)),
        all(input$Selpeak!=""),
        all(!is.null(input$Selpeak))))
       {
       Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50,","))))
       Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))
       PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))
       LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))
       FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))      
      
      # if(input$Sel_choice=="Logistic")
      #   {
      #     PeakDesc<-rep(10000,length(Selpeak))
      #     LtPeakFinal<-rep(0.0001,length(Selpeak))
      #     FinalSel<-rep(0.999,length(Selpeak))
      #   }
      # if(input$Sel_choice=="Dome-shaped")
      #   {
        # }
      
       Sel.out<-doubleNorm24.sel(Sel50=Sel50[1],Selpeak=Selpeak[1],PeakDesc=PeakDesc[1],LtPeakFinal=LtPeakFinal[1],FinalSel=FinalSel[1])
       Sel.out<-data.frame(Bin=Sel.out[,1],Sel=Sel.out[,2],Fleet="Fleet 1")
       if(length(Sel50)>1)
       {
        for(ii in 2:length(Sel50))
        {
        Sel.out.temp<-doubleNorm24.sel(Sel50=Sel50[ii],Selpeak=Selpeak[ii],PeakDesc=PeakDesc[ii],LtPeakFinal=LtPeakFinal[ii],FinalSel=FinalSel[ii])
        Sel.out.temp<-data.frame(Bin=Sel.out.temp[,1],Sel=Sel.out.temp[,2],Fleet=paste0("Fleet ",ii))
        Sel.out<-rbind(Sel.out,Sel.out.temp)
        }
       }
        selplot.out<-ggplot(Sel.out,aes(Bin,Sel,colour=Fleet)) +  
          geom_line(lwd=1.5) + 
          ylab("Length Bins") + 
          xlab("Selectivity") +  
          scale_color_viridis_d() 
      }      
    }
  }
    if(!is.null(get0("selplot.out"))){return(selplot.out)}
    else(return(NULL))
    }) 

output$Selplot_SSS <- renderPlot({ 
    if(!is.null(input$Sel50_sss)&!is.null(input$Selpeak_sss))
    {
    if(input$Sel_choice_sss=="Logistic"&any(any(any(is.na(as.numeric(trimws(unlist(strsplit(input$Sel50_sss,",")))))),input$Sel50_sss[1]=="",is.null(input$Sel50_sss)),any(any(is.na(as.numeric(trimws(unlist(strsplit(input$Selpeak_sss,",")))))),input$Selpeak_sss[1]=="",is.null(input$Selpeak_sss)))) return(NULL) 
    if(input$Sel_choice_sss=="Logistic")
    {
      if(all(length(as.numeric(trimws(unlist(strsplit(input$Sel50_sss,",")))))==length(as.numeric(trimws(unlist(strsplit(input$Selpeak_sss,","))))),
        all(input$Sel50_sss!=""),
        all(!str_detect(input$Sel50_sss,"NA")),
        all(!is.null(input$Sel50_sss)),
        all(input$Selpeak_sss!=""),
        all(!is.null(input$Selpeak_sss)),
        all(!str_detect(input$Selpeak_sss,"NA"))))
      {
       Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50_sss,","))))
       Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak_sss,","))))
       PeakDesc<-rep(10000,length(Selpeak))
       LtPeakFinal<-rep(0.0001,length(Selpeak))
       FinalSel<-rep(0.999,length(Selpeak))
       Sel.out<-doubleNorm24.sel(Sel50=Sel50[1],Selpeak=Selpeak[1],PeakDesc=PeakDesc[1],LtPeakFinal=LtPeakFinal[1],FinalSel=FinalSel[1])
       Sel.out<-data.frame(Bin=Sel.out[,1],Sel=Sel.out[,2],Fleet="Fleet 1")
       if(length(Sel50)>1)
       {
        for(ii in 2:length(Sel50))
        {
        Sel.out.temp<-doubleNorm24.sel(Sel50=Sel50[ii],Selpeak=Selpeak[ii],PeakDesc=PeakDesc[ii],LtPeakFinal=LtPeakFinal[ii],FinalSel=FinalSel[ii])
        Sel.out.temp<-data.frame(Bin=Sel.out.temp[,1],Sel=Sel.out.temp[,2],Fleet=paste0("Fleet ",ii))
        Sel.out<-rbind(Sel.out,Sel.out.temp)
        }
       }
        selplot.out<-ggplot(Sel.out,aes(Bin,Sel,colour=Fleet)) +  
          geom_line(lwd=1.5) + 
          ylab("Length Bins") + 
          xlab("Selectivity") +  
          scale_color_viridis_d() 
      }
    }
  

  if(input$Sel_choice_sss=="Dome-shaped")
    {
        if(all(length(as.numeric(trimws(unlist(strsplit(input$Sel50_sss,",")))))==length(as.numeric(trimws(unlist(strsplit(input$Selpeak_sss,","))))),
        length(as.numeric(trimws(unlist(strsplit(input$Sel50_sss,",")))))==length(as.numeric(trimws(unlist(strsplit(input$PeakDesc_sss,","))))),
        length(as.numeric(trimws(unlist(strsplit(input$Sel50_sss,",")))))==length(as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_sss,","))))),
        length(as.numeric(trimws(unlist(strsplit(input$Sel50_sss,",")))))==length(as.numeric(trimws(unlist(strsplit(input$FinalSel_sss,","))))),
        all(input$Sel50_sss!=""),
        all(!is.null(input$Sel50_sss)),
        all(input$Selpeak_sss!=""),
        all(!is.null(input$Selpeak_sss))))
       {
       Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50_sss,","))))
       Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak_sss,","))))
       PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc_sss,","))))
       LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_sss,","))))
       FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel_sss,","))))      
      
       Sel.out<-doubleNorm24.sel(Sel50=Sel50[1],Selpeak=Selpeak[1],PeakDesc=PeakDesc[1],LtPeakFinal=LtPeakFinal[1],FinalSel=FinalSel[1])
       Sel.out<-data.frame(Bin=Sel.out[,1],Sel=Sel.out[,2],Fleet="Fleet 1")
       if(length(Sel50)>1)
       {
        for(ii in 2:length(Sel50))
        {
        Sel.out.temp<-doubleNorm24.sel(Sel50=Sel50[ii],Selpeak=Selpeak[ii],PeakDesc=PeakDesc[ii],LtPeakFinal=LtPeakFinal[ii],FinalSel=FinalSel[ii])
        Sel.out.temp<-data.frame(Bin=Sel.out.temp[,1],Sel=Sel.out.temp[,2],Fleet=paste0("Fleet ",ii))
        Sel.out<-rbind(Sel.out,Sel.out.temp)
        }
       }
        selplot.out<-ggplot(Sel.out,aes(Bin,Sel,colour=Fleet)) +  
          geom_line(lwd=1.5) + 
          ylab("Length Bins") + 
          xlab("Selectivity") +  
          scale_color_viridis_d() 
      }      
    }
    }
    if(!is.null(get0("selplot.out"))){return(selplot.out)}
    else(return(NULL))
    })

#############################################
###              END PLOTS                ###
#############################################

#############################################
######## PREPARE FILES andD RUN SSS #########
#############################################
SSS.run<-observeEvent(input$run_SSS,{
      show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[1],text="Create model files")
print(1)
    # progress <- shiny::Progress$new(session, min=1, max=2)
    #        on.exit(progress$close())
       
    #        progress$set(message = 'Model run in progress',
    #                     detail = '')
       
    #        for (i in 1:2) {
    #          progress$set(value = i)
    #          Sys.sleep(0.5)
    #        }

  	#Copy and move files
	  	if(file.exists(paste0("Scenarios/",input$Scenario_name)))
			{
				unlink(paste0("Scenarios/",input$Scenario_name),recursive=TRUE)
#				file.remove(paste0(getwd(),"/Scenarios/",input$Scenario_name))
			}
	  	#if(input$)
	  		{
	  			file.copy(paste0("SSS_files/sssexample_BH"),paste0("Scenarios"),recursive=TRUE,overwrite=TRUE)
				file.rename(paste0("Scenarios/sssexample_BH"), paste0("Scenarios/",input$Scenario_name))
			}
      
      #BOOKMARKING: copy inputs rds and uploaded csvs into scenarios folder
      saveInputs(input, bookmarkFilePath(), latestBookmarkURL(), session)

      # session$doBookmark()
      # bookmarkInputPath <- file.path(dirname(bookmarkFilePath()), latestBookmarkURL(), "input.rds")
      # if(!dir.exists(dirname(file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds")))) {
      #   dir.create(dirname(file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds")), recursive = TRUE)
      # }
      # file.copy(from = bookmarkInputPath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds"), overwrite=TRUE)
      # for(i in 1:4){
      #   if(!is.null(input[[paste0("file",i)]])){
      #     file.copy(from = input[[paste0("file",i)]]$datapath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", input[[paste0("file",i)]]$name), overwrite=TRUE)
      #   }
      # }
      
      # dir_delete(bookmarkFilePath()) #delete bookmark created from session$doBookmark as we are just using it to create the query string and grab the filepath

	  	#if()
#	  		{
#	  			file.copy(paste0(getwd(),"/SSS_files/sssexample_RickPow"),paste0(getwd(),"/Scenarios"),recursive=TRUE,overwrite=TRUE)
#				file.rename(paste0(getwd(),"/Scenarios/sssexample_RickPow"), paste0(getwd(),"/Scenarios/",input$Scenario_name))
#			}
  	#Read data and control files
		data.file<-SS_readdat(paste0("Scenarios/",input$Scenario_name,"/sss_example.dat")) 
		ctl.file<-SS_readctl(paste0("Scenarios/",input$Scenario_name,"/sss_example.ctl"),use_datlist = TRUE, datlist=data.file) 
		#Read, edit then write new DATA file
    data.file$Comments <- c(data.file$Comments, input$scenario_description_input)
    data.file$styr<-input$styr
		data.file$endyr<-input$endyr
    data.file$Nages<-input$Nages_in_sss #Nages()

	#Catches
		Catch.data<-rv.Ct$data
		catch.dep.fleets<-ncol(Catch.data)
    data.file$Nfleets<-catch.dep.fleets
    if(!is.null(rv.Index$data))
      {
        index.fleets<-max(rv.Index$data$Fleet)
        if(index.fleets>catch.dep.fleets) {data.file$Nfleets<-index.fleets}
        if(index.fleets==catch.dep.fleets) {data.file$Nfleets<-index.fleets+1}
        if(index.fleets<catch.dep.fleets) {data.file$Nfleets<-catch.dep.fleets}
      }
    
		if((data.file$Nfleets-1)>1){
			for(i in 1:(data.file$Nfleets-2))
			{
				data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
				data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
			}
			data.file$fleetinfo$fleetname<-c(paste0("Fishery",1:(catch.dep.fleets-1)),"Depl")
			data.file$fleetinfo$type[c(2,data.file$Nfleets)]<-c(1,3)
      data.file$fleetinfo$surveytiming[c(2,data.file$Nfleets)]<-c(-1,0.1)
      data.file$CPUEinfo[,1]<-1:data.file$Nfleets
      data.file$CPUEinfo[c(2,data.file$Nfleets),2]<-c(1,34)
      data.file$CPUE$index<-data.file$Nfleets
		}
		year.in<-Catch.data[,1]
		catch.cols<-colnames(data.file$catch)
		catch_temp<-list()
		for(i in 1:(data.file$Nfleets-1))
		{
			catch_temp[[i]]<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(i,length(year.in)+1),
						c(0,Catch.data[,i+1]),
						rep(0.01,length(year.in)+1)
						)
		}
		data.file$catch<-list.rbind(catch_temp)
		colnames(data.file$catch)<-catch.cols
	
  #Relative stock status
  data.file$CPUE$year<-c(input$styr,input$status_year)

	#Length composition data
		if(input$Linf_f_mean_sss>30){data.file$binwidth<-2}
		data.file$minimum_size<-floor(input$Linf_f_mean_sss/10)
		data.file$maximum_size<-ceiling(input$Linf_f_mean_sss+(input$Linf_f_mean_sss*0.1))
		data.file$lbin_vector<-seq(data.file$minimum_size,data.file$maximum_size,data.file$binwidth)
	  data.file$N_lbinspop<-length(data.file$lbin_vector)
  #Age composition data
		# if (is.null(inFile_age)){
		# data.file$N_agebins<-Nages()
		# data.file$agebin_vector<-1:Nages()		
		# data.file$ageerror<-data.frame(matrix(c(rep(-1,(Nages()+1)),rep(0.001,(Nages()+1))),2,(Nages()+1),byrow=TRUE))
		# colnames(data.file$ageerror)<-paste0("age",1:Nages())		
		# 	}
#Catch units     
    if(input$Ct_units_choice_SSS)
    {
      ct.units<-as.numeric(trimws(unlist(strsplit(input$fleet_ct_units_SSS,","))))
      #data.file$fleetinfo[ct.units,4]<-2 #use this when just specifying which are fleets are numbers
      data.file$fleetinfo[,4]<-c(ct.units,1)
    }
		
		SS_writedat(data.file,paste0("Scenarios/",input$Scenario_name,"/sss_example.dat"),overwrite=TRUE)			
		####################### END DATA FILE #####################################

    ####################### START SSS CTL FILE #####################################
    ctl.file$Comments <- c(ctl.file$Comments, input$scenario_description_input)
    if(!is.null(input$GT5)){if(input$GT5)
      {
        ctl.file$N_platoon<-5
        ctl.file$sd_ratio<-0.7
        ctl.file$submorphdist<-c(-1,0.25,0.5,0.25,0.125)
      }
    }
    
    #if(all(any(input$est_parms==TRUE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data))))==TRUE)
    #{
    fem_vbgf<-VBGF(input$Linf_f_mean_sss,input$k_f_mean_sss,input$t0_f_mean_sss,c(0:Nages()))
    #c("lognormal","truncated normal","uniform","beta")
    prior.name<-c("no prior","symmetric beta", "beta","lognormal","gamma","normal")
    prior.type<-c(0:3,5,6)
    #Females
    #M
    if(input$M_f_prior=="lognormal"){ctl.file$MG_parms[1,3:4]<-c(input$M_f_mean_sss,log(input$M_f_mean_sss))}
    else {ctl.file$MG_parms[1,3:4]<-c(input$M_f_mean_sss,input$M_f_mean_sss)}
        
    #L0    
    ctl.file$Growth_Age_for_L1<-input$t0_f_mean_sss
    ctl.file$Growth_Age_for_L1<-0
    #if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[2,3:4]<-c(fem_vbgf[1],log(fem_vbgf[1]))}
    #else {ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]}
    if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[2,3:4]<-c(0,log(0.0000001))}
    else {ctl.file$MG_parms[2,3:4]<-0}
    
    #Linf
    if(input$Linf_f_prior=="lognormal"){ctl.file$MG_parms[3,3:4]<-c(input$Linf_f_mean_sss,log(input$Linf_f_mean_sss))}     
    else{ctl.file$MG_parms[3,3:4]<-input$Linf_f_mean_sss}
    
    #k
    if(input$k_f_prior=="lognormal"){ctl.file$MG_parms[4,3:4]<-c(input$k_f_mean_sss,log(input$k_f_mean_sss))}        
    else {ctl.file$MG_parms[4,3:4]<-input$k_f_mean_sss}
    
    #CV young
    if(input$CV_lt_f_young_prior=="lognormal"){ctl.file$MG_parms[5,3:4]<-c(input$CV_lt_f_young_mean_sss,log(input$CV_lt_f_young_mean_sss))}     
    else{ctl.file$MG_parms[5,3:4]<-input$CV_lt_f_young_mean_sss}
    
    #CV old
    if(input$CV_lt_f_old_prior=="lognormal"){ctl.file$MG_parms[6,3:4]<-c(input$CV_lt_f_old_mean_sss,log(input$CV_lt_f_old_mean_sss))}
    else{ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_old_mean_sss}
    
    #Weight-length
    ctl.file$MG_parms[7,3:4]<-input$WLa_f_sss                                    #coefficient
    ctl.file$MG_parms[8,3:4]<- input$WLb_f_sss                                   #exponent  

    #Maturity
    ctl.file$MG_parms[9,3:4]<-input$L50_f_sss                                     #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f_sss-input$L50_f_sss)  #Maturity slope
    
    #Males
    ctl.file$MG_parms[13,3:4]<-c(input$M_f_mean_sss,log(input$M_f_mean_sss))    #M
    #ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]                                      #L0
    ctl.file$MG_parms[14,3:4]<-0                                                #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f_mean_sss                            #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f_mean_sss                               #k
    ctl.file$MG_parms[17,3:4]<-input$CV_lt_f_young_mean_sss                           #CV
    ctl.file$MG_parms[18,3:4]<-input$CV_lt_f_old_mean_sss                           #CV
    #Weight-length
    ctl.file$MG_parms[19,3:4]<-input$WLa_f_sss                                  #coefficient
    ctl.file$MG_parms[20,3:4]<- input$WLb_f_sss                                 #exponent  
    ctl.file$MG_parms[11,3:4]<-input$Fec_a_f_sss                                #coefficient
    ctl.file$MG_parms[12,3:4]<- input$Fec_b_f_sss                               #exponent  

    if(input$male_offset_SSS)
    {
      ctl.file$parameter_offset_approach<-2                         #Change to offset approach
      ctl.file$MG_parms[13,c(1,3:4)]<-0                                  #M
      ctl.file$MG_parms[14,c(1,3:4)]<-0                                  #L0
      ctl.file$MG_parms[15,c(1,3:4)]<-0                                  #Linf
      ctl.file$MG_parms[16,c(1,3:4)]<-0                                  #k
      ctl.file$MG_parms[17,c(1,3:4)]<-0                                  #CV
      ctl.file$MG_parms[18,c(1,3:4)]<-0                                  #CV
      #Weight-length
      ctl.file$MG_parms[19,c(1,3:4)]<-input$WLa_f_sss                                  #coefficient
      ctl.file$MG_parms[20,c(1,3:4)]<-input$WLb_f_sss                                  #exponent  
    }
    
    if(input$male_parms_SSS)
      {   
        male_vbgf_sss<-VBGF(input$Linf_m_mean_sss,input$k_m_mean_sss,input$t0_m_mean_sss,c(input$t0_f_mean_sss:Nages()))

        #M
        if(input$M_m_prior_sss=="lognormal"){ctl.file$MG_parms[13,3:4]<-c(input$M_m_mean_sss,log(input$M_m_mean_sss))}
        else {ctl.file$MG_parms[13,3:4]<-c(input$M_m_mean_sss,input$M_m_mean_sss)}
            
        #L0    
        if(input$t0_f_prior_sss=="lognormal"){ctl.file$MG_parms[14,3:4]<-c(male_vbgf_sss[1],log(male_vbgf_sss[1]))}
        else {ctl.file$MG_parms[14,3:4]<-c(male_vbgf_sss[1],male_vbgf_sss[1])}
        # if(input$t0_f_prior_sss=="lognormal"){ctl.file$MG_parms[14,3:4]<-c(0,log(0.0000001))}
        #else {ctl.file$MG_parms[14,3:4]<-c(0,0)}
        
        #Linf
        if(input$Linf_f_prior_sss=="lognormal"){ctl.file$MG_parms[15,3:4]<-c(input$Linf_m_mean_sss,log(input$Linf_m_mean_sss))}     
        else{ctl.file$MG_parms[15,3:4]<-c(input$Linf_m_mean_sss,input$Linf_m_mean_sss)}
        
        #k
        if(input$k_f_prior_sss=="lognormal"){ctl.file$MG_parms[16,3:4]<-c(input$k_m_mean_sss,log(input$k_m_mean_sss))}        
        else {ctl.file$MG_parms[16,3:4]<-c(input$k_m_mean_sss,input$k_m_mean_sss)}
        
        #CV young
        if(input$CV_lt_f_young_prior_sss=="lognormal"){ctl.file$MG_parms[17,3:4]<-c(input$CV_lt_m_young_mean_sss,log(input$CV_lt_m_young_mean_sss))}     
        else{ctl.file$MG_parms[17,3:4]<-c(input$CV_lt_m_young_mean_sss,input$CV_lt_m_young_mean_sss)}
        
        #CV old
        if(input$CV_lt_f_old_prior_sss=="lognormal"){ctl.file$MG_parms[18,3:4]<-c(input$CV_lt_m_old_mean_sss,log(input$CV_lt_m_old_mean_sss))}
        else{ctl.file$MG_parms[18,3:4]<-c(input$CV_lt_m_old_mean_sss,input$CV_lt_m_old_mean_sss)}
        
        #Weight-length
        ctl.file$MG_parms[19,3:4]<-input$WLa_m_sss                                    #coefficient
        ctl.file$MG_parms[20,3:4]<- input$WLb_m_sss                                   #exponent  
      }     

    #S-R
    #ctl.file$SR_parms[1,3:4]<-input$lnR0  #lnR0
    
    if(input$h_ss_prior=="lognormal"){ctl.file$SR_parms[2,3:4]<-c(input$h_mean_ss,log(h_mean_ss))}
    else{ctl.file$SR_parms[2,3:4]<-input$h_mean_ss}        
    #}
    ctl.file$MainRdevYrFirst<-input$styr	#Start year of recruitment estimation
		ctl.file$MainRdevYrLast<-input$endyr		#Last year of recruitment estimation
		
    #
      ctl.file$Q_options[1]<-data.file$Nfleets
    #Selectivity
      Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50_sss,","))))
      Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak_sss,","))))
      bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]

    if(input$Sel_choice_sss=="Logistic")
    {
      #ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
      ctl.file$size_selex_parms[2,3:4]<- 15
      ctl.file$size_selex_parms[3,3:4]<- log(-((Sel50[1]-Selpeak[1])^2/log(0.5)))
      ctl.file$size_selex_parms[4,3:4]<- -15
      ctl.file$size_selex_parms[6,3:4]<- 15
      }
    if(input$Sel_choice_sss=="Dome-shaped")
    {     
      PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc_sss,","))))
      LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_sss,","))))
      FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel_sss,","))))
      
      #ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
      ctl.file$size_selex_parms[2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[1]-bin.width)/(PeakDesc[1]-Selpeak[1]-bin.width+0.000000001))
      ctl.file$size_selex_parms[3,3:4]<- log(-((Sel50[1]-Selpeak[1])^2/log(0.5)))
      ctl.file$size_selex_parms[4,3:4]<- log(LtPeakFinal[1])
      ctl.file$size_selex_parms[6,3:4]<- -log((1/(FinalSel[1]+0.000000001)-1))
    }

    #Add other fleets
    if((data.file$Nfleets-1)>1){
      for(i in 1:(data.file$Nfleets-2))
      {
        #ctl.file$init_F<-rbind(ctl.file$init_F,ctl.file$init_F[1,])
        ctl.file$size_selex_types<-rbind(ctl.file$size_selex_types,ctl.file$size_selex_types[1,])
        ctl.file$age_selex_types<-rbind(ctl.file$age_selex_types,ctl.file$age_selex_types[1,])
        ctl.file$size_selex_parms<-rbind(ctl.file$size_selex_parms,ctl.file$size_selex_parms[1:6,])
        
        if(input$Sel_choice_sss=="Logistic")
        {
          #ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
          ctl.file$size_selex_parms[6*i+2,3:4]<- 15
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+4,3:4]<- -15
          ctl.file$size_selex_parms[6*i+6,3:4]<- 15
        }

        if(input$Sel_choice_sss=="Dome-shaped")
        {
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
#          if(PeakDesc[i+1]-Selpeak[i+1]-bin.width<=0 & PeakDesc[i+1]-Selpeak[i+1]-bin.width>-2){Sel2_denom<-+0.000000001}
          ctl.file$size_selex_parms[6*i+2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[i+1]-bin.width)/(PeakDesc[i+1]-Selpeak[i+1]-bin.width+0.000000001))
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+4,3:4]<- log(LtPeakFinal[i+1])
          ctl.file$size_selex_parms[6*i+6,3:4]<- -log((1/(FinalSel[i+1]+0.000000001)-1))
        }
    
      }

        ctl.file$size_selex_types[,1]<-c(rep(24,data.file$Nfleets-1),0)
        ctl.file$age_selex_types[,1]<-10
        
      #Re-label so r4ss can interpret these new entries
      #rownames(ctl.file$init_F)<-paste0("InitF_seas_1_flt_",1:data.file$Nfleets,"Fishery",1:data.file$Nfleets)
      rownames(ctl.file$age_selex_types)<-rownames(ctl.file$size_selex_types)<-c(paste0("Fishery",1:(data.file$Nfleets-1)),"Depl")
      size_selex_parms_rownames<-list()
      for(f_i in 1:(data.file$Nfleets-1))
      {
        size_selex_parms_rownames[[f_i]]<-c(paste0("SizeSel_P_1_Fishery",f_i,"(",f_i,")"),
          paste0("SizeSel_P_2_Fishery",f_i,"(",f_i,")"),
          paste0("SizeSel_P_3_Fishery",f_i,"(",f_i,")"),
          paste0("SizeSel_P_4_Fishery",f_i,"(",f_i,")"),
          paste0("SizeSel_P_5_Fishery",f_i,"(",f_i,")"),
          paste0("SizeSel_P_6_Fishery",f_i,"(",f_i,")"))    
      }
      size_selex_parms_rownames<-unlist(size_selex_parms_rownames)
      rownames(ctl.file$size_selex_parms)<-size_selex_parms_rownames
    }


    SS_writectl(ctl.file,paste0("Scenarios/",input$Scenario_name,"/sss_example.ctl"),overwrite=TRUE)

#Forecast file modfications
#Reference points
#if(!input$use_forecastnew)
#{
forecast.file<-SS_readforecast(paste0("Scenarios/",input$Scenario_name,"/forecast.ss"))

if(input$RP_choices){
    forecast.file$SPRtarget<-input$SPR_target
    forecast.file$Btarget<-input$B_target

    CR_choices<-c("1: Catch fxn of SSB, buffer on F",
              "2: F fxn of SSB, buffer on F",
              "3: Catch fxn of SSB, buffer on catch",
              "4: F fxn of SSB, buffer on catch")
    CR_choices_num.vec<-c(1:4)
    forecast.file$ControlRuleMethod<-CR_choices_num.vec[CR_choices==input$CR_Ct_F]
    forecast.file$SBforconstantF<-input$slope_hi
    forecast.file$BfornoF<-input$slope_low  
  }

if(input$Forecast_choice)
  {
    forecast.file$Nforecastyrs<-input$forecast_num
    buffer.in<-as.numeric(trimws(unlist(strsplit(input$forecast_buffer,","))))
    if(length(buffer.in)==1){forecast.file$Flimitfraction<-buffer.in}    
    if(length(buffer.in)>1)
      {
        forecast.file$Flimitfraction<--1
        buffer.datafr<-data.frame(Year=c((data.file$endyr+1):(data.file$endyr+input$forecast_num)),Fraction=buffer.in)
        rownames(buffer.datafr)<-paste0("#_Flimitfraction_m",1:input$forecast_num)
        forecast.file$Flimitfraction_m<-buffer.datafr      
      }
  }

SS_writeforecast(forecast.file,paste0("Scenarios/",input$Scenario_name),overwrite=TRUE)  
#}

#if(input$use_forecastnew)
#  {
#    forecast.file<-SS_readforecast(paste0("Scenarios/",input$Scenario_name,"/forecast.ss_new"))
#    SS_writeforecast(forecast.file,paste0("Scenarios/",input$Scenario_name),overwrite=TRUE)  
#  }

#Set prior inputs
    #0 = normal
    #10 = truncated normal
    #1 = symmetric beta (rbeta)
    #2 = beta 
    #3 = lognormal
    #30 = truncated lognormal
    #4 = uniform
    #99 = used only for the steepness parameter. Indicates h will come from FMSY/M prior

    sss.prior.name<-c("no prior","symmetric beta","beta","normal","truncated normal","lognormal","truncated lognormal","uniform")
    sss.prior.type<-c(-1,1,2,0,10,3,30,4)
    
    Dep.in_sss<-c(sss.prior.type[sss.prior.name==input$Depl_prior_sss],input$Depl_mean_sss,input$Depl_SD_sss)
    
    h.in_sss<-c(sss.prior.type[sss.prior.name==input$h_prior_sss],input$h_mean_sss,input$h_SD_sss)
    if(!input$male_offset_SSS)
    {
      M.in_sss<-c(sss.prior.type[sss.prior.name==input$M_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss,sss.prior.type[sss.prior.name==input$M_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss)
      Linf.in_sss<-c(sss.prior.type[sss.prior.name==input$Linf_f_prior_sss],input$Linf_f_mean_sss,input$Linf_f_SD_sss,sss.prior.type[sss.prior.name==input$Linf_f_prior_sss],input$Linf_f_mean_sss,input$Linf_f_SD_sss)
      k.in_sss<-c(sss.prior.type[sss.prior.name==input$k_f_prior_sss],input$k_f_mean_sss,input$k_f_SD_sss,sss.prior.type[sss.prior.name==input$k_f_prior_sss],input$k_f_mean_sss,input$k_f_SD_sss)      
      t0.in_sss<-c(sss.prior.type[sss.prior.name==input$t0_f_prior_sss],input$t0_f_mean_sss,input$t0_f_SD_sss,sss.prior.type[sss.prior.name==input$t0_f_prior_sss],input$t0_f_mean_sss,input$t0_f_SD_sss)
    }

   if(input$male_offset_SSS)
     {
       M.in_sss<-c(sss.prior.type[sss.prior.name==input$M_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss,sss.prior.type[sss.prior.name==input$M_prior_sss],0,0)
       Linf.in_sss<-c(sss.prior.type[sss.prior.name==input$Linf_f_prior_sss],input$Linf_f_mean_sss,input$Linf_f_SD_sss,sss.prior.type[sss.prior.name==input$Linf_f_prior_sss],0,0)
       k.in_sss<-c(sss.prior.type[sss.prior.name==input$k_f_prior_sss],input$k_f_mean_sss,input$k_f_SD_sss,sss.prior.type[sss.prior.name==input$k_f_prior_sss],0,0)      
       t0.in_sss<-c(sss.prior.type[sss.prior.name==input$t0_f_prior_sss],input$t0_f_mean_sss,input$t0_f_SD_sss,sss.prior.type[sss.prior.name==input$t0_f_prior_sss],0,0)
     }

    if(input$male_parms_SSS)
    {
      M.in_sss<-c(sss.prior.type[sss.prior.name==input$M_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss,sss.prior.type[sss.prior.name==input$M_m_prior_sss],input$M_m_mean_sss,input$M_m_SD_sss)
      Linf.in_sss<-c(sss.prior.type[sss.prior.name==input$Linf_f_prior_sss],input$Linf_f_mean_sss,input$Linf_f_SD_sss,sss.prior.type[sss.prior.name==input$Linf_m_prior_sss],input$Linf_m_mean_sss,input$Linf_f_SD_sss)
      k.in_sss<-c(sss.prior.type[sss.prior.name==input$k_f_prior_sss],input$k_f_mean_sss,input$k_f_SD_sss,sss.prior.type[sss.prior.name==input$k_m_prior_sss],input$k_m_mean_sss,input$k_m_SD_sss)      
      t0.in_sss<-c(sss.prior.type[sss.prior.name==input$t0_f_prior_sss],input$t0_f_mean_sss,input$t0_f_SD_sss,sss.prior.type[sss.prior.name==input$t0_m_prior_sss],input$t0_m_mean_sss,input$t0_m_SD_sss)
    }
      show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[2],text="Model run in progress")

#Run SSS
        if(is.na(input$Depl_mean_sss)|input$Depl_mean_sss<0)
      {
      #Throw warning if not enough selectivity inputs
         sendSweetAlert(
          session = session,
          title = "Stock Status input missing",
          text = "Please check to see if you have provided an input for relatives stock status and that it is > 0.",
          type = "error")
         remove_modal_spinner()
         #shiny::stopApp()
      }

else
  {
  SSS.out<-SSS(paste0("Scenarios/",input$Scenario_name),
      file.name=c("sss_example.dat","sss_example.ctl"),
      reps=input$SSS_reps,
      seed.in=input$SSS_seed,
      Dep.in=Dep.in_sss,
      M.in=M.in_sss,
      SR_type=3,
      h.in=h.in_sss,
      FMSY_M.in=c(-1,0.5,0.1),
      BMSY_B0.in=c(-1,0.5,0.1),
      Linf.k.cor=input$Linf_k_cor_sss,
      Linf.in=Linf.in_sss,
      k.in=k.in_sss,
      t0.in=t0.in_sss,
      Zfrac.Beta.in=c(-99,0.2,0.6,-99,0.5,2),
      R_start=c(0,input$lnR0_sss),
      doR0.loop=c(1,round(input$lnR0_sss*0.5),round(input$lnR0_sss*1.5),(round(input$lnR0_sss*1.3)-round(input$lnR0_sss*0.5))/10),
      sum_age=0,
      ts_yrs=c(input$styr,input$endyr),
      pop.ltbins=NA,
      #ofl_yrs=c(input$endyr+1,input$endyr+2),
      sexes=T,
      BH_FMSY_comp=F,
      OStype=input$OS_choice)
#save(SSS.out)
show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[3],text="Process model output")

if(exists(load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))))
  {
  output$SSS_priors_post<-renderPlot({
      if(exists(load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))))
      {
      load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))
      sss.M.f<-rbind(data.frame(value=SSS.out$Prior$M_f,type="prior",metric="Female M"),data.frame(value=SSS.out$Post$M_f,type="post",metric="Female M"))
      sss.M.m<-rbind(data.frame(value=SSS.out$Prior$M_m,type="prior",metric="Male M"),data.frame(value=SSS.out$Post$M_m,type="post",metric="Male M"))
      sss.h<-rbind(data.frame(value=SSS.out$Prior$h,type="prior",metric="h"),data.frame(value=SSS.out$Post$h,type="post",metric="h"))
      sss.Dep<-rbind(data.frame(value=SSS.out$Prior$Dep,type="prior",metric="Dep"),data.frame(value=SSS.out$Post$Dep.Obs,type="post",metric="Dep"))
      sss.vals.out<-rbind(sss.M.f,sss.M.m,sss.h,sss.Dep)
      
      ggplot(sss.vals.out,aes(x=value,color=type,fill=type))+
        geom_histogram(position="dodge",alpha=0.5)+
        theme(legend.position="bottom")+
        theme(legend.title=element_blank())+
        facet_grid(~metric,scales = "free")
  #    Mf.plot<-ggplot(sss.M.f,aes(x=value,color=type))+geom_histogram(position="dodge",alpha=0.5,fill="white")
  #    Mm.plot<-ggplot(sss.M.m,aes(x=value,color=type))+geom_histogram(position="dodge",alpha=0.5,fill="white")
  #    h.plot<-ggplot(sss.h,aes(x=value,color=type))+geom_histogram(position="dodge",alpha=0.5,fill="white")
  #    Dep.plot<-ggplot(sss.Dep,aes(x=value,color=type))+geom_histogram(position="dodge",alpha=0.5,fill="white")
        }
      else{return(NULL)}
    })  

  output$SSS_growth_priors_post<-renderPlot({
      if(exists(load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))))
      {
      load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))
      sss.L1_f<-rbind(data.frame(value=SSS.out$Prior$L1_f,type="prior",metric="Female L1"),data.frame(value=SSS.out$Post$L1_f,type="post",metric="Female L1"))
      sss.Linf_f<-rbind(data.frame(value=SSS.out$Prior$Linf_f,type="prior",metric="Female Linf"),data.frame(value=SSS.out$Post$Linf_f,type="post",metric="Female Linf"))
      sss.k_f<-rbind(data.frame(value=SSS.out$Prior$k_f,type="prior",metric="Female k"),data.frame(value=SSS.out$Post$k_f,type="post",metric="Female k"))
      sss.L1_m<-rbind(data.frame(value=SSS.out$Prior$L1_m,type="prior",metric="Male L1"),data.frame(value=SSS.out$Post$L1_m,type="post",metric="Male L1"))
      sss.Linf_m<-rbind(data.frame(value=SSS.out$Prior$Linf_m,type="prior",metric="Male Linf"),data.frame(value=SSS.out$Post$Linf_m,type="post",metric="Male Linf"))
      sss.k_m<-rbind(data.frame(value=SSS.out$Prior$k_m,type="prior",metric="Male k"),data.frame(value=SSS.out$Post$k_m,type="post",metric="Male k"))
      sss.vals.growth.out<-rbind(sss.L1_f,sss.Linf_f,sss.k_f,sss.L1_m,sss.Linf_m,sss.k_m)
      
      ggplot(sss.vals.growth.out,aes(x=value,color=type,fill=type))+
        geom_histogram(position="dodge",alpha=0.5)+
        theme(legend.position="bottom")+
        theme(legend.title=element_blank())+
        facet_wrap(~metric,scales = "free") 
      }
      else{return(NULL)}
    })  

  output$SSS_OFL_plot<-renderPlot({
      if(exists(load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))))
      {
      load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))
      ofl.years<-as.numeric(unique(reshape2::melt(SSS.out$OFL)$Var2))
      ggplot(reshape2::melt(SSS.out$OFL),aes(Var2,value,group=Var2))+
          geom_boxplot(fill="#236192")+
          scale_x_continuous(breaks=ofl.years,labels=as.character(ofl.years))+
          ylab("OFL (mt)")+
          xlab("Year")
      }
      else{return(NULL)}
    })  

  output$SSS_ABC_plot<-renderPlot({
      if(exists(load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))))
      {
      load(paste0("Scenarios/",input$Scenario_name,"/SSS_out.DMP"))
      abc.years<-as.numeric(unique(reshape2::melt(SSS.out$ABC)$Var2))
      ggplot(reshape2::melt(SSS.out$ABC),aes(Var2,value,group=Var2))+
          geom_boxplot(fill="#658D1B")+
          scale_x_continuous(breaks=abc.years,labels=as.character(abc.years))+
          ylab("ABC (mt)")+
          xlab("Year")
      }
      else{return(NULL)}
    })  
  }
    remove_modal_spinner()
  }
})

###############
### END SSS ###
###############

##################################################################
### PREPARE FILES and RUN Length and Age-based Stock Synthsis ###
##################################################################
SS.file.update<-observeEvent(input$run_SS,{
		# if(is.null(inFile) | !anyNA(inp$
		#							styr,ndyr,
		# 							input$Nages,
		# 							input$M_f,
		# 							input$k_f,
		# 							input$Linf_f,
		# 							input$t0_f,
		# 							input$L50_f,
		# 							input$L95_f,
		# 							input$M_m,
		# 							input$k_m,
		# 							input$Linf_m,
		# 							input$t0_m,
		# 							input$L50_m,
		# 							input$L95_m,
		# 							))
		# {

    updateTabsetPanel(session, "tabs",
      selected = '1')

		# progress <- shiny::Progress$new(session, min=1, max=2)
  #          on.exit(progress$close())
       
  #          progress$set(message = 'Model run in progress',
  #                       detail = '')
       
  #          for (i in 1:2) {
  #            progress$set(value = i)
  #            Sys.sleep(0.5)
  #          }

if(!any(input$use_par,input$use_datanew,input$use_controlnew,input$user_model))
#if(which(c(input$use_par,input$use_datanew,input$use_datanew_user,input$use_controlnew,input$use_controlnew_user,input$user_model))!=0)
  {
      #Copy and move files
	  	if(file.exists(paste0("Scenarios/",input$Scenario_name)))
			{
				unlink(paste0("Scenarios/",input$Scenario_name),recursive=TRUE)   #Deletes previous run
#				file.remove(paste0(getwd(),"/Scenarios/",input$Scenario_name))
			}
	  	if(input$Ct_F_LO_select=="Estimate F" & is.null(rv.Ct$data)){
          file.copy(paste0("SS_LO_F_files"),paste0("Scenarios"),recursive=TRUE,overwrite=TRUE)
          file.rename(paste0("Scenarios/SS_LO_F_files"), paste0("Scenarios/",input$Scenario_name))
        }
      else{
        file.copy(paste0("SS_LB_files"),paste0("Scenarios"),recursive=TRUE,overwrite=TRUE)
		    file.rename(paste0("Scenarios/SS_LB_files"), paste0("Scenarios/",input$Scenario_name))
        }
  }

#BOOKMARKING: copy inputs rds and uploaded csvs into scenarios folder
saveInputs(input, bookmarkFilePath(), latestBookmarkURL(), session)

  # session$doBookmark()
  # bookmarkInputPath <- file.path(dirname(bookmarkFilePath()), latestBookmarkURL(), "input.rds")
  
  # if(!dir.exists(dirname(file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds")))) {
  #   dir.create(dirname(file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds")), recursive = TRUE)
  # }
  
  # file.copy(from = bookmarkInputPath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", "input.rds"), overwrite=TRUE)
  # for(i in 1:4){
  #   if(!is.null(input[[paste0("file",i)]])){
  #     file.copy(from = input[[paste0("file",i)]]$datapath,  to = file.path("Scenarios", input$Scenario_name, "Scenario_Inputs", input[[paste0("file",i)]]$name), overwrite=TRUE)
  #   }
  # }
  
  # dir_delete(bookmarkFilePath()) #delete bookmark created from session$doBookmark as we are just using it to create the query string and grab the filepath

# if(!input$use_customfile)
#   {
#   }
		#Read data and control files
    if(!input$user_model)
    {
      data.file<-SS_readdat(paste0("Scenarios/",input$Scenario_name,"/datafile.dat")) 
      ctl.file<-SS_readctl(paste0("Scenarios/",input$Scenario_name,"/controlfile.ctl"),use_datlist = TRUE, datlist=data.file)       
    }

   
# if(input$use_datanew)
#   {
#     data.file<-SS_readdat(paste0("Scenarios/",input$Scenario_name,"/data_echo.ss_new")) 
#   }

# if(input$use_controlnew)
#   {
#     data.file<-SS_readdat(paste0("Scenarios/",input$Scenario_name,"/data_echo.ss_new")) 
#     ctl.file<-SS_readctl(paste0("Scenarios/",input$Scenario_name,"/control.ss_new"),use_datlist = TRUE, datlist=data.file) 
#   }

		# data.file<-SS_readdat(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.dat")) 
		# ctl.file<-SS_readctl(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.ctl"),use_datlist = TRUE, datlist=data.file) 
  #if(input$Ct_F_LO_select=="Estimate F" & is.null(rv.Ct$data))
  #  {
  #    data.file<-SS_readdat(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.dat")) 
  #    ctl.file<-SS_readctl(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.ctl"),use_datlist = TRUE, datlist=data.file)       
  #  }

if(!input$user_model)
{
#Prepare inputs to evaluate any errors
      Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50,","))))
      Sel50_phase<-as.numeric(trimws(unlist(strsplit(input$Sel50_phase,","))))
      Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))
      Selpeak_phase<-as.numeric(trimws(unlist(strsplit(input$Selpeak_phase,","))))
      bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]
      minmaxbin<-c(Selpeak[1]-min(data.file$lbin_vector),max(data.file$lbin_vector)-Selpeak[1])
      #sel.inputs.comps<-length(Sel50)-length(Sel50_phase)-length(Selpeak)-length(Selpeak_phase)
      sel.inputs.lts<-c(length(Sel50),length(Sel50_phase),length(Selpeak),length(Selpeak_phase))
      Nfleets<-max(ncol(rv.Ct$data)-1,rv.Lt$data[,3],rv.Age$data[,3],rv.Index$data[,3])


if(input$Sel_choice=="Dome-shaped")
    {       
      PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))
      PeakDesc_phase<-as.numeric(trimws(unlist(strsplit(input$PeakDesc_phase,","))))
      LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))
      LtPeakFinal_phase<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_phase,","))))
      FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))
      FinalSel_phase<-as.numeric(trimws(unlist(strsplit(input$FinalSel_phase,","))))
      minmaxbin<-c(Selpeak[1]-min(data.file$lbin_vector),max(data.file$lbin_vector)-Selpeak[1])
      sel.inputs.lts<-c(length(Sel50),length(Sel50_phase),length(Selpeak),length(Selpeak_phase),length(PeakDesc),length(PeakDesc_phase),length(LtPeakFinal),length(LtPeakFinal_phase),length(FinalSel),length(FinalSel_phase))
    }

#Search for errors in inputs      
      #Throw warning if not enough selectivity inputs
      if(!all(Nfleets==sel.inputs.lts))
      {
      #Throw warning if not enough selectivity inputs
         sendSweetAlert(
          session = session,
          title = "Selectivity input warning",
          text = "Please check to see if you have provided filled in the inputs correctly. Especially check selectivity for missing fleets (both in parameter and phases). Total fleets includes fishing fleets and surveys.",
          type = "error")
         remove_modal_spinner()
      }


  if(all(Nfleets==sel.inputs.lts))
  {     
        checkmod<-1  #add object to verify no errors in inputs and model can be run
        show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[2],text="Model run in progress")
  #if(!input$use_par)
  #{
    #if(all(!input$use_datanew,!input$user_model))
    #{
		#Read, edit then write new DATA file
		data.file$styr<-input$styr
		data.file$endyr<-input$endyr
		data.file$Nages<-input$Nages_in #Nages()
    if(is.null(rv.Ct$data)){data.file$spawn_month<-input$rec_month_LO}
    if(!is.null(rv.Ct$data) & input$est_parms==F){data.file$spawn_month<-input$rec_month}
    if(!is.null(rv.Ct$data) & input$est_parms==T){data.file$spawn_month<-input$rec_month_est}
    catch.fleets.Ct<-catch.fleets.Lt<-catch.fleets.Age<-NA
    if(!is.null(rv.Ct$data)){catch.fleets.Ct<-max(ncol(rv.Ct$data)-1)}
    if(all(!is.null(rv.Lt$data),is.null(rv.Ct$data))){catch.fleets.Lt<-max(rv.Lt$data[,3])}
    if(all(!is.null(rv.Age$data),is.null(rv.Ct$data))){catch.fleets.Age<-max(rv.Age$data[,3])}
    catch.fleets<-max(catch.fleets.Ct,catch.fleets.Lt,catch.fleets.Age,na.rm=TRUE)
    data.file$Nfleets<-max(catch.fleets,rv.Lt$data[,3],rv.Age$data[,3],rv.Index$data[,3])

#########
#Catches#
#########
		if (is.null(rv.Ct$data)) 
		{
		#inFile<- rv.Lt$data
		Lt.comp.data<-rv.Lt$data
		Age.comp.data<- rv.Age$data
    #data.file$Nfleets<-max(Lt.comp.data[,2],Age.comp.data[,2])
		if(input$Ct_F_LO_select=="Estimate F"){data.file$bycatch_fleet_info[4:5]<-c(input$styr,input$endyr)}
    if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				if(input$Ct_F_LO_select=="Estimate F"){data.file$bycatch_fleet_info<-rbind(data.file$bycatch_fleet_info,data.file$bycatch_fleet_info[1,])}
 			}
      if(input$Ct_F_LO_select=="Estimate F"){data.file$bycatch_fleet_info[,1]<-c(1:data.file$Nfleets)}
		}
		year.in<-input$styr:input$endyr
		catch.cols<-colnames(data.file$catch)
		catch_temp<-list()
		if(catch.fleets==1){catch.level<-1000}
    if(catch.fleets>1){
        catch.level<-as.numeric(trimws(unlist(strsplit(input$Wt_fleet_Ct,","))))
        catch.level<-catch.level/sum(catch.level)*1000
      }
    for(i in 1:catch.fleets)
		{
		catch_temp[[i]]<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(i,length(year.in)+1),
						c(catch.level[i],rep(catch.level[i],length(year.in))),
						c(0.01,rep(1000,length(year.in)))
						)			
		}
		data.file$catch<-list.rbind(catch_temp)
		colnames(data.file$catch)<-catch.cols
		}

		if(!is.null(rv.Ct$data))
		{
		Catch.data<-rv.Ct$data
		#data.file$Nfleets<-max(ncol(Catch.data)-1,data.file$Nfleets)
		year.in<-Catch.data[,1]
		catch.cols<-colnames(data.file$catch)
		catch_temp<-list()
		for(i in 1:catch.fleets)
		{
			catch_temp[[i]]<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(i,length(year.in)+1),
						c(0.00000000000000000001,Catch.data[,i+1]),
						rep(0.01,length(year.in)+1)
						)
		}
		data.file$catch<-list.rbind(catch_temp)
		colnames(data.file$catch)<-catch.cols
		#Add user equilibirium catch
    data.file$catch$catch[data.file$catch$year==-999]<-as.numeric(trimws(unlist(strsplit(input$Eq_Ct_fleet,",")))) 
    data.file$catch$catch[data.file$catch$year==-999&data.file$catch$catch==0]<-0.00000000000000000001
    }


#Index data
    if (!is.null(rv.Index$data)) {
    Index.data<-rv.Index$data
    data.file$N_cpue<-unique(rv.Index$data[,3])
    data.file$CPUE<-data.frame(year=rv.Index$data[,1],seas=rv.Index$data[,2],index=rv.Index$data[,3],obs=rv.Index$data[,4],se_log=rv.Index$data[,5])
    }


#########################
#Length composition data#
#########################
#Population length data bins
  data.file$binwidth<-2
  if(!is.null(rv.Lt$data)){data.file$binwidth<-as.numeric(colnames(rv.Lt$data)[7])-as.numeric(colnames(rv.Lt$data)[6])}
  data.file$minimum_size<-2
  if(!is.null(rv.Lt$data)){data.file$minimum_size<-as.numeric(colnames(rv.Lt$data)[6])}
  max.bin.in<-2*(round((Linf()+(Linf()*0.25))/2))+2 #0.2326
  data.file$maximum_size<-max.bin.in
  # if(input$advance_ss_click)
  #   {
      data.file$binwidth<-input$lt_bin_size
      data.file$minimum_size<-input$lt_min_bin
      data.file$maximum_size<-input$lt_max_bin 
    # }
		#inFile<- rv.Lt$data

		if (is.null(rv.Lt$data)) {
		  if(input$est_parms==FALSE){Linf_bins<-input$Linf_f_fix}
		  if(input$est_parms==TRUE){Linf_bins<-input$Linf_f_mean}
		  data.file$binwidth<-2
		  data.file$minimum_size<-2
      max.bin.in<-2*(round((Linf()+(Linf()*0.25))/2))+2 #0.2326
      data.file$maximum_size<-max.bin.in
      data.file$lbin_vector<-seq(data.file$minimum_size,data.file$maximum_size,data.file$binwidth)
      data.file$N_lbins<-length(data.file$lbin_vector)
      data.file$lencomp<-NULL  
      }
      
		if (!is.null(rv.Lt$data)) {
    Lt.comp.data<-rv.Lt$data
    data.file$N_lbins<-ncol(Lt.comp.data)-5
    data.file$lbin_vector<-as.numeric(colnames(rv.Lt$data)[6:ncol(rv.Lt$data)]) #as.numeric(colnames(Lt.comp.data[,5:ncol(Lt.comp.data)]))
    if(data.file$maximum_size<max(data.file$lbin_vector)){data.file$maximum_size<-(2*round(max(data.file$lbin_vector)/2))+2}
    lt.data.names<-c(colnames(data.file$lencomp[,1:6]),paste0("f",data.file$lbin_vector),paste0("m",data.file$lbin_vector))
    lt.data.females<-lt.data.males<-lt.data.unknowns<-lt.data.sex3<-data.frame(matrix(rep(NA,length(lt.data.names)),nrow=1))
    colnames(Lt.comp.data)[1:5]<-c("Year","Month","Fleet","Sex","Nsamps")
    #female lengths
    if(nrow(subset(Lt.comp.data,Sex==1))>0){
    Lt.comp.data_female<-subset(Lt.comp.data,Sex==1 & Nsamps>0) 
    samp.yrs<-Lt.comp.data_female[,1]
    lt.data.females<-data.frame(cbind(samp.yrs,
        Lt.comp.data_female[,2],
        Lt.comp.data_female[,3],
        Lt.comp.data_female[,4],
        rep(0,length(samp.yrs)),
        Lt.comp.data_female[,5],
        Lt.comp.data_female[,6:ncol(Lt.comp.data_female)],
        Lt.comp.data_female[,6:ncol(Lt.comp.data_female)]*0)
        )
    }
    #male lengths
    if(nrow(subset(Lt.comp.data,Sex==2))>0){
      Lt.comp.data_male<-subset(Lt.comp.data,Sex==2 & Nsamps>0)
      samp.yrs_males<-Lt.comp.data_male[,1]
      lt.data.males<-data.frame(cbind(samp.yrs_males,
        Lt.comp.data_male[,2],
        Lt.comp.data_male[,3],
        Lt.comp.data_male[,4],
        rep(0,length(samp.yrs_males)),
        Lt.comp.data_male[,5],
        Lt.comp.data_male[,6:ncol(Lt.comp.data_male)]*0,
        Lt.comp.data_male[,6:ncol(Lt.comp.data_male)])
        )
      }
    #unknown sex lengths
    if(nrow(subset(Lt.comp.data,Sex==0))>0){
      Lt.comp.data_unknown<-subset(Lt.comp.data,Sex==0 & Nsamps>0)
      samp.yrs_unknown<-Lt.comp.data_unknown[,1]
      lt.data.unknowns<-data.frame(cbind(samp.yrs_unknown,
        Lt.comp.data_unknown[,2],
        Lt.comp.data_unknown[,3],
        Lt.comp.data_unknown[,4],
        rep(0,length(samp.yrs_unknown)),
        Lt.comp.data_unknown[,5],
        Lt.comp.data_unknown[,6:ncol(Lt.comp.data_unknown)],
        Lt.comp.data_unknown[,6:ncol(Lt.comp.data_unknown)]*0)
        )
      }
    #Maintain sample sex ratio
     if(input$Sex3options){
     if(input$Sex3){
      yrsfleet_females<-paste0(Lt.comp.data_female[,1],Lt.comp.data_female[,3])
      yrsfleet_males<-paste0(Lt.comp.data_male[,1],Lt.comp.data_male[,3])
      #Match years
      #samp.yrs_sex3<-samp.yrs_females[match(samp.yrs_males,samp.yrs_females)]
      sex3_match_female<-yrsfleet_females%in%yrsfleet_males
      sex3_match_male<-yrsfleet_males%in%yrsfleet_females
      #Subset years
      Lt.comp.data_female_sex3<-Lt.comp.data_female[sex3_match_female,]
      Lt.comp.data_male_sex3<-Lt.comp.data_male[sex3_match_male,]
      lt.data.sex3<-data.frame(cbind(Lt.comp.data_female_sex3[,1],
        Lt.comp.data_female_sex3[,2],
        Lt.comp.data_female_sex3[,3],
        rep(3,nrow(Lt.comp.data_female_sex3)),
        rep(0,nrow(Lt.comp.data_female_sex3)),
        Lt.comp.data_female_sex3[,5]+Lt.comp.data_male_sex3[,5],
        Lt.comp.data_female_sex3[,6:ncol(Lt.comp.data_female_sex3)],
        Lt.comp.data_male_sex3[,6:ncol(Lt.comp.data_male_sex3)])
        )
      lt.data.females<-lt.data.females[!sex3_match_female,]
      lt.data.males<-lt.data.males[!sex3_match_male,]
       }
     }
    colnames(lt.data.females)<-colnames(lt.data.males)<-colnames(lt.data.unknowns)<-colnames(lt.data.sex3)<-lt.data.names
    
    data.file$lencomp<-na.omit(rbind(lt.data.unknowns,lt.data.females,lt.data.males,lt.data.sex3))      
    }
		#}
		#else{
		# data.file$lencomp<-data.frame(matrix(cbind(samp.yrs,
		# 	rep(1,length(samp.yrs)),
		# 	rep(1,length(samp.yrs)),
		# 	rep(1,length(samp.yrs)),
		# 	rep(0,length(samp.yrs)),
		# 	colSums(Lt.comp.data[-1]),
		# 	t(Lt.comp.data)[-1,],
		# 	t(Lt.comp.data)[-1,]*0),
		# 	nrow=length(samp.yrs),
		# 	ncol=6+length(Lt.comp.data[,1])*2,
		# 	byrow=FALSE))[,,drop=FALSE]			
		# }
#		colnames(data.file$lencomp)<-lt.data.names

######################
#Age composition data#
######################
		Age.comp.data<-rv.Age$data
    Plus_age<-input$Nages_in
    if (is.null(Age.comp.data)) 
    {
      data.file$N_agebins<-Plus_age
      data.file$agebin_vector<-0:(Plus_age-1)    
      data.file$ageerror<-data.frame(matrix(c(rep(-1,(Plus_age+1)),rep(0.001,(Plus_age+1))),2,(Plus_age+1),byrow=TRUE))
      colnames(data.file$ageerror)<-paste0("age",0:Plus_age)         
    }

    if (!is.null(Age.comp.data))
    {
      
      if(ncol(Age.comp.data)-8>(Plus_age+1))
      {
         sendSweetAlert(
          session = session,
          title = "Age data input warning",
          text = "The max age bin in the age composition data is more than the plus group. Adjust the plus group input to be equal to or more than the maximum age bin.",
          type = "warning")
         remove_modal_spinner()
      }
      
      data.file$N_agebins<-ncol(Age.comp.data)-8
      data.file$agebin_vector<-as.numeric(colnames(Age.comp.data[,9:ncol(Age.comp.data)]))
      data.file$ageerror<-data.frame(matrix(c(rep(-1,(Plus_age+1)),rep(0.001,(Plus_age+1))),2,(Plus_age+1),byrow=TRUE))
      
      if(!is.null(input$Ageing_error_choice)){       
      if(input$Ageing_error_choice)
        {
          data.file$ageerror<-data.frame((rv.AgeErr$data))

       if(ncol(data.frame((rv.AgeErr$data)))!=(Plus_age+1))
      {
         sendSweetAlert(
          session = session,
          title = "Ageing error data input warning",
          text = "The maximum age in the ageing error matrix does not match the popuation age plus group. Please makes sure these match.",
          type = "error")
         remove_modal_spinner()
      }

          data.file$N_ageerror_definitions<-nrow(rv.AgeErr$data)/2
        }
      }

      #Label object for r4ss
      colnames(data.file$ageerror)<-paste0("age",0:Plus_age)         
      rownames(data.file$ageerror)<-c(1:nrow(data.file$ageerror))
      # data.file$ageerror<-data.frame(matrix(c(rep(-1,(Nages()+1)),rep(0.001,(Nages()+1))),2,(Nages()+1),byrow=TRUE))
      # colnames(data.file$ageerror)<-paste0("age",1:Nages())         
      age.data.names<-c(c("Yr","Month","Fleet","Sex","Part","Ageerr","Lbin_lo","Lbin_hi","Nsamp"),paste0("f",data.file$agebin_vector),paste0("m",data.file$agebin_vector))
      age.data.females<-age.data.males<-age.data.unknowns<-age.data.sex3<-data.frame(matrix(rep(NA,length(age.data.names)),nrow=1))
      colnames(Age.comp.data)[1:8]<-c("Year","Month","Fleet","Sex","AgeErr","Lbin_low","Lbin_hi","Nsamps")
    #female ages
    if(nrow(subset(Age.comp.data,Sex==1))>0){
      Age.comp.data_female<-subset(Age.comp.data,Sex==1 & Nsamps>0) 
      samp.yrs_females<-Age.comp.data_female[,1]
      age.data.females<-data.frame(cbind(samp.yrs_females,
        Age.comp.data_female[,2],
        Age.comp.data_female[,3],
        Age.comp.data_female[,4],
        rep(0,length(samp.yrs_females)),
        Age.comp.data_female[,5],
        Age.comp.data_female[,6],
        Age.comp.data_female[,7],
        Age.comp.data_female[,8],
        Age.comp.data_female[,9:ncol(Age.comp.data_female)],
        Age.comp.data_female[,9:ncol(Age.comp.data_female)]*0)
        )
    }
    #male ages
    if(nrow(subset(Age.comp.data,Sex==2))>0){
      Age.comp.data_male<-subset(Age.comp.data,Sex==2 & Nsamps>0)
      samp.yrs_males<-Age.comp.data_male[,1]
      age.data.males<-data.frame(cbind(samp.yrs_males,
        Age.comp.data_male[,2],
        Age.comp.data_male[,3],
        Age.comp.data_male[,4],
        rep(0,length(samp.yrs_males)),
        Age.comp.data_male[,5],
        Age.comp.data_male[,6],
        Age.comp.data_male[,7],
        Age.comp.data_male[,8],
        Age.comp.data_male[,9:ncol(Age.comp.data_male)]*0,
        Age.comp.data_male[,9:ncol(Age.comp.data_male)])
        )
      }
    #unknown sex ages
    if(nrow(subset(Age.comp.data,Sex==0))>0){
      Age.comp.data_unknown<-subset(Age.comp.data,Sex==0 & Nsamps>0)
      samp.yrs_unknown<-Age.comp.data_unknown[,1]
      age.data.unknowns<-data.frame(cbind(samp.yrs_unknown,
        Age.comp.data_unknown[,2],
        Age.comp.data_unknown[,3],
        Age.comp.data_unknown[,4],
        rep(0,length(samp.yrs_unknown)),
        Age.comp.data_unknown[,5],
        Age.comp.data_unknown[,6],
        Age.comp.data_unknown[,7],
        Age.comp.data_unknown[,8],
        Age.comp.data_unknown[,9:ncol(Age.comp.data_unknown)],
        Age.comp.data_unknown[,9:ncol(Age.comp.data_unknown)]*0)
        )
      }

    #Maintain sample sex ratio
     if(input$Sex3options){
     if(input$AgeSex3){
      age_yrsfleetagetype_females<-paste0(Age.comp.data_female[,1],Age.comp.data_female[,3],Age.comp.data_female[,6])
      age_yrsfleetagetype_males<-paste0(Age.comp.data_male[,1],Age.comp.data_male[,3],Age.comp.data_male[,6])
      #Match years
      age_sex3_match_female<-age_yrsfleetagetype_females%in%age_yrsfleetagetype_males
      age_sex3_match_male<-age_yrsfleetagetype_males%in%age_yrsfleetagetype_females
      #Subset years
      Age.comp.data_female_sex3<-Age.comp.data_female[age_sex3_match_female,]
      Age.comp.data_male_sex3<-Age.comp.data_male[age_sex3_match_male,]
      age.data.sex3<-data.frame(cbind(Age.comp.data_female_sex3[,1],
        Age.comp.data_female_sex3[,2],
        Age.comp.data_female_sex3[,3],
        rep(3,nrow(Age.comp.data_female_sex3)),
        rep(0,nrow(Age.comp.data_female_sex3)),
        Age.comp.data_female_sex3[,5],
        Age.comp.data_female_sex3[,6],
        Age.comp.data_female_sex3[,7],
        Age.comp.data_female_sex3[,8]+Age.comp.data_male_sex3[,8],
        Age.comp.data_female_sex3[,9:ncol(Age.comp.data_female_sex3)],
        Age.comp.data_male_sex3[,9:ncol(Age.comp.data_male_sex3)])
        )
      age.data.females<-age.data.females[!age_sex3_match_female,]
      age.data.males<-age.data.males[!age_sex3_match_male,]
       }
     }

    #if(nrow(subset(Age.comp.data,Sex==0))>0){age.data.unknowns<-data.frame(cbind(
    #  age.data.unknowns,
    #  Age.comp.data[1,7:ncol(Age.comp.data_unknown)],
    #    Age.comp.data[1,7:ncol(Age.comp.data_unknown)]*0))
    #  }
    colnames(age.data.females)<-colnames(age.data.males)<-colnames(age.data.unknowns)<-colnames(age.data.sex3)<-age.data.names
    data.file$agecomp<-na.omit(rbind(age.data.females,age.data.males,age.data.unknowns,age.data.sex3))
    }
  


  # 	inFile_age<- rv.Age$data
		# if (is.null(inFile_age)){
		# data.file$N_agebins<-Nages()
		# data.file$agebin_vector<-1:Nages()		
		# data.file$ageerror<-data.frame(matrix(c(rep(-1,(Nages()+1)),rep(0.001,(Nages()+1))),2,(Nages()+1),byrow=TRUE))
		# colnames(data.file$ageerror)<-paste0("age",1:Nages())		
		# 	}
		# if (!is.null(inFile_age)){
		# Age.comp.data<-rv.Age$data
		# age.classes<-nrow(Age.comp.data)
		# data.file$N_agebins<-age.classes
		# data.file$agebin_vector<-Age.comp.data[,1]
		# data.file$ageerror<-data.frame(matrix(c(rep(-1,(age.classes+1)),rep(0.001,(age.classes+1))),2,(age.classes+1),byrow=TRUE))		
		# colnames(data.file$ageerror)<-paste0("age",1:Nages())		
		# age.samp.yrs<-as.numeric(colnames(Age.comp.data)[-1])
		# age.data.names<-c(c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi","Nsamp"),paste0("f",Age.comp.data[,1]),paste0("m",Age.comp.data[,1]))
		# if(length(age.samp.yrs)==1){
		# 	data.file$agecomp<-data.frame(matrix(c(samp.yrs,
		# 	rep(1,length(age.samp.yrs)),
		# 	rep(1,length(age.samp.yrs)),
		# 	rep(1,length(age.samp.yrs)),
		# 	rep(0,length(age.samp.yrs)),
		# 	rep(-1,length(age.samp.yrs)),
		# 	rep(-1,length(age.samp.yrs)),
		# 	rep(-1,length(age.samp.yrs)),
		# 	colSums(Age.comp.data[-1]),
		# 	t(Age.comp.data)[-1,],
		# 	t(Age.comp.data)[-1,]*0),
		# 	nrow=length(age.samp.yrs),
		# 	ncol=9+length(Age.comp.data[,1])*2,
		# 	byrow=FALSE))[,,drop=FALSE]
		# }
		# else{
		# data.file$agecomp<-data.frame(matrix(cbind(samp.yrs,
		# 	rep(1,length(age.samp.yrs)),
		# 	rep(1,length(age.samp.yrs)),
		# 	rep(1,length(age.samp.yrs)),
		# 	rep(0,length(age.samp.yrs)),
		# 	rep(-1,length(age.samp.yrs)),
		# 	rep(-1,length(age.samp.yrs)),
		# 	rep(-1,length(age.samp.yrs)),
		# 	colSums(Age.comp.data[-1]),
		# 	t(Age.comp.data)[-1,],
		# 	t(Age.comp.data)[-1,]*0),
		# 	nrow=length(age.samp.yrs),
		# 	ncol=9+length(Age.comp.data[,1])*2,
		# 	byrow=FALSE))[,,drop=FALSE]			
		# }
		# colnames(data.file$agecomp)<-age.data.names
		# }

#Create data info 
  if(data.file$Nfleets>1){
      for(i in 1:(data.file$Nfleets-1))
      {
        data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
        data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
        data.file$len_info<-rbind(data.file$len_info,data.file$len_info[1,])
        data.file$age_info<-rbind(data.file$age_info,data.file$age_info[1,])
      }
     
      #Set Dirichlet on
#      data.file$age_info[,5]<-data.file$len_info[,5]<-1

      #Set up the correct fleet enumeration
#      data.file$len_info[,6]<-1:data.file$Nfleets #Used for Dirichlet set-up
#      data.file$age_info[,6]<-(data.file$Nfleets+1):(2*data.file$Nfleets) #Used for Dirichlet set-up

#Survey names
      if(is.null(rv.Ct$data)){data.file$fleetinfo$fleetname<-paste0("Fishery",1:data.file$Nfleets)}
      if(!is.null(rv.Ct$data))
        {
          fishery.names<-gsub(" ","",colnames(rv.Ct$data)[-1])
          if(!is.null(rv.Index$data)&data.file$Nfleets>catch.fleets)
            {
              Surveyonly<-subset(rv.Index$data,Fleet>catch.fleets)
              fleet.survey.names<-unique(c(fishery.names,unique(Surveyonly[,6])))
              survey.fleets<-unique(Surveyonly[,3])      
              data.file$fleetinfo$fleetname<-fleet.survey.names 
            }
          if(is.null(rv.Index$data)|all(!is.null(rv.Index$data)&data.file$Nfleets==catch.fleets)){data.file$fleetinfo$fleetname[1:length(fishery.names)]<-fishery.names}
          #if(!is.null(rv.Index$data)& max(rv.Index$data[,3])>length(fishery.names)){data.file$fleetinfo[survey.fleets,1]<-3}
          if(length(data.file$fleetinfo$fleetname)>length(fishery.names)){data.file$fleetinfo[c((length(fishery.names)+1):length(data.file$fleetinfo$fleetname)),1]<-3}
        }
       data.file$CPUEinfo[,1]<-1:data.file$Nfleets
     }
  
  if(!is.null(rv.Index$data)&data.file$Nfleets>catch.fleets)
  {
    if(any(fleet.survey.names=="RSS"))
    {
      data.file$CPUEinfo[grep("RSS",fleet.survey.names),2]<-34
    }
  }

#Change survey timing to 1

  data.file$fleetinfo$surveytiming[data.file$fleetinfo$type%in%3]<-1

#Catch units     
    if(input$Ct_units_choice)
    {
      ct.units<-as.numeric(trimws(unlist(strsplit(input$fleet_ct_units,","))))
      #data.file$fleetinfo[ct.units,4]<-2 #use this when just specifying which are fleets are numbers
      data.file$fleetinfo[,4]<-ct.units
    }
    data.file$Comments <- c(data.file$Comments, input$scenario_description_input)
		SS_writedat(data.file,paste0("Scenarios/",input$Scenario_name,"/datafile.dat"),overwrite=TRUE)			
  #}
		####################### END DATA FILE #####################################
##################################################################################
		####################### START CTL FILE ####################################
		#Read, edit then write new CONTROL file
    #if(all(!input$use_controlnew,!input$user_model))
    # {
    #Change to 1 platoon 
    if(!is.null(input$GT1)){if(input$GT1){ctl.file$N_platoon<-1}}
    
    #LENGTH or AGE-ONLY
		if(all(!is.null(c(rv.Lt$data,rv.Age$data,rv.Index$data)),is.null(rv.Ct$data))==TRUE)
    {
    fem_vbgf<-VBGF(input$Linf_f,input$k_f,input$t0_f,c(0:Nages()))
    #Females
    ctl.file$MG_parms[1,3]<-input$M_f         #M
    #ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]   #L0
    ctl.file$Growth_Age_for_L1<-input$t0_f
    ctl.file$MG_parms[2,3:4]<-0               #L0
    ctl.file$MG_parms[3,3:4]<-input$Linf_f    #Linf
    ctl.file$MG_parms[4,3:4]<-input$k_f       #k
    ctl.file$MG_parms[5,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f,","))))[1]   #CV
    ctl.file$MG_parms[6,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f,","))))[2]   #CV
    #Maturity
    ctl.file$MG_parms[9,3:4]<-input$L50_f                                 #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f-input$L50_f)  #Maturity slope
    #ctl.file$MG_parms[11,3:4]<-input$Fec_a_f        #coefficient
    #ctl.file$MG_parms[12,3:4]<- input$Fec_b_f        #exponent  
    #Males
    ctl.file$MG_parms[13,3]<-input$M_f        #M
    #ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]   #L0
    ctl.file$MG_parms[14,3:4]<-0              #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f   #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f      #k
    ctl.file$MG_parms[17,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f,","))))[1]  #CV
    ctl.file$MG_parms[18,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f,","))))[2]  #CV
    #ctl.file$MG_parms[19,3:4]<-input$WLa_f       #coefficient
    #ctl.file$MG_parms[20,3:4]<-input$WLb_f      #exponent  

    if(input$male_offset)
    {
      ctl.file$parameter_offset_approach<-2                         #Change to offset approach
      ctl.file$MG_parms[13,3:4]<-0                                  #M
      ctl.file$MG_parms[14,3:4]<-0                                  #L0
      ctl.file$MG_parms[15,3:4]<-0                                  #Linf
      ctl.file$MG_parms[16,3:4]<-0                                  #k
      ctl.file$MG_parms[17,3:4]<-0                                  #CV
      ctl.file$MG_parms[18,3:4]<-0                                  #CV
      #Weight-length
      ctl.file$MG_parms[19,3:4]<-0                                  #coefficient
      ctl.file$MG_parms[20,3:4]<-0                                  #exponent  
    }

    if(input$male_parms)
      {   
        male_vbgf<-VBGF(input$Linf_m,input$k_m,input$t0_m,c(input$t0_f:Nages()))
        ctl.file$MG_parms[13,3]<-input$M_m           #M
        ctl.file$MG_parms[14,3:4]<-male_vbgf[1]      #L0
        ctl.file$MG_parms[15,3:4]<-input$Linf_m      #Linf
        ctl.file$MG_parms[16,3:4]<-input$k_m         #k
        ctl.file$MG_parms[17,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_m,","))))[1]     #CV
        ctl.file$MG_parms[18,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_m,","))))[2]     #CV
#        ctl.file$MG_parms[19,3:4]<-input$WLa_m       #coefficient
#        ctl.file$MG_parms[20,3:4]<-input$WLb_m       #exponent  
      }
        if(input$Ct_F_LO_select=="Estimate F"){ctl.file$SR_parms[1,7]=-1}  #lnR0
        if(input$Ct_F_LO_select=="Constant Catch"){ctl.file$SR_parms[1,7]=1}  #lnR0
        
        ctl.file$SR_function<-c(3,2)[c("Beverton-Holt","Ricker")==input$SR_choice_LO]
        ctl.file$SR_parms[2,3:4]<-input$h_LO     #steepness

    }

    #LENGTH and CATCH with fixed parameters
    if(all(any(input$est_parms==FALSE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data)),all(!is.null(rv.Index$data),!is.null(rv.Ct$data))))==TRUE)
    {
    fem_vbgf<-VBGF(input$Linf_f_fix,input$k_f_fix,input$t0_f_fix,c(0:Nages()))
    #Females
    ctl.file$MG_parms[1,3]<-input$M_f_fix           #M
    #ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]           #L0
    ctl.file$Growth_Age_for_L1<-input$t0_f_fix
    ctl.file$MG_parms[2,3:4]<-0                     #L0
    ctl.file$MG_parms[3,3:4]<-input$Linf_f_fix      #Linf
    ctl.file$MG_parms[4,3:4]<-input$k_f_fix         #k
    ctl.file$MG_parms[5,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f_fix,","))))[1]     #CV
    ctl.file$MG_parms[6,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f_fix,","))))[2]     #CV
    #Weight-length
    ctl.file$MG_parms[7,3:4]<-input$WLa_f_fix       #coefficient
    ctl.file$MG_parms[8,3:4]<- input$WLb_f_fix      #exponent  
    #Maturity
    ctl.file$MG_parms[9,3:4]<-input$L50_f_fix                                     #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f_fix-input$L50_f_fix)  #Maturity slope
    ctl.file$MG_parms[11,3:4]<-input$Fec_a_f_fix       #coefficient
    ctl.file$MG_parms[12,3:4]<- input$Fec_b_f_fix      #exponent  

    #Males
    ctl.file$MG_parms[13,3]<-input$M_f_fix          #M
    #ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]          #L0
    ctl.file$MG_parms[14,3:4]<-0                    #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f_fix     #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f_fix        #k
    ctl.file$MG_parms[17,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f_fix,","))))[1]    #CV
    ctl.file$MG_parms[18,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_f_fix,","))))[2]    #CV
    ctl.file$MG_parms[19,3:4]<-input$WLa_f_fix       #coefficient
    ctl.file$MG_parms[20,3:4]<- input$WLb_f_fix      #exponent  

    if(input$male_offset_fix)
    {
      ctl.file$parameter_offset_approach<-2                         #Change to offset approach
      ctl.file$MG_parms[13,3:4]<-0                                  #M
      ctl.file$MG_parms[14,3:4]<-0                                  #L0
      ctl.file$MG_parms[15,3:4]<-0                                  #Linf
      ctl.file$MG_parms[16,3:4]<-0                                  #k
      ctl.file$MG_parms[17,3:4]<-0                                  #CV
      ctl.file$MG_parms[18,3:4]<-0                                  #CV
      #Weight-length
      ctl.file$MG_parms[19,3:4]<-0                                  #coefficient
      ctl.file$MG_parms[20,3:4]<-0                                  #exponent  
    }


    if(input$male_parms_fix)
      {   
        male_vbgf<-VBGF(input$Linf_m_fix,input$k_m_fix,input$t0_m_fix,c(input$t0_f_fix:Nages()))
        ctl.file$MG_parms[13,3]<-input$M_m_fix        #M
        ctl.file$MG_parms[14,3:4]<-male_vbgf[1]       #L0
        ctl.file$MG_parms[15,3:4]<-input$Linf_m_fix   #Linf
        ctl.file$MG_parms[16,3:4]<-input$k_m_fix      #k
        ctl.file$MG_parms[17,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_m_fix,","))))[1]  #CV
        ctl.file$MG_parms[18,3:4]<-as.numeric(trimws(unlist(strsplit(input$CV_lt_m_fix,","))))[2]  #CV
        #Weight-length
        ctl.file$MG_parms[19,3:4]<-input$WLa_m_fix       #coefficient
        ctl.file$MG_parms[20,3:4]<-input$WLb_m_fix       #exponent  
      }
      
    #S-R
    ctl.file$SR_function<-c(3,2)[c("Beverton-Holt","Ricker")==input$SR_choice_fixed]
    ctl.file$SR_parms[1,3:4]<-input$lnR0  #lnR0
    ctl.file$SR_parms[2,3:4]<-input$h     #steepnes
    }

    #LENGTH and CATCH with estimated parameters
    if(all(any(input$est_parms==TRUE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data)),all(!is.null(rv.Index$data),!is.null(rv.Ct$data))))==TRUE)
    {
    fem_vbgf<-VBGF(input$Linf_f_mean,input$k_f_mean,input$t0_f_mean,c(0:Nages()))
    #c("lognormal","truncated normal","uniform","beta")
    prior.name<-c("no prior","symmetric beta", "beta","lognormal","gamma","normal")
    prior.type<-c(0:3,5,6)
    #Females
    #M
    if(input$M_f_prior=="lognormal"){ctl.file$MG_parms[1,3:4]<-c(input$M_f_mean,log(input$M_f_mean))}
    else {ctl.file$MG_parms[1,3:4]<-c(input$M_f_mean,input$M_f_mean)}
    ctl.file$MG_parms[1,5]<-input$M_f_SD                            
    ctl.file$MG_parms[1,6]<-prior.type[prior.name==input$M_f_prior] 
    ctl.file$MG_parms[1,7]<-input$M_f_phase                        
        
    #L0    
    ctl.file$Growth_Age_for_L1<-input$t0_f_mean

    # if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[2,3:4]<-c(fem_vbgf[1],log(fem_vbgf[1]))}
    # else {ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]}
    if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[2,3:4]<-c(0,log(0.0000001))}
    else {ctl.file$MG_parms[2,3:4]<-0}
    ctl.file$MG_parms[2,5]<-input$t0_f_SD             
    ctl.file$MG_parms[2,6]<-prior.type[prior.name==input$t0_f_prior]
    ctl.file$MG_parms[2,7]<-input$t0_f_phase

    #Linf
    if(input$Linf_f_prior=="lognormal"){ctl.file$MG_parms[3,3:4]<-c(input$Linf_f_mean,log(input$Linf_f_mean))}     
    else{ctl.file$MG_parms[3,3:4]<-input$Linf_f_mean}
    ctl.file$MG_parms[3,5]<-input$Linf_f_SD         
    ctl.file$MG_parms[3,6]<-prior.type[prior.name==input$Linf_f_prior]      
    ctl.file$MG_parms[3,7]<-input$Linf_f_phase      

    #k
    if(input$k_f_prior=="lognormal"){ctl.file$MG_parms[4,3:4]<-c(input$k_f_mean,log(input$k_f_mean))}        
    else {ctl.file$MG_parms[4,3:4]<-input$k_f_mean}
    ctl.file$MG_parms[4,5]<-input$k_f_SD            
    ctl.file$MG_parms[4,6]<-prior.type[prior.name==input$k_f_prior]        
    ctl.file$MG_parms[4,7]<-input$k_f_phase         
    
    #CV young
    if(input$CV_lt_f_young_prior=="lognormal"){ctl.file$MG_parms[5,3:4]<-c(input$CV_lt_f_young_mean,log(input$CV_lt_f_young_mean))}     
    else{ctl.file$MG_parms[5,3:4]<-input$CV_lt_f_young_mean}
    ctl.file$MG_parms[5,5]<-input$CV_lt_f_young_SD       
    ctl.file$MG_parms[5,6]<-prior.type[prior.name==input$CV_lt_f_young_prior]       
    ctl.file$MG_parms[5,7]<-input$CV_lt_f_young_phase       
    
    #CV old
    if(input$CV_lt_f_old_prior=="lognormal"){ctl.file$MG_parms[6,3:4]<-c(input$CV_lt_f_old_mean,log(input$CV_lt_f_old_mean))}
    else{ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_old_mean}
    ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_old_mean     
    ctl.file$MG_parms[6,5]<-input$CV_lt_f_old_SD       
    ctl.file$MG_parms[6,6]<-prior.type[prior.name==input$CV_lt_f_old_prior]  
    ctl.file$MG_parms[6,7]<-input$CV_lt_f_old_phase 

    #Weight-length
    ctl.file$MG_parms[7,3:4]<-input$WLa_f_est       #coefficient
    ctl.file$MG_parms[8,3:4]<- input$WLb_f_est      #exponent  

    #Maturity
    ctl.file$MG_parms[9,3:4]<-input$L50_f_est                                     #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f_est-input$L50_f_est)  #Maturity slope
    ctl.file$MG_parms[11,3:4]<-input$Fec_a_f_est       #coefficient
    ctl.file$MG_parms[12,3:4]<- input$Fec_b_f_est      #exponent  
        
    #Males
    ctl.file$MG_parms[13,3:4]<-c(input$M_f_mean,log(input$M_f_mean))    #M
    #ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]                              #L0
    ctl.file$MG_parms[14,3:4]<-0                                        #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f_mean                        #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f_mean                           #k
    ctl.file$MG_parms[17,3:4]<-input$CV_lt_f_old_mean                       #CV
    ctl.file$MG_parms[18,3:4]<-input$CV_lt_f_old_mean                       #CV
    #Weight-length
    ctl.file$MG_parms[19,3:4]<-input$WLa_f_est       #coefficient
    ctl.file$MG_parms[20,3:4]<- input$WLb_f_est      #exponent  
        
 if(input$male_offset_est)
    {
      ctl.file$parameter_offset_approach<-2                         #Change to offset approach
      ctl.file$MG_parms[13,3:4]<-0                                  #M
      ctl.file$MG_parms[14,3:4]<-0                                  #L0
      ctl.file$MG_parms[15,3:4]<-0                                  #Linf
      ctl.file$MG_parms[16,3:4]<-0                                  #k
      ctl.file$MG_parms[17,3:4]<-0                                  #CV
      ctl.file$MG_parms[18,3:4]<-0                                  #CV
      #Weight-length
      ctl.file$MG_parms[19,3:4]<-0                                  #coefficient
      ctl.file$MG_parms[20,3:4]<-0                                  #exponent  
    }

    if(input$male_parms_est)
      {   
        male_vbgf_est<-VBGF(input$Linf_m_mean,input$k_m_mean,input$t0_m_mean,c(input$t0_f_mean:Nages()))

        # ctl.file$MG_parms[13,3]<-input$M_m_mean        #M
        # ctl.file$MG_parms[14,3:4]<-male_vbgf_est[1]    #L0
        # ctl.file$MG_parms[15,3:4]<-input$Linf_m_mean   #Linf
        # ctl.file$MG_parms[16,3:4]<-input$k_m_mean      #k
        # ctl.file$MG_parms[17,3:4]<-input$CV_lt_m_mean  #CV
        # ctl.file$MG_parms[18,3:4]<-input$CV_lt_m_mean  #CV

        #M
        if(input$M_m_prior=="lognormal"){ctl.file$MG_parms[13,3:4]<-c(input$M_m_mean,log(input$M_m_mean))}
        else {ctl.file$MG_parms[13,3:4]<-c(input$M_m_mean,input$M_m_mean)}
        ctl.file$MG_parms[13,5]<-input$M_m_SD                            
        ctl.file$MG_parms[13,6]<-prior.type[prior.name==input$M_m_prior] 
        ctl.file$MG_parms[13,7]<-input$M_m_phase                         
            
        #L0    
        #if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[14,3:4]<-c(male_vbgf_est[1],log(male_vbgf_est[1]+0.000000001))}
        #else {ctl.file$MG_parms[14,3:4]<-male_vbgf_est[1]}
        if(input$t0_m_prior=="lognormal"){ctl.file$MG_parms[14,3:4]<-c(male_vbgf_est[1],log(male_vbgf_est[1]+0.000000001))}
        else {ctl.file$MG_parms[14,3:4]<-male_vbgf_est[1]}
        ctl.file$MG_parms[14,5]<-input$t0_m_SD             
        ctl.file$MG_parms[14,6]<-prior.type[prior.name==input$t0_m_prior]
        ctl.file$MG_parms[14,7]<-input$t0_m_phase

        #Linf
        if(input$Linf_m_prior=="lognormal"){ctl.file$MG_parms[15,3:4]<-c(input$Linf_m_mean,log(input$Linf_m_mean))}     
        else{ctl.file$MG_parms[15,3:4]<-input$Linf_m_mean}
        ctl.file$MG_parms[15,5]<-input$Linf_m_SD         
        ctl.file$MG_parms[15,6]<-prior.type[prior.name==input$Linf_m_prior]      
        ctl.file$MG_parms[15,7]<-input$Linf_m_phase      

        #k
        if(input$k_m_prior=="lognormal"){ctl.file$MG_parms[16,3:4]<-c(input$k_m_mean,log(input$k_m_mean))}        
        else {ctl.file$MG_parms[16,3:4]<-input$k_m_mean}
        ctl.file$MG_parms[16,5]<-input$k_m_SD            
        ctl.file$MG_parms[16,6]<-prior.type[prior.name==input$k_m_prior]        
        ctl.file$MG_parms[16,7]<-input$k_m_phase         
        
        #CV young
        if(input$CV_lt_m_young_prior=="lognormal"){ctl.file$MG_parms[17,3:4]<-c(input$CV_lt_m_young_mean,log(input$CV_lt_m_young_mean))}     
        else{ctl.file$MG_parms[17,3:4]<-input$CV_lt_m_young_mean}
        ctl.file$MG_parms[17,5]<-input$CV_lt_m_young_SD       
        ctl.file$MG_parms[17,6]<-prior.type[prior.name==input$CV_lt_m_young_prior]       
        ctl.file$MG_parms[17,7]<-input$CV_lt_m_young_phase       
        
        #CV old
        if(input$CV_lt_m_old_prior=="lognormal"){ctl.file$MG_parms[18,3:4]<-c(input$CV_lt_m_old_mean,log(input$CV_lt_m_old_mean))}
        else{ctl.file$MG_parms[18,3:4]<-input$CV_lt_m_old_mean}
        ctl.file$MG_parms[18,5]<-input$CV_lt_m_old_SD       
        ctl.file$MG_parms[18,6]<-prior.type[prior.name==input$CV_lt_m_old_prior]  
        ctl.file$MG_parms[18,7]<-input$CV_lt_m_old_phase 

        #Weight-length
        ctl.file$MG_parms[19,3:4]<-input$WLa_m_est       #coefficient
        ctl.file$MG_parms[20,3:4]<- input$WLb_m_est      #exponent  
      }     

    #S-R
    ctl.file$SR_function<-c(3,2)[c("Beverton-Holt","Ricker")==input$SR_choice_est]
    ctl.file$SR_parms[1,3:4]<-input$lnR0_est  #lnR0
    if(input$h_ss_prior=="lognormal"){ctl.file$SR_parms[2,3:4]<-c(input$h_mean_ss,log(h_mean_ss))}
    else{ctl.file$SR_parms[2,3:4]<-input$h_mean_ss}    
    ctl.file$SR_parms[2,5]<-input$h_SD_ss       
    ctl.file$SR_parms[2,6]<-prior.type[prior.name==input$h_ss_prior]  
    ctl.file$SR_parms[2,7]<-input$h_phase 
    
    }



		#Recruitment estimation		
		ctl.file$do_recdev<-0
		ctl.file$recdev_phase<- -1
		ctl.file$MainRdevYrFirst<-input$styr	#Start year of recruitment estimation
		ctl.file$MainRdevYrLast<-input$endyr		#Last year of recruitment estimation
		ctl.file$last_early_yr_nobias_adj<-input$styr		#End year of early rev devs (no bias)
		ctl.file$first_yr_fullbias_adj<-input$styr			#First year full bias
		ctl.file$last_yr_fullbias_adj<-input$endyr			#Last year full bias
		ctl.file$first_recent_yr_nobias_adj<-input$endyr	#First year recent no bias
		
		if(input$rec_choice)
			{
				ctl.file$SR_parms[3,3:4]<-input$sigmaR 			#sigma R
        if(input$RecDevChoice=="1: Devs sum to zero"){ctl.file$do_recdev<-1}
				if(input$RecDevChoice=="2: Simple deviations"){ctl.file$do_recdev<-2}
				if(input$RecDevChoice=="3: deviation vector"){ctl.file$do_recdev<-3}
				if(input$RecDevChoice=="4: option 3 plus penalties"){ctl.file$do_recdev<-4}
				ctl.file$MainRdevYrFirst<-input$Rdev_startyr	#Start year of recruitment estimation
				ctl.file$MainRdevYrLast<-input$Rdev_endyr		#Last year of recruitment estimation
				ctl.file$recdev_phase<- 1
			if(input$biasC_choice)
				{
					#With bias correction
					ctl.file$recdev_early_start<--1								#Year early rec dev phase starts 
					ctl.file$recdev_early_phase<-3								#Early rec dev phase
					ctl.file$Fcast_recr_phase<-0								#Forecast rec dev phase
					ctl.file$last_early_yr_nobias_adj<-input$NobiasC_early		#End year of early rev devs (no bias)
					ctl.file$first_yr_fullbias_adj<-input$BiasC_startyr			#First year full bias
					ctl.file$last_yr_fullbias_adj<-input$BiasC_endyr			#Last year full bias
					ctl.file$first_recent_yr_nobias_adj<-input$NobiasC_recent	#First year recent no bias
					ctl.file$max_bias_adj<-input$BiasC							#Max bias adjustment				
				}
			}

		#SELECTIVITY
    #Length Selectivity
      if(input$Ct_F_LO_select=="Estimate F" & is.null(rv.Ct$data)){ctl.file$size_selex_types[2]<-3} #Change to recognize discard fishery
		  Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50,","))))
      Sel50_phase<-as.numeric(trimws(unlist(strsplit(input$Sel50_phase,","))))
      Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))
      Selpeak_phase<-as.numeric(trimws(unlist(strsplit(input$Selpeak_phase,","))))
      bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]
      minmaxbin<-c(Selpeak[1]-min(data.file$lbin_vector),max(data.file$lbin_vector)-Selpeak[1])
      sel.inputs.comps<-length(Sel50)-length(Sel50_phase)-length(Selpeak)-length(Selpeak_phase)
      sel.inputs.lts<-c(length(Sel50),length(Sel50_phase),length(Selpeak),length(Selpeak_phase))

    if(input$Sel_choice=="Logistic")
		{
			#Throw warning if not enough selectivity inputs
      if(!all(data.file$Nfleets==sel.inputs.lts))
      {
         sendSweetAlert(
          session = session,
          title = "Selectivity input warning",
          text = "Please check to see if you have provided selectivity inputs (both parameter and phases) for all fleets in the model. This includes fishing fleets and surverys.",
          type = "error")
         remove_modal_spinner()
         stopApp()
      }
      
      #ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      #ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
      ctl.file$size_selex_parms[1,1:2]<-c(Selpeak[1]-minmaxbin[1],Selpeak[1]+minmaxbin[2])
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
      ctl.file$size_selex_parms[2,3:4]<- 15
      ctl.file$size_selex_parms[3,3:4]<- log(-((Sel50[1]-Selpeak[1])^2/log(0.5)))
			ctl.file$size_selex_parms[4,3:4]<- -15
      ctl.file$size_selex_parms[6,3:4]<- 15
      #phases
      ctl.file$size_selex_parms[1,7]<- Selpeak_phase[1]
      ctl.file$size_selex_parms[2,7]<- -1
      ctl.file$size_selex_parms[3,7]<- Sel50_phase[1]
      ctl.file$size_selex_parms[4,7]<- -1
      ctl.file$size_selex_parms[6,7]<- -1
			}
		if(input$Sel_choice=="Dome-shaped")
		{       
      #Throw warning if not enough selectivity inputs
      sel.inputs.comps<-length(Sel50)-length(Sel50_phase)-length(Selpeak)-length(Selpeak_phase)-length(PeakDesc)-length(PeakDesc_phase)-length(LtPeakFinal)-length(LtPeakFinal_phase)-length(FinalSel)-length(FinalSel_phase)
      sel.inputs.lts<-c(length(Sel50),length(Sel50_phase),length(Selpeak),length(Selpeak_phase),length(PeakDesc),length(PeakDesc_phase),length(LtPeakFinal),length(LtPeakFinal_phase),length(FinalSel),length(FinalSel_phase))
      if(!all(data.file$Nfleets==sel.inputs.lts))
      {
         sendSweetAlert(
          session = session,
          title = "Selectivity input warning",
          text = "Please check to see if you have provided selectivity inputs (both parameter and phases) for all fleets in the model. This includes fishing fleets and surverys.",
          type = "error")
          remove_modal_spinner()
          break
      }

      PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))
      PeakDesc_phase<-as.numeric(trimws(unlist(strsplit(input$PeakDesc_phase,","))))
      LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))
      LtPeakFinal_phase<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_phase,","))))
      FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))
      FinalSel_phase<-as.numeric(trimws(unlist(strsplit(input$FinalSel_phase,","))))
      minmaxbin<-c(Selpeak[1]-min(data.file$lbin_vector),max(data.file$lbin_vector)-Selpeak[1])
			
      #ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      #ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
      ctl.file$size_selex_parms[1,1:2]<-c(Selpeak[1]-minmaxbin[1],Selpeak[1]+minmaxbin[2])
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
			ctl.file$size_selex_parms[2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[1]-bin.width)/(PeakDesc[1]-Selpeak[1]-bin.width+0.000000001))
			ctl.file$size_selex_parms[3,3:4]<- log(-((Sel50[1]-Selpeak[1])^2/log(0.5)))
			ctl.file$size_selex_parms[4,3:4]<- log(LtPeakFinal[1])
			ctl.file$size_selex_parms[6,3:4]<- -log((1/(FinalSel[1]+0.000000001)-1))
		  #phases
      ctl.file$size_selex_parms[1,7]<- Selpeak_phase[1]
      ctl.file$size_selex_parms[2,7]<- PeakDesc_phase[1]
      ctl.file$size_selex_parms[3,7]<- Sel50_phase[1]
      ctl.file$size_selex_parms[4,7]<- LtPeakFinal_phase[1]
      ctl.file$size_selex_parms[6,7]<- FinalSel_phase[1]
		}

# if(input$dirichlet)
# {
#   dirichlet.index<-c(unique(data.file$lencomp[,3]),(unique(data.file$agecomp[,3])+3))
#   ctl.file$dirichlet_parms[dirichlet.index,3:4]<-0
#   ctl.file$dirichlet_parms[dirichlet.index,7]<-2
# }
    
    #Add other fleets
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				ctl.file$init_F<-rbind(ctl.file$init_F,ctl.file$init_F[1,])
				ctl.file$size_selex_types<-rbind(ctl.file$size_selex_types,ctl.file$size_selex_types[1,])
				  if(input$Ct_F_LO_select=="Estimate F" & is.null(rv.Ct$data)){ctl.file$size_selex_types[,2]<-3}
        ctl.file$age_selex_types<-rbind(ctl.file$age_selex_types,ctl.file$age_selex_types[1,])
				ctl.file$size_selex_parms<-rbind(ctl.file$size_selex_parms,ctl.file$size_selex_parms[1:6,])
        minmaxbin<-c(Selpeak[i+1]-min(data.file$lbin_vector),max(data.file$lbin_vector)-Selpeak[i+1])

        if(input$Sel_choice=="Logistic")
        {
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+3,7]<- Sel50_phase[i+1]
          #ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
          # ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(Selpeak[i+1]-minmaxbin[1],Selpeak[1]+minmaxbin[2])
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
          ctl.file$size_selex_parms[6*i+1,7]<- Selpeak_phase[i+1]
          ctl.file$size_selex_parms[6*i+2,3:4]<- 15
          ctl.file$size_selex_parms[6*i+2,7]<- -1
          ctl.file$size_selex_parms[6*i+4,3:4]<- -15
          ctl.file$size_selex_parms[6*i+4,7]<- -1
          ctl.file$size_selex_parms[6*i+6,3:4]<- 15
          ctl.file$size_selex_parms[6*i+6,7]<- -1          
        }

        if(input$Sel_choice=="Dome-shaped")
        {
          # ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(Selpeak[i+1]-minmaxbin[1],Selpeak[1]+minmaxbin[2])
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
          ctl.file$size_selex_parms[6*i+1,7]<- Selpeak_phase[i+1]
          ctl.file$size_selex_parms[6*i+2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[i+1]-bin.width)/(PeakDesc[i+1]-Selpeak[i+1]-bin.width+0.000000001))
          ctl.file$size_selex_parms[6*i+2,7]<- PeakDesc_phase[i+1]
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+3,7]<- Sel50_phase[i+1]
          ctl.file$size_selex_parms[6*i+4,3:4]<- log(LtPeakFinal[i+1])
          ctl.file$size_selex_parms[6*i+4,7]<- LtPeakFinal_phase[i+1]
          ctl.file$size_selex_parms[6*i+6,3:4]<- -log((1/(FinalSel[i+1]+0.000000001)-1))
          ctl.file$size_selex_parms[6*i+6,7]<- FinalSel_phase[i+1]          
        }
    
       #Dirichlet data-weighting
#        ctl.file$dirichlet_parms<-rbind(ctl.file$dirichlet_parms,ctl.file$dirichlet_parms[1:2,])
			}


			#Re-label so r4ss can interpret these new entries
			rownames(ctl.file$init_F)<-paste0("InitF_seas_1_flt_",1:data.file$Nfleets,"Fishery",1:data.file$Nfleets)
			rownames(ctl.file$age_selex_types)<-rownames(ctl.file$size_selex_types)<-paste0("Fishery",1:data.file$Nfleets)
			size_selex_parms_rownames<-list()
			for(f_i in 1:data.file$Nfleets)
			{
				size_selex_parms_rownames[[f_i]]<-c(paste0("SizeSel_P_1_Fishery",f_i,"(",f_i,")"),
					paste0("SizeSel_P_2_Fishery",f_i,"(",f_i,")"),
					paste0("SizeSel_P_3_Fishery",f_i,"(",f_i,")"),
					paste0("SizeSel_P_4_Fishery",f_i,"(",f_i,")"),
					paste0("SizeSel_P_5_Fishery",f_i,"(",f_i,")"),
					paste0("SizeSel_P_6_Fishery",f_i,"(",f_i,")"))		
			}
			size_selex_parms_rownames<-unlist(size_selex_parms_rownames)
			rownames(ctl.file$size_selex_parms)<-size_selex_parms_rownames
		}


    #Remove surveys from initial F lines and add q and xtra variance lines
    if(!is.null(rv.Index$data)|data.file$Nfleets>catch.fleets)
      {
        if(data.file$Nfleets>catch.fleets)
          {
            noncatch.fleets<-c((catch.fleets+1):data.file$Nfleets)
            ctl.file$init_F<-ctl.file$init_F[-noncatch.fleets,]
#            ctl.file$init_F<-ctl.file$init_F[-survey.fleets,]
          }
 
        #q set-up
        q.setup.names<-c("fleet","link","link_info","extra_se","biasadj", "float")
        q.setup.lines<-data.frame(t(c(unique(rv.Index$data[,3])[1],1,0,0,0,1)))
        if(input$Indexvar){q.setup.lines<-data.frame(t(c(unique(rv.Index$data[,3])[1],1,0,1,0,1)))}
        qnames<-c("LO","HI","INIT","PRIOR","PR_SD","PR_type","PHASE","env_var&link","dev_link","dev_minyr","dev_maxyr","dev_PH","Block","Block_Fxn")
        q.lines<-data.frame(t(c(-15,15,1,0,1,0,-1,rep(0,7))))
        if(input$Indexvar){q.lines<-data.frame(rbind(c(-15,15,1,0,1,0,-1,rep(0,7)),c(0,5,0,0,99,0,3,0,0,0,0,0,0,0)))}
        if(length(unique(rv.Index$data[,3]))>1)
        {
          for(q in 2:length(unique(rv.Index$data[,3])))
          {
            if(!input$Indexvar)
            {
              q.setup.lines<-rbind(q.setup.lines,c(unique(rv.Index$data[,3])[q],1,0,0,0,1))
              q.lines<-rbind(q.lines,c(-15,15,1,0,1,0,-1,rep(0,7)))                          
            }
      
            if(input$Indexvar)
            {
              q.setup.lines<-rbind(q.setup.lines,c(unique(rv.Index$data[,3])[q],1,0,1,0,1))
              #if(unique(rv.Index$data[,6])[q]!="RSS"){q.setup.lines<-rbind(q.setup.lines,c(unique(rv.Index$data[,3])[q],1,0,1,0,1))}
              #if(unique(rv.Index$data[,6])[q]=="RSS"){q.setup.lines<-rbind(q.setup.lines,c(unique(rv.Index$data[,3])[q],1,0,0,0,1))}
              if(unique(rv.Index$data[,6])[q]!="RSS"){q.lines<-rbind(q.lines,data.frame(rbind(c(-15,15,1,0,1,0,-1,rep(0,7)),c(0,5,0,0,99,0,3,0,0,0,0,0,0,0))))}          
              if(unique(rv.Index$data[,6])[q]=="RSS"){q.lines<-rbind(q.lines,data.frame(rbind(c(-15,15,1,0,1,0,-1,rep(0,7)),c(0,5,0,0,99,0,-3,0,0,0,0,0,0,0))))}          
            }  
          }
        }
        names(q.setup.lines)<-q.setup.names
        rownames(q.setup.lines)<-unique(rv.Index$data[,6])
        ctl.file$Q_options<-q.setup.lines
        names(q.lines)<-qnames
        if(!input$Indexvar){rownames(q.lines)<-paste0("LnQ_base_",unique(rv.Index$data[,6]),"(",unique(rv.Index$data[,3]),")")}
        #rnames.temp<-c(paste0("LnQ_base_",unique(rv.Index$data[,5]),"(",unique(rv.Index$data[,2]),")"),paste0("Q_extraSD_",unique(rv.Index$data[,5]),"(",unique(rv.Index$data[,2]),")"))
        #rnames.temp[1:length(rnames.temp)%%2 != 0]
        if(input$Indexvar)
        {
          qnames.temp1<-paste0("LnQ_base_",unique(rv.Index$data[,6]),"(",unique(rv.Index$data[,3]),")")
          qnames.temp2<-paste0("Q_extraSD_",unique(rv.Index$data[,6]),"(",unique(rv.Index$data[,3]),")")
          qnames.temp<-as.vector(rbind(qnames.temp1,qnames.temp2))
        # if(length(rnames.temp1)>1)
        # {
        #   for(xx in 2:length(rnames.temp1))
        #   {
        #     rnames.temp<-c(rnames.temp1[x],rnames.temp2[x])
        #   }          
        # }
          rownames(q.lines)<-qnames.temp
          
      }
        ctl.file$Q_parms<-q.lines
  if(data.file$Nfleets>catch.fleets)
  {
    if(any(fleet.survey.names=="RSS"))
      {
        RSS.index<-grep("RSS",fleet.survey.names) 
        #ctl.file$Q_parms<-ctl.file$Q_parms
        ctl.file$size_selex_types[RSS.index,1]<-0 #Rename RSS selectivity types
        ctl.file$size_selex_parms<-ctl.file$size_selex_parms[-c((RSS.index*6-5):(RSS.index*6)),] #Remove selectivity related to RSS
      }
    }
  }


    # if(input$Data_wt=="Dirichlet")
    # {
    #     Dirichlet.fleets<-c(unique(data.file$lencomp[,3]),(unique(data.file$agecomp[,3])+data.file$Nfleets))
    #   # if(Dirichlet.fleets>1)
    #   #   {
    #   #     for(i in 1:length(Dirichlet.fleets)){ctl.file$dirichlet_parms<-rbind(ctl.file$dirichlet_parms,ctl.file$dirichlet_parms[1,])}
    #   #   }
    #     ctl.file$dirichlet_parms[Dirichlet.fleets,3:4]<-0.5
    #     ctl.file$dirichlet_parms[Dirichlet.fleets,7]<-2        
    # }

    #Change data weights
    # Lt_dat_wts<-as.numeric(trimws(unlist(strsplit(input$Lt_datawts,","))))
    # ctl.file$Variance_adjustments[1,]<-Lt_dat_wts

		#Change likelihood component weight of catch
		if (is.null(rv.Ct$data))
			{
				lts.lambdas<-ctl.file$lambdas[1,]
        ct.lambdas<-ctl.file$lambdas[2,]
        init.ct.lambdas<-ctl.file$lambdas[3,]
        if(data.file$Nfleets>1)
        {  
          for(i_lam in 2:data.file$Nfleets)
            {
              lts.lambdas_temp<-ctl.file$lambdas[1,]
              ct.lambdas_temp<-ct.lambdas[1,]
              init.ct.lambdas_temp<-init.ct.lambdas[1,]
              lts.lambdas_temp[1,2]<-ct.lambdas_temp[1,2]<-init.ct.lambdas_temp[1,2]<-i_lam
              lts.lambdas<-rbind(lts.lambdas,lts.lambdas_temp)
              ct.lambdas<-rbind(ct.lambdas,ct.lambdas_temp)
              init.ct.lambdas<-rbind(init.ct.lambdas,init.ct.lambdas_temp)
            }
        }
          if(input$Ct_F_LO_select=="Estimate F")
            {
              if(data.file$Nfleets>1)
              {                
                lt.lam.in<-as.numeric(trimws(unlist(strsplit(input$Wt_fleet_Ct,","))))/sum(as.numeric(trimws(unlist(strsplit(input$Wt_fleet_Ct,",")))))
                lt.lam<-lt.lam.in/max(lt.lam.in)
                lts.lambdas[,4]<-lt.lam
              }
              if(data.file$Nfleets==1)
              {                
                lts.lambdas[,4]<-1
              }
            }
          rownames(lts.lambdas)<-paste0("length_Fishery",c(1:data.file$Nfleets),"_sizefreq_method_1_Phz1")
          ct.lambdas[,4]<-0
          rownames(ct.lambdas)<-paste0("catch_Fishery",c(1:data.file$Nfleets),"_Phz1")
          init.ct.lambdas[,4]<-0
          rownames(init.ct.lambdas)<-paste0("init_equ_catch_Fishery",c(1:data.file$Nfleets),"_lambda_for_init_equ_catch_can_only_enable/disable for_all_fleets_Phz1")
          ctl.file$lambdas<-rbind(lts.lambdas,ct.lambdas,init.ct.lambdas)
          ctl.file$N_lambdas<-nrow(ctl.file$lambdas)

#        ctl.file$lambdas[1,4]<-0
			}

		if(!is.null(rv.Ct$data))
			{
  				ct.lambdas<-ctl.file$lambdas[2,]
          init.ct.lambdas<-ctl.file$lambdas[3,]
        
        if(data.file$Nfleets>1)
          {  
            for(i_lam in 2:data.file$Nfleets)
              {
                ct.lambdas_temp<-ct.lambdas[1,]
                init.ct.lambdas_temp<-init.ct.lambdas[1,]
                ct.lambdas_temp[1,2]<-init.ct.lambdas_temp[1,2]<-i_lam
                ct.lambdas<-rbind(ct.lambdas,ct.lambdas_temp)
                init.ct.lambdas<-rbind(init.ct.lambdas,init.ct.lambdas_temp)
              }
           }
        ct.lambdas[,4]<-1
        rownames(ct.lambdas)<-paste0("catch_Fishery",c(1:data.file$Nfleets),"_Phz1")
        init.ct.lambdas[,4]<-0
        ctl.file$lambdas<-rbind(ct.lambdas,init.ct.lambdas)
        rownames(init.ct.lambdas)<-paste0("init_equ_catch_Fishery",c(1:data.file$Nfleets),"_lambda_for_init_equ_catch_can_only_enable/disable for_all_fleets_Phz1")
        ctl.file$N_lambdas<-data.file$Nfleets*2
        #ctl.file$lambdas[1,4]<-1
				# ctl.file$lambdas[2,4]<-0
				# browser()
        ctl.file$init_F$INIT<-0.00000000000000000001
				ctl.file$init_F$PHASE<--1
        #Turn on if equilibrium catch > 0
        ctl.file$init_F$INIT[as.numeric(trimws(unlist(strsplit(input$Eq_Ct_fleet,","))))>0.0000000000001]<-0.01
        ctl.file$init_F$PHASE[as.numeric(trimws(unlist(strsplit(input$Eq_Ct_fleet,","))))>0.0000000000001]<-1

			}
    ctl.file$Comments <- c(ctl.file$Comments, input$scenario_description_input)
		SS_writectl(ctl.file,paste0("Scenarios/",input$Scenario_name,"/controlfile.ctl"),overwrite=TRUE)
  #}
#}
}
}
		####################### END CTL FILE ####################################
if(input$user_model)
{
  starter.file<-SS_readstarter(paste0("Scenarios/",input$Scenario_name,"/starter.ss"))
  #Use par file
    if(input$use_par)
    {
      starter.file$init_values_src<-1
    }
    if(!input$use_par|is.null(input$use_par))
    {
      starter.file$init_values_src<-0
    }


#Use datanew file
    if(input$use_datanew)
    {
      starter.file$datfile<-"data_echo.ss_new"
    }

    if(!input$use_datanew|is.null(input$use_datanew))
    {
      if(!input$user_model|is.null(input$use_datanew)){starter.file$datfile<-"datafile.dat"}
    }

#Use controlnew file
    if(input$use_controlnew)
    {
      starter.file$ctlfile<-"control.ss_new"
    }

if(input$use_forecastnew)
  {
    forecast.file<-SS_readforecast(paste0("Scenarios/",input$Scenario_name,"/forecast.ss_new"))
    SS_writeforecast(forecast.file,paste0("Scenarios/",input$Scenario_name),overwrite=TRUE)  
  }

#    if(!input$use_controlnew|is.null(input$use_controlnew))
 #   {
 #     if(!input$user_model|is.null(input$use_controlnew)){starter.file$ctlfile<-"controlfile.ctl"}
 #   }
SS_writestarter(starter.file,paste0("Scenarios/",input$Scenario_name),overwrite=TRUE)

}

if(exists("checkmod")|input$user_model)
  {
      starter.file<-SS_readstarter(paste0("Scenarios/",input$Scenario_name,"/starter.ss"))

#Phase 0
    if(input$use_phase0)
    {
      starter.file$last_estimation_phase<-0
    }
    #if(!input$use_par|is.null(input$use_par))
    else{starter.file$last_estimation_phase<-10}

	#Jitter selection 
				starter.file$jitter_fraction<-0
		
   #  if(input$jitter_choice)
			# {
			# 	starter.file$jitter_fraction<-input$jitter_fraction
   #      starter.file$init_values_src<-0
			# }
 				SS_writestarter(starter.file,paste0("Scenarios/",input$Scenario_name),overwrite=TRUE)


#Forecast file modfications
#Reference points
#if(is.null(input$use_forecastnew)|!input$use_forecastnew)
#{
forecast.file<-SS_readforecast(paste0("Scenarios/",input$Scenario_name,"/forecast.ss"))

if(input$RP_choices){
    forecast.file$SPRtarget<-input$SPR_target
    forecast.file$Btarget<-input$B_target

    CR_choices<-c("1: Catch fxn of SSB, buffer on F",
              "2: F fxn of SSB, buffer on F",
              "3: Catch fxn of SSB, buffer on catch",
              "4: F fxn of SSB, buffer on catch")
    CR_choices_num.vec<-c(1:4)
    forecast.file$ControlRuleMethod<-CR_choices_num.vec[CR_choices==input$CR_Ct_F]
    forecast.file$SBforconstantF<-input$slope_hi
    forecast.file$BfornoF<-input$slope_low  
  }

if(input$Forecast_choice)
  {
    forecast.file$Nforecastyrs<-input$forecast_num
    buffer.in<-as.numeric(trimws(unlist(strsplit(input$forecast_buffer,","))))
    if(length(buffer.in)==1){forecast.file$Flimitfraction<-buffer.in}    
    if(length(buffer.in)>1)
      {
        forecast.file$Flimitfraction<--1
        buffer.datafr<-data.frame(Year=c((data.file$endyr+1):(data.file$endyr+input$forecast_num)),Fraction=buffer.in)
        #rownames(buffer.datafr)<-paste0("#_Flimitfraction_m",1:input$forecast_num)
        forecast.file$Flimitfraction_m<-buffer.datafr      
      }
  }

SS_writeforecast(forecast.file,paste0("Scenarios/",input$Scenario_name),overwrite=TRUE)  
#}


########
	#Run Stock Synthesis and plot output
    show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[2],text="Model run in progress")
		if(input$Data_wt=="None"){DataWT_opt<-"none"}
    if(input$Data_wt=="Dirichlet-multinomial"){DataWT_opt<-"DM"}
    if(input$Data_wt=="Francis"){DataWT_opt<-"Francis"}
    if(input$Data_wt=="McAllister-Ianelli"){DataWT_opt<-"MI"}
 				    
#RUN SS3 MODELS
    if(is.null(input$user_model))
    {
    if(is.null(input$no_hess)){
      cmd.in<-""
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)

      if(!file.exists(paste0("Scenarios/",input$Scenario_name,"data_echo.ss_new")))
        {
      cmd.in<-" -nohess"
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
        }
    }

    if(!is.null(input$no_hess))
    {
      if(input$no_hess)
      {
      cmd.in<-" -nohess"
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
      }
      if(!input$no_hess)
      {
      cmd.in<-""
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
      }
    }
    }


###
    if(!is.null(input$user_model))
    {
    if(input$user_model==FALSE)
    {
    if(is.null(input$no_hess)){
      cmd.in<-""
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)

      if(!file.exists(paste0("Scenarios/",input$Scenario_name,"data_echo.ss_new")))
        {
      cmd.in<-" -nohess"
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
        }
    }

    if(!is.null(input$no_hess))
    {
      if(input$no_hess)
      {
      cmd.in<-" -nohess"
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
      }
      if(!input$no_hess)
      {
      cmd.in<-""
      if(!is.null(input$add_comms)){if(input$add_comms==TRUE){cmd.in=paste0(" -nohess ",input$add_comms_in)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
      }
    }
  }

    if(input$user_model==TRUE)
    {
    if(is.null(input$no_hess_user)){
      cmd.in<-""
      if(!is.null(input$add_comms_user)){if(input$add_comms_user==TRUE){cmd.in=paste0(" ",input$add_comms_in_user)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)

      if(!file.exists(paste0("Scenarios/",input$Scenario_name,"data_echo.ss_new")))
        {
      cmd.in<-" -nohess"
      if(!is.null(input$add_comms_user)){if(input$add_comms_user==TRUE){cmd.in=paste0(" ",input$add_comms_in_user)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
        }
    }

    if(!is.null(input$no_hess_user))
    {
      if(input$no_hess_user)
      {
      cmd.in<-" -nohess"
      if(!is.null(input$add_comms_user)){if(input$add_comms_user==TRUE){cmd.in=paste0(" ",input$add_comms_in_user)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
      }
      if(!input$no_hess_user)
      {
      cmd.in<-""
      if(!is.null(input$add_comms_user)){if(input$add_comms_user==TRUE){cmd.in=paste0(" ",input$add_comms_in_user)}}
      RUN.SS(paste0("Scenarios/",input$Scenario_name),ss.cmd=cmd.in)
      }
    }
  }
}

###

#  observeEvent(input$add_comms, {
#     updatePrettyCheckbox(
#       session = session,
#       inputId = "add_comms",
#       value = FALSE
#     )
#   })

# observeEvent(input$add_comms_user, {
#     updatePrettyCheckbox(
#       session = session,
#       inputId = "add_comms_user",
#       value = FALSE
#     )
#   })
 	 			
    if(file.exists(paste0("Scenarios/",input$Scenario_name,"/data_echo.ss_new")))
      {
      Model.output<-try(SS_output(paste0("Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE))

      if(class(Model.output)=="try-error")
        {
          Model.output<-SS_output(paste0("Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
        }

      if(input$Data_wt!="None")
         {
           if(Model.output$inputs$covar==TRUE)
             {
               tune_comps(Model.output,dir=paste0("Scenarios/",input$Scenario_name),niters_tuning=3,option=DataWT_opt,show_in_console = TRUE,verbose=FALSE)
               Model.output<-try(SS_output(paste0("Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE))
             }
           if(Model.output$inputs$covar==FALSE)
              {
               tune_comps(Model.output,dir=paste0("Scenarios/",input$Scenario_name),option=DataWT_opt,niters_tuning=3,extras = " -nohess",show_in_console = TRUE,verbose=FALSE)
               Model.output<-SS_output(paste0("Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
             }
         }
  
      data.file<-SS_readdat(paste0("Scenarios/",input$Scenario_name,"/data_echo.ss_new"))    
      #No plots or figures
      if(is.null(input$no_plots_tables))
        {      
          show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[4],text="Making plots")
          RPs_4_plots<-as.numeric(trimws(unlist(strsplit(input$plot_RPs_inputs,","))))
          SS_plots(Model.output,maxyr=data.file$endyr+1,verbose=FALSE,btarg=RPs_4_plots[1],minbthresh=RPs_4_plots[2])
        }

      if(is.null(input$no_tables))
        {      
          show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[5],text="Making tables")
          try(SSexecutivesummary(Model.output))   
        }

   if(!is.null(input$no_plots_tables)){      
      if(input$no_plots_tables==FALSE)
      {      
        #Make SS plots  
        show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[4],text="Making plots")
         RPs_4_plots<-as.numeric(trimws(unlist(strsplit(input$plot_RPs_inputs,","))))
         SS_plots(Model.output,maxyr=data.file$endyr+1,verbose=FALSE,btarg=RPs_4_plots[1],minbthresh=RPs_4_plots[2])
      }
    }

   if(!is.null(input$no_tables)){      
      if(input$no_tables==FALSE)
      {      
        #Make SS3 tables
        show_modal_spinner(spin="flower",color=wes_palettes$Zissou1[5],text="Making tables")
        try(SSexecutivesummary(Model.output))   
      }
    }
    
  #Run multiple jitters
    if(.Platform[["OS.type"]] == "windows"){os_exe <- "ss3"} 
    if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){os_exe <- "ss3_osx"} 
    if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){os_exe <- "ss3_osx_arm64"} 
    if(R.version[["os"]] == "linux-gnu"){os_exe <- "ss3_linux"}
      
    if(input$jitter_choice)
    {
      if(input$Njitter>0)
      {
         show_modal_spinner(spin="flower",color=wes_palettes$Moonrise1[1],text="Run jitters")
         #file.copy(paste0("Scenarios/",input$Scenario_name,"/ss.exe"),paste0("Scenarios/",input$Scenario_name,"/ss_copy.exe"),overwrite = FALSE)
         if(input$jitter_parallel)
         {
          ncores <- parallelly::availableCores(omit = 1)
          future::plan(future::multisession, workers = ncores)
         }
         jits<-r4ss::jitter(
                      dir=paste0(getwd(),"/Scenarios/",input$Scenario_name),
                      Njitter=input$Njitter,
                      printlikes = TRUE,
                      jitter_fraction=input$jitter_fraction,
                      init_values_src=0,
                      verbose=TRUE,
                      exe = os_exe,
                      extras = "-nohess"
                      )
         
         profilemodels <- r4ss::SSgetoutput(dirvec=paste0(getwd(),"/Scenarios/",input$Scenario_name), keyvec=0:input$Njitter, getcovar=FALSE)
         profilesummary <- r4ss::SSsummarize(profilemodels)
         minlikes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]==min(profilesummary$likelihoods[1,-length(profilesummary$likelihoods)],na.rm=TRUE)
         #Find best fit model
         index.minlikes<-c(1:length(minlikes))[minlikes]
         jitter.likes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]
         ref.like<-min(jitter.likes,na.rm = TRUE)
         
         #Make plot and save to folder
           main.dir<-getwd()
           if(!file.exists(paste0(main.dir,"/Scenarios/",input$Scenario_name,"/Jitter Results")))
          {
              dir.create(paste0(main.dir,"/Scenarios/",input$Scenario_name,"/Jitter Results"))
          }
           setwd(paste0(main.dir,"/Scenarios/",input$Scenario_name,"/Jitter Results"))
           png("jitterplot.png")
         jitterplot<-plot(c(1:length(jitter.likes)),jitter.likes,type="p",col="black",bg="blue",pch=21,xlab="Jitter run",ylab="-log likelihood value",cex=1.25)
         points(c(1:length(jitter.likes))[jitter.likes>ref.like],jitter.likes[jitter.likes>ref.like],type="p",col="black",bg="red",pch=21,cex=1.25)
         abline(h=ref.like)
         # likebc<-round((length(jitter.likes[ref.like==jitter.likes])/(input$Njitter+1))*100,0)
         # likelessbc<-round((length(jitter.likes[ref.like>jitter.likes])/(input$Njitter+1))*100,0)
         # like10<-round((length(jitter.likes[(ref.like+10)<jitter.likes])/(input$Njitter+1))*100,0)
         # like2<-round(((length(jitter.likes[(ref.like+2)>jitter.likes])-(length(jitter.likes[ref.like==jitter.likes])))/(input$Njitter+1))*100,0)
         # like_2_10<-round(100-(likebc+like10+like2),0)
         # legend("topright",c(paste("  ",likelessbc,"% < BC",sep=""),paste(likebc,"% = BC",sep=""),paste(like2,"% < BC+2",sep=""),paste(like_2_10,"% > BC+2 & < BC+10",sep=""),paste(like10,"% > BC+10",sep="")),bty="n")
         dev.off()
          save(profilesummary,file=paste0("jitter_summary.DMP"))
          SSplotComparisons(profilesummary, legendlabels = c(0:input$Njitter), ylimAdj = 1.30, subplot = c(1), new = FALSE,print=TRUE,plotdir=getwd())
          SSplotComparisons(profilesummary, legendlabels = c(0:input$Njitter), ylimAdj = 1.30, subplot = c(3), new = FALSE,print=TRUE,plotdir=getwd())
         
         output$Jitterplot<-renderPlot({
        # if(input$Njitter==1){return(NULL)}
        # if(input$Njitter>1)
        # {
         #jitter.likes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]
         #ref.like<-min(jitter.likes)
         jitterplot<-plot(c(1:length(jitter.likes)),jitter.likes,type="p",col="black",bg="blue",pch=21,xlab="Jitter run",ylab="-log likelihood value",cex=1.25)
         points(c(1:length(jitter.likes))[jitter.likes>ref.like],jitter.likes[jitter.likes>ref.like],type="p",col="black",bg="red",pch=21,cex=1.25)
         abline(h=ref.like)
         # likebc<-round((length(jitter.likes[ref.like==jitter.likes])/(input$Njitter+1))*100,0)
         # likelessbc<-round((length(jitter.likes[ref.like>jitter.likes])/(input$Njitter+1))*100,0)
         # like10<-round((length(jitter.likes[(ref.like+10)<jitter.likes])/(input$Njitter+1))*100,0)
         # like2<-round(((length(jitter.likes[(ref.like+2)>jitter.likes])-(length(jitter.likes[ref.like==jitter.likes])))/(input$Njitter+1))*100,0)
         # like_2_10<-round(100-(likebc+like10+like2),0)
         # legend("topright",c(paste("  ",likelessbc,"% < BC",sep=""),paste(likebc,"% = BC",sep=""),paste(like2,"% < BC+2",sep=""),paste(like_2_10,"% > BC+2 & < BC+10",sep=""),paste(like10,"% > BC+10",sep="")),bty="n")  
        # }
        })
      #Spawning output comp
      output$Jittercompplot1<-renderPlot({
        SSplotComparisons(profilesummary, legendlabels = c(0:input$Njitter), ylimAdj = 1.30, subplot = c(1), new = FALSE)
        })
      #Relative stock status comp
      output$Jittercompplot2<-renderPlot({
        SSplotComparisons(profilesummary, legendlabels = c(0:input$Njitter), ylimAdj = 1.30, subplot = c(3), new = FALSE)
        })

         #R-run to get new best fit model
         show_modal_spinner(spin="flower",color=wes_palettes$Moonrise1[2],text="Re-run best model post-jitters")
         file.copy(paste0(main.dir,"/Scenarios/",input$Scenario_name,"/ss3.par_",(index.minlikes[1]-1),".sso"),paste0(main.dir,"/Scenarios/",input$Scenario_name,"/ss3.par"),overwrite = TRUE)
         #file.rename(paste0("Scenarios/",input$Scenario_name,"/ss_copy.exe"),paste0("Scenarios/",input$Scenario_name,"/ss.exe"),overwrite = FALSE)
             starter.file$init_values_src<-1
             starter.file$jitter_fraction<-0
         SS_writestarter(starter.file,paste0(main.dir,"/Scenarios/",input$Scenario_name),overwrite=TRUE)
         RUN.SS(paste0(main.dir,"/Scenarios/",input$Scenario_name),ss.cmd="")
         Model.output<-try(SS_output(paste0(main.dir,"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE))
          if(class(Model.output)=="try-error")
          {
            Model.output<-SS_output(paste0(main.dir,"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
          }
         show_modal_spinner(spin="flower",color=wes_palettes$Moonrise1[3],text="Making plots")
         RPs_4_plots<-as.numeric(trimws(unlist(strsplit(input$plot_RPs_inputs,","))))
         SS_plots(Model.output,maxyr=data.file$endyr+1,verbose=FALSE,btarg=RPs_4_plots[1],minbthresh=RPs_4_plots[2])
         show_modal_spinner(spin="flower",color=wes_palettes$Moonrise1[4],text="Making tables")
         try(SSexecutivesummary(Model.output))                 
    }   
    setwd(main.dir)
  }
 
     #Add retro runs   
  #    if(input$Retro_choice){           
  #    mydir<-paste0(getwd(),"/Scenarios/")
  #    model_settings = get_settings(settings = list(base_name = input$Scenario_name,
  #                       run = "retro",
  #                       retro_yrs = input$first_retro_year:input$final_retro_year))

  #   # tryCatch({
  #       run_diagnostics(mydir = mydir, model_settings = model_settings)
  #   # },
  #   # warning = function(warn){
  #   #     showNotification(paste0(warn), type = 'warning')
  #   # },
  #   # error = function(err){
  #   #     showNotification(paste0(err), type = 'err')
  #   # })
  # } 
	 
	 
  	#Convergence diagnostics
		output$converge.grad <- renderText({
 				max.grad<-paste0("Maximum gradient: ",Model.output$maximum_gradient_component)
 					
			})

		output$converge.covar <- renderText({
 				covar<-paste0("Was covariance file created? ",Model.output$inputs$covar)
 					
			})
 		
 		output$converge.dec <- renderText({
 				if(Model.output$maximum_gradient_component<0.1 & Model.output$inputs$covar==TRUE)
 					{converge.dec<-"Model appears converged. Please check outputs for nonsense."}
 				else{converge.dec<-"Model may not have converged or inputs are missing. Please use the Jitter option or check/change starting values before re-running model."}
			})
 		
 		#Relative biomass
		output$SSout_relSB_table <- render_gt({
				SB_indices<-c(which(rownames(Model.output$derived_quants)==paste0("Bratio_",input$endyr)),
					which(rownames(Model.output$derived_quants)=="B_MSY/SSB_unfished"),
					which(rownames(Model.output$derived_quants)==paste0("SPRratio_",input$endyr)),
					which(rownames(Model.output$derived_quants)==paste0("OFLCatch_",(input$endyr+1))),
					which(rownames(Model.output$derived_quants)==paste0("ForeCatch_",(input$endyr+1)))
					)
				Output_relSB_table<-data.frame(Model.output$derived_quants[SB_indices,1:3])
					# Label=c(paste0("SO",input$endyr+1,"/SO_0"),
					# 					  "SO_MSY/SO_0",
					# 					  paste0("SPR",input$endyr+1),
					# 					  paste0("OFL",(input$endyr+1)),
					# 					  paste0("ABC",(input$endyr+1))
					# 					  ))
				Output_relSB_table[,1]<-c(paste0("SO",input$endyr,"/SO_0"),
										  "SO_MSY/SO_0",
										  paste0("1-SPR",input$endyr),
										  paste0("OFL",(input$endyr+1)),
										  paste0("ABC",(input$endyr+1))
										  )
				Output_relSB_table<-mutate_if(Output_relSB_table,is.numeric, round, 2)  
        relSB_tab<-gt(Output_relSB_table) %>%
        tab_header(
        title = "Key Derived Outputs",
        subtitle = ""
        ) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Value)) %>%
        opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
        
					# rownames=c(expression(SO[input$endyr]/SO[0]),
					# 					  expression(SO[MSY]/SO[0]),
					# 					  expression(SPR[input$endyr]),
					# 					  expression(OFL[input$endyr]),
					# 					  expression(ABC[input$endyr])
					# 					  ))
				# Output_relSB_table[,1]<-c(expression('B',[input$endyr],'/B',[0]),
				# 						  expression('B'[MSY]/'B'[0]),
				# 						  expression('SPR'[input$endyr]),
				# 						  expression('OFL'[input$endyr]),
				# 						  expression('ABC'[input$endyr])
				# 						  )	
				})

		#F estimate and relative to FMSY and proxies		
		output$SSout_F_table <- render_gt({
				F_indices<-c(which(rownames(Model.output$derived_quants)==paste0("F_",input$endyr)),
							which(rownames(Model.output$derived_quants)=="annF_Btgt"),
							which(rownames(Model.output$derived_quants)=="annF_SPR"),
							which(rownames(Model.output$derived_quants)=="annF_MSY")
							)
				F_values<-Model.output$derived_quants[F_indices,1:3]
        F_values<-mutate_if(F_values,is.numeric, round, 2)  
        F_values_tab<-gt(F_values) %>%
        tab_header(
        title = "Fishing Intensity",
        subtitle = ""
        ) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Value))) %>%
        opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
 			})
		#Time series output
 		output$SSout_table <- render_gt({
        Output_table<-Model.output$sprseries[,c(1,3,4,23,24,7,8,9,10,12,13,25,53,54)]
        Output_table<-mutate_if(Output_table,is.numeric, round, 2)  
       gt(Output_table)%>%
        tab_header(
        title = "Time Series of Derived Model Outputs",
        subtitle = ""
        ) %>%
        data_color(columns = c(4,7,10,12), method = "auto", palette = "viridis",reverse=TRUE) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Deplete))) %>%
        opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
      })
 		
 		#Parameters
 		output$Parameters_table <- render_gt({
 				parm_tab<-cbind(rownames(Model.output$estimated_non_dev_parameters),Model.output$estimated_non_dev_parameters)
			  colnames(parm_tab)[1]<-"Parameter"
        parm_tab<-mutate_if(parm_tab,is.numeric, round, 2)  
        gt(parm_tab)%>%
        tab_header(
        title = "Estimated Parameters",
        subtitle = ""
        ) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Value))) %>%
        opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
      })

output$Sel_transform_table <- render_gt({

Est.sel.tranformed<-Model.output$parameters[grep("Size_",Model.output$parameters$Label),c(2,3,8)]
if(length(grep("peak_",Est.sel.tranformed$Label))>0)
  {
    Est.sel.peak<-Est.sel.tranformed$Value[grep("peak_",Est.sel.tranformed$Label)]
    Est.sel.tranformed$Value[grep("peak_",Est.sel.tranformed$Label)]<-Est.sel.peak
    Est.sel.tranformed$Label[grep("peak_",Est.sel.tranformed$Label)]<-"Length at Peak Selectivity"
  }

if(length(grep("ascend_",rownames(Model.output$parameters)))>0)
  {
    Est.sel.Sel50<-Est.sel.tranformed$Value[grep("ascend_",Est.sel.tranformed$Label)]
    Est.sel.tranformed$Value[grep("ascend_",Est.sel.tranformed$Label)]<-Est.sel.peak-sqrt(-exp(Est.sel.Sel50)*log(0.5))
    Est.sel.tranformed$Label[grep("ascend_",Est.sel.tranformed$Label)]<-"Length at 50% Selectivity"
  }

if(length(grep("top_logit",rownames(Model.output$parameters)))>0)
  {
    Est.sel.Decline1<-Est.sel.tranformed$Value[grep("top_logit",Est.sel.tranformed$Label)]
    Est.sel.tranformed$Value[grep("top_logit",Est.sel.tranformed$Label)]<-((max(data.file$lbin_vector)-Est.sel.peak-bin.width)/-exp(Est.sel.Decline1))+(Est.sel.peak+bin.width)
    Est.sel.tranformed$Label[grep("top_logit",Est.sel.tranformed$Label)]<-"Length at 1st declining selectivity"
  }

if(length(grep("descend",rownames(Model.output$parameters)))>0)
  {
    Est.sel.Width<-Est.sel.tranformed$Value[grep("descend",Est.sel.tranformed$Label)]
    Est.sel.tranformed$Value[grep("descend",Est.sel.tranformed$Label)]<-exp(Est.sel.Width)
    Est.sel.tranformed$Label[grep("descend",Est.sel.tranformed$Label)]<-"Width of declining selectivity"
  }


if(length(grep("end_logit",rownames(Model.output$parameters)))>0)
  {
    Est.sel.Final<-Est.sel.tranformed$Value[grep("end_logit",Est.sel.tranformed$Label)]
    Est.sel.tranformed$Value[grep("end_logit",Est.sel.tranformed$Label)]<-(1/(1-exp(-Est.sel.Final)))-0.000001
    Est.sel.tranformed$Label[grep("end_logit",Est.sel.tranformed$Label)]<-"Selectivity at max bin size"
  }
  
  Est.sel.tranformed<-mutate_if(Est.sel.tranformed,is.numeric, round, 3) 
  gt(Est.sel.tranformed)%>%
    tab_header(
    title = "Transformed Selectivity Parameters",
        subtitle = ""
        ) %>%
        tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Value))) %>%
        opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)

})
} 	

      if(!file.exists(paste0("Scenarios/",input$Scenario_name,"/data_echo.ss_new")))
      {
       sendSweetAlert(
        session = session,
        title = "Model Warning",
        text = "Model did not run or Hessian did not invert. Double check data files for errors and each input for missing values (or for 0 SD for lognormal priors) and/or re-run model using a different model specification (e.g., starting values).",
        type = "warning")
     }
    remove_modal_spinner()
    
    observeEvent(exists("Model.output"), {
    updateTabsetPanel(session, "tabs",
      selected = '2')
    })

    updateCheckboxInput(inputId=input$user_model,value=FALSE)
  }  
 })


###############################################################
### Likelihood profiles, Sensitivities, and Ensemble models ###
###############################################################

  roots <- getVolumes()()  


  #CODE TO ALLOW USERS TO SAVE OUTPUT SOMEWHERE OTHER THAN SCENARIOS FOLDER 
    # pathModelout <- reactive({
    #     shinyDirChoose(input, "Modelout_dir", roots= roots,session=session, filetypes=c('', 'txt'))
    #     return(parseDirPath(roots, input$Modelout_dir))
    #   })

    # observeEvent(as.numeric(input$tabs)==2,{      
    # #observeEvent(exists("Model.output"),{      
    #   pathModelout.dir <-pathModelout()
    # if(!identical(pathModelout.dir, character(0)))
    # {
    #   #dir.create(paste0(pathModelout.dir,"/Scenarios"))
    #   file.copy(paste0("Scenarios/",input$Scenario_name), pathModelout.dir,recursive=TRUE,overwrite=TRUE)
    #   if(input$Retro_choice){file.copy(paste0("Scenarios/",input$Scenario_name,"_retro"), pathModelout.dir,recursive=TRUE,overwrite=TRUE)}
    # }
    # })


########################
### Model efficiency ###
########################

      shinyDirChoose(input,"ModEff_dir", roots=roots,session=session, filetypes=c('', 'txt'))
  pathModeff <- reactive({
        return(parseDirPath(roots, input$ModEff_dir))
      })

  

# if(exists("ModEff_dir")){print(ModEff_dir)}
#  observeEvent(as.numeric(input$tabs)==12,{      
#   output$ModEff_model_pick<-renderUI({
#       pickerInput(
#       inputId = "myModEff",
#       label = "Choose model to evaluate",
#       choices = list.files(pathModEff()),
#       options = list(
#         `actions-box` = TRUE,
#         size = 12,
#         `selected-text-format` = "count > 3"
#         ),
#       multiple = TRUE
#       )
#     })
#   })

 observeEvent(req(input$run_adnuts),{
 #browser()
 #output$RetroPath <- renderText({paste0("Selected model folder:\n", pathRetro())})
 modeff.mod.dir<-pathModeff() #pathModEff()
 #modeff.mod.dir<-parseDirPath(roots, input$ModEff_dir) #pathModEff()
 modeff.dir<-dirname(modeff.mod.dir)
 modeff.name<-paste0(basename(modeff.mod.dir),"_",input$ModEff_choice)
#if(dir.exists(file.path(modeff.dir,modeff.name))==FALSE)
#{
  dir.create(file.path(modeff.dir,modeff.name))
  file.copy(list.files(modeff.mod.dir,full.names=TRUE),to=file.path(modeff.dir,modeff.name),recursive=TRUE,overwrite=TRUE)
#}

#optimize model
if(input$Opt_mod==TRUE)
{
  show_modal_spinner(spin="flower",color=wes_palettes$Rushmore[1],text=paste0("Run initial optimization?"))
  RUN.SS(file.path(modeff.dir,modeff.name),ss.cmd="-nox -mcmc 100 -hbf")

  remove_modal_spinner()
}

#Set mcmc model
show_modal_spinner(spin="flower",color=wes_palettes$Rushmore[1],text=paste0("Run ",input$ModEff_choice," model"))
chains <- parallelly::availableCores(omit = 1)
    if(.Platform[["OS.type"]] == "windows"){os_exe <- "ss3"} 
    if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){os_exe <- "ss3_osx"} 
    if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){os_exe <- "ss3_osx_arm64"} 
    if(R.version[["os"]] == "linux-gnu"){os_exe <- "ss3_linux"}
m<-os_exe
p<-file.path(modeff.dir,modeff.name)
#Run MCMC model with either rwm or nuts
 if(input$ModEff_choice=="RWM")
   {
     fit_model<- adnuts::sample_rwm(model=m, path=p, iter=input$iter, warmup=0.25*input$iter,
                       chains=chains, thin=input$thin, duration=NULL)
   }

  if (input$ModEff_choice=="Nuts") 
  {
    fit_model <- adnuts::sample_nuts(model=m, path=p,  iter=input$iter, warmup=0.25*input$iter, 
          chains=chains, cores=4,control=list(metric='mle', max_treedepth=5),mceval=TRUE)
  }

fit.mod.summary<-utils::capture.output(summary(fit_model), file=NULL)


output$fit.model.summary <- renderText({
 				#paste0(fit.mod.summary[1],fit.mod.summary[2],fit.mod.summary[3])
 				fit.mod.summary
			})

parmax<-10
if(length(fit_model$par_names)<10){parmax<-length(fit_model$par_names)}

png(paste0(p,"/pairs_plot_slow.png"),width=600, height=350)
 pairs_admb(fit_model, pars=1:parmax, order='slow')
 dev.off()

png(paste0(p,"/pairs_plot_fast.png"),width=600, height=350)
 pairs_admb(fit_model, pars=1:parmax, order='fast')
 dev.off()


output$pairs_slow <- renderImage({
       #region image.path1<-normalizePath(paste0(p,"/pairs_plot_fast.png"),mustWork=FALSE)
       return(list(
        src = paste0(p,"/pairs_plot_slow.png"),
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

output$pairs_fast <- renderImage({
       #region image.path1<-normalizePath(paste0(p,"/pairs_plot_fast.png"),mustWork=FALSE)
       return(list(
        src = paste0(p,"/pairs_plot_fast.png"),
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

save(fit_model,file=paste0(p,"/fit_model.RData"))

    remove_modal_spinner()

#if(input$run_stanout==TRUE){launch_shinyadmb(fit_model)}

      })


  ###########################  
  ### Likelihood profiles ###
  ###########################

  pathLP <- reactive({
      shinyDirChoose(input, "LP_dir", roots=roots,session=session, filetypes=c('', 'txt'))
        return(parseDirPath(roots, input$LP_dir))
      })
  
  observeEvent(input$LP_dir,{
  output$LikeProfPath <- renderText({paste0("Selected scenario folder:\n", pathLP())})
  })

  observeEvent(as.numeric(input$tabs)==4,{      
  pathLP.dir <-pathLP()
  output$LikeProf_model_picks<-renderUI({
      pickerInput(
      inputId = "myPicker_LP",
      label = "Choose parameters to profile over",
      choices = c("Steepness","lnR0","Natural mortality female","Linf female","k female", "CV@Lt young female","CV@Lt old female","Natural mortality male","Linf male","k male", "CV@Lt young male", "CV@Lt old male"),
      options = list(
        `actions-box` = TRUE,
        size = 12,
        `selected-text-format` = "count > 3"
        ),
      multiple = TRUE
    )
 })
})

observeEvent(input$run_Profiles,{
       show_modal_spinner(spin="flower",color=wes_palettes$Darjeeling1[1],text="Profiles running")
       starter.file<-SS_readstarter(paste0(pathLP(),"/starter.ss"))
       #data.file<-SS_readdat(paste0(pathLP(),"/data_echo.ss_new"))
       #ctl.file<-SS_readctl(paste0(pathLP(),"/control.ss_new"),use_datlist = TRUE, datlist=data.file)
       rep.parms<-SS_output(pathLP(),covar=FALSE,verbose=FALSE)
       rep.parms.names<-rownames(rep.parms$parameters)
       #SS_parm_names<-c("SR_BH_steep", "SR_LN(R0)","NatM_p_1_Fem_GP_1","L_at_Amax_Fem_GP_1","VonBert_K_Fem_GP_1","CV_young_Fem_GP_1","CV_old_Fem_GP_1","NatM_p_1_Mal_GP_1","L_at_Amax_Mal_GP_1","VonBert_K_Mal_GP_1","CV_young_Mal_GP_1","CV_old_Mal_GP_1")
       #SS_parm_names<-c(rownames(ctl.file$SR_parms)[2], rownames(ctl.file$SR_parms)[1],rownames(ctl.file$MG_parms)[1],rownames(ctl.file$MG_parms)[3],rownames(ctl.file$MG_parms)[4],rownames(ctl.file$MG_parms)[5],rownames(ctl.file$MG_parms)[6],rownames(ctl.file$MG_parms)[13],rownames(ctl.file$MG_parms)[15],rownames(ctl.file$MG_parms)[16],rownames(ctl.file$MG_parms)[17],rownames(ctl.file$MG_parms)[18])
       #SS_parm_names<-c(rep.parms.names[24], rep.parms.names[23],rep.parms.names[1],rep.parms.names[3],rep.parms.names[4],rep.parms.names[5],rep.parms.names[6],rep.parms.names[13],rep.parms.names[15],rep.parms.names[16],rep.parms.names[17],rep.parms.names[18],"LnQ_base_Acoustic_Visual(6)")
       SS_parm_names<-c(rep.parms.names[grep("steep",rep.parms.names)], rep.parms.names[grep("R0",rep.parms.names)],rep.parms.names[grep("NatM",rep.parms.names)][1],rep.parms.names[grep("L_at_Amax_Fem",rep.parms.names)],rep.parms.names[grep("K_Fem",rep.parms.names)],rep.parms.names[grep("CV_young_Fem",rep.parms.names)],rep.parms.names[grep("CV_old_Fem",rep.parms.names)],rep.parms.names[grep("NatM",rep.parms.names)][2],rep.parms.names[grep("L_at_Amax_Mal",rep.parms.names)],rep.parms.names[grep("K_Mal",rep.parms.names)],rep.parms.names[grep("CV_young_Mal",rep.parms.names)],rep.parms.names[grep("CV_old_Mal",rep.parms.names)])
       parmnames<-input$myPicker_LP
         if(is.null(parmnames))
          {
            sendSweetAlert(
                session = session,
                title = "Missing parameter choices",
                text = "No parameters have been chosen to profile. Please select at least one.",
                type = "warning")
          }

        if(!is.null(parmnames))
          {
       parmnames_vec<-c("Steepness","lnR0","Natural mortality female","Linf female","k female", "CV@Lt young female","CV@Lt old female","Natural mortality male","Linf male","k male", "CV@Lt young male", "CV@Lt old male")
       prof_parms_names<-SS_parm_names[parmnames_vec%in%parmnames]
       
      #  prior_like<-starter.file$prior_like
      #  use_prior_like_in<-rep(0,length(prof_parms_names))
      #  if(prior_like==1){use_prior_like_in = rep(1,length(prof_parms_names))}
       mydir = dirname(pathLP())
       get = get_settings_profile(parameters =  prof_parms_names,
              low =  as.numeric(trimws(unlist(strsplit(input$Prof_Low_val,",")))),
              high = as.numeric(trimws(unlist(strsplit(input$Prof_Hi_val,",")))),
              step_size = as.numeric(trimws(unlist(strsplit(input$Prof_step,",")))),
              param_space = rep('real',length(as.numeric(trimws(unlist(strsplit(input$Prof_Low_val,","))))))
              #use_prior_like = use_prior_like_in
              )
       
       if(.Platform[["OS.type"]] == "windows"){os_exe <- "ss3"} 
       if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){os_exe <- "ss3_osx"} 
       if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){os_exe <- "ss3_osx_arm64"} 
       if(R.version[["os"]] == "linux-gnu"){os_exe <- "ss3_linux"}
       
       model_settings = get_settings(settings = list(base_name = basename(pathLP()),
                        run = "profile",
                        profile_details = get,
                        exe=os_exe,
                        prior_check = FALSE))


       try(run_diagnostics(mydir = mydir, model_settings = model_settings))

       #file.remove(paste0(dirname(mydir),"/run_diag_warning.txt"))

       output$LikeProf_plot_modout <- renderImage({
       image.path1<-normalizePath(file.path(paste0(pathLP(),"_profile_",prof_parms_names[1],"/parameter_panel_",prof_parms_names[1],".png")),mustWork=FALSE)
       return(list(
        src = image.path1,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

       output$LikeProf_plot_Piner <- renderImage({
       image.path2<-normalizePath(file.path(paste0(pathLP(),"_profile_",prof_parms_names[1],"/piner_panel_",prof_parms_names[1],".png")),mustWork=FALSE)
       return(list(
        src = image.path2,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

       output$LikeProf_plot_SO <- renderImage({
       image.path3<-normalizePath(file.path(paste0(pathLP(),"_profile_",prof_parms_names[1],"/",prof_parms_names[1],"_trajectories_compare1_spawnbio.png")),mustWork=FALSE)
       return(list(
        src = image.path3,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

       output$LikeProf_plot_SOt_SO0 <- renderImage({
       image.path4<-normalizePath(file.path(paste0(pathLP(),"_profile_",prof_parms_names[1],"/",prof_parms_names[1],"_trajectories_compare3_Bratio.png")),mustWork=FALSE)
       return(list(
        src = image.path4,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)
        }
       remove_modal_spinner()
})

observeEvent(input$run_MultiProfiles,{
       show_modal_spinner(spin="flower",color=wes_palettes$Darjeeling1[2],text="Multi-profiles running")
       refdir<-pathLP()
       mydir <- dirname(refdir)
       #Read in reference model
       ref.model<-SS_output(refdir)
       #Read in parameter files
       par.df <- fread(input$file_multi_profile$datapath,check.names=FALSE,data.table=FALSE)
       L <- readLines(input$file_multi_profile$datapath, n = 1)
       if(grepl(";", L)) {par.df <- read.csv2(input$file_multi_profile$datapath,check.names=FALSE)}
       SS_parm_names<-rownames(ref.model$parameters)[c(23:24,1,3,4:6,13,15:18)]
       parmnames_vec<-c("lnR0","Steepness","Natural mortality female","Linf female","k female", "CV@Lt young female","CV@Lt old female","Natural mortality male","Linf male","k male", "CV@Lt young male", "CV@Lt old male")
       parmnames<-colnames(par.df)
       prof_parms_names<-SS_parm_names[parmnames_vec%in%parmnames]
       modelnames<-paste0(parmnames[1]," ",par.df[,1],";",parmnames[2]," ",par.df[,2])
       #Make new folder
        #para = rownames(model_settings$profile_details)[aa]
        profile_dir <- paste0(refdir,"_profile_", paste(prof_parms_names,collapse="_"))
        dir.create(profile_dir, showWarnings = FALSE)
        if (length(list.files(profile_dir)) !=0) 
          {
            remove <- list.files(profile_dir)
            file.remove(file.path(profile_dir, remove))
          }
        all_files <- list.files(refdir)
        file.copy(from = file.path(refdir,all_files), to = profile_dir, overwrite = TRUE)
 
       #Set-up the starter file control file
       starter.file<-SS_readstarter(paste0(profile_dir,"/starter.ss"))
       ctlfile.in<-starter.file$ctlfile
       starter.file$ctlfile<-"control_modified.ss"
       starter.file$init_values_src<-0
       #starter.file$prior_like<-1
       SS_writestarter(starter.file,profile_dir,overwrite=TRUE)
#       low_in <-  as.numeric(trimws(unlist(strsplit(input$Prof_Low_val,",")))),
#       high_in <- as.numeric(trimws(unlist(strsplit(input$Prof_Hi_val,",")))),
#       step_size_in <- as.numeric(trimws(unlist(strsplit(input$Prof_step,","))))
#       par.df<-data.frame(mapply(function(x) seq(low[x],high[x],step_size[x]),x=1:length(low)))
#       colnames(par.df)<-prof_parms_names
      if(.Platform[["OS.type"]] == "windows"){os_exe <- "ss3"} 
      if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){os_exe <- "ss3_osx"} 
      if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){os_exe <- "ss3_osx_arm64"}
      if(R.version[["os"]] == "linux-gnu"){os_exe <- "ss3_linux"}
       
      if(input$Hess_multi_like==FALSE)
      {
        profile <- profile_multi(
          dir = profile_dir, # directory
          #globalpar = TRUE,
          oldctlfile = "control.ss_new",
          newctlfile = "control_modified.ss",
          string = prof_parms_names,
          profilevec = par.df,
          extras = "-nohess",
          prior_check=FALSE,
          exe = os_exe,
          show_in_console = TRUE
        )        
      }
       
      if(input$Hess_multi_like==TRUE)
      {
        profile <- profile_multi(
          dir = profile_dir, # directory
          #globalpar = TRUE,
          oldctlfile = "control.ss_new",
          newctlfile = "control_modified.ss",
          string = prof_parms_names,
          profilevec = par.df,
          prior_check=TRUE,
          exe = os_exe,
          show_in_console = TRUE
        )
      }

    # get model output
    profilemodels <- SSgetoutput(dirvec=profile_dir,keyvec=1:nrow(par.df), getcovar=FALSE)
    n <- length(profilemodels)
    profilesummary <- SSsummarize(profilemodels)
    try(SSplotComparisons(profilesummary, legendlabels = modelnames, ylimAdj = 1.30, new = FALSE,plot=FALSE,print=TRUE, legendloc = 'topleft',uncertainty=TRUE,plotdir=profile_dir,btarg=input$TRP_multi_like,minbthresh=input$LRP_multi_like))
    save(profilesummary,file=paste0(profile_dir,"/multiprofile.DMP"))
    # add total likelihood (row 1) to table created above
    par.df$like <- as.numeric(profilesummary$likelihoods[1, 1:n])
    par.df$likediff <- as.numeric(profilesummary$likelihoods[1, 1:n]-ref.model$likelihoods_used[1,1])
    par.df$Bratio <- as.numeric(profilesummary$Bratio[grep((profilesummary$endyrs[1]),profilesummary$Bratio$Label), 1:n])
    par.df$SB0 <- as.numeric(profilesummary$SpawnBio[1, 1:n])
    par.df$SBcurrent <- as.numeric(profilesummary$SpawnBio[grep((profilesummary$endyrs[1]),profilesummary$SpawnBio$Label), 1:n])
    SBcurrmax<-max(par.df$SBcurrent)
    colnames(par.df)<-c(parmnames,c("Likelihood","Likelihood_difference",paste0("SB",profilesummary$endyrs[1],"/SB0"),"SB0",paste0("SB",profilesummary$endyrs[1])))
    save(par.df,file=paste0(profile_dir,"/multiprofilelikelihoods.DMP"))
    write.csv(par.df,file=paste0(profile_dir,"/multiprofilelikelihoods.csv"))
    
    #Extract component likelihoods
    #Likelihoods
    likes_non0<-rowSums(profilesummary$likelihoods[,1:nrow(par.df)])>0
    likes_non0_par<-likes_non0_par.min<-profilesummary$likelihoods[likes_non0,]
    for(ii in 1:nrow(likes_non0_par))
    {
      likes_non0_par.min[ii,1:nrow(par.df)]<-likes_non0_par[ii,1:nrow(par.df)]-min(likes_non0_par[ii,1:nrow(par.df)])
    }
    colnames(likes_non0_par.min)<-c(par.df[,1],"Label")
    like.comps.plot<-reshape2::melt(likes_non0_par.min,id = "Label")

    #Plots
    LC.plot<-ggplot(like.comps.plot,aes(variable,value,group=Label,color=Label))+
      geom_point(size=5)+
      geom_line(lwd=1.2)+
      ylab("Difference in likelihood from the minimum") +
      theme(legend.position = c(0.8, 0.8))+
      labs(color = "Likelihood component") +
      scale_x_discrete(name = paste(parmnames[1],"and",parmnames[2]), 
                       breaks =par.df[,1], 
                       labels = paste0(par.df[,1],"\n",par.df[,2]))
    ggsave(paste0(profile_dir,"/","like_component_profile.png"),plot=LC.plot,width=10,height=10,units="in")

    #Lengths
    likes_length<-profilesummary$likelihoods_by_fleet[profilesummary$likelihoods_by_fleet$Label=="Length_like",]
    if(nrow(likes_length)>0)
    {
      likes_length_short<-likes_length[,3:ncol(likes_length)]
      likes_length_non0<-likes_length_non0.min<-data.frame(model=likes_length[,1],likes_length_short[,colSums(likes_length[,3:ncol(likes_length)])!=0])
      likes_length_non0$model<-likes_length_non0.min$model<-as.numeric(par.df[,1])
      for(ii in 2:ncol(likes_length_non0))
      {
        likes_length_non0.min[,ii]<-likes_length_non0[,ii]-min(likes_length_non0.min[,ii])
      }
      likes_length_non0.min.melt<-reshape2::melt(likes_length_non0.min,id.vars="model")
    
    #Plots
    LC_lt.plot<-ggplot(likes_length_non0.min.melt,aes(model,value,group=variable,color=variable))+
      geom_point(size=5)+
      geom_line(lwd=1.2)+
      theme(legend.position = c(0.8, 0.8))+
      ylab("Difference in likelihood from the minimum")+
      labs(color = "Length components") +
      scale_x_continuous(name = paste(parmnames[1],"and",parmnames[2]), 
                       breaks =par.df[,1], 
                       labels = paste0(par.df[,1],"\n",par.df[,2])) 
    ggsave(paste0(profile_dir,"/","length_component_profile.png"),plot=LC_lt.plot,width=10,height=10,units="in")


    }

    #Ages
    likes_age<-profilesummary$likelihoods_by_fleet[profilesummary$likelihoods_by_fleet$Label=="Age_like",]
    if(nrow(likes_age)>0)
    {
      likes_age_short<-likes_age[,3:ncol(likes_age)]
      likes_age_non0<-likes_age_non0.min<-data.frame(model=likes_age[,1],likes_age_short[,colSums(likes_age[,3:ncol(likes_age)])!=0])
      likes_age_non0$model<-likes_age_non0.min$model<-as.numeric(par.df[,1])
      for(ii in 2:ncol(likes_age_non0))
      {
        likes_age_non0.min[,ii]<-likes_age_non0[,ii]-min(likes_age_non0.min[,ii])
      }
        likes_age_non0.min.melt<-reshape2::melt(likes_age_non0.min,id.vars="model")
    
    #Plots
        LC_age.plot<-ggplot(likes_age_non0.min.melt,aes(model,value,group=variable,color=variable))+
      geom_point(size=5)+
      geom_line(lwd=1.2)+
      theme(legend.position = c(0.8, 0.8))+
      ylab("Difference in likelihood from the minimum")+
      labs(color = "Age components")+
      scale_x_continuous(name = paste(parmnames[1],"and",parmnames[2]), 
                         breaks =par.df[,1], 
                         labels = paste0(par.df[,1],"\n",par.df[,2])) 
      ggsave(paste0(profile_dir,"/","age_component_profile.png"),plot=LC_age.plot,width=10,height=10,units="in")
    }

    #Survey
    likes_survey<-profilesummary$likelihoods_by_fleet[profilesummary$likelihoods_by_fleet$Label=="Surv_like",]
    if(nrow(likes_survey)>0)
    {
      likes_survey_short<-likes_survey[,3:ncol(likes_survey)]
      likes_survey_non0<-likes_survey_non0.min<-data.frame(model=likes_survey[,1],likes_survey_short[,colSums(likes_survey[,3:ncol(likes_survey)])!=0])
      likes_survey_non0$model<-likes_survey_non0.min$model<-as.numeric(par.df[,1])
      for(ii in 2:ncol(likes_survey_non0))
      {
        likes_survey_non0.min[,ii]<-likes_survey_non0[,ii]-min(likes_survey_non0.min[,ii])
      }
      likes_survey_non0.min.melt<-reshape2::melt(likes_survey_non0.min,id.vars="model")
    
    #Plot
    LC_survey.plot<-ggplot(likes_survey_non0.min.melt,aes(model,value,group=variable,color=variable))+
      geom_point(size=5)+
      geom_line(lwd=1.2)+
      theme(legend.position = c(0.8, 0.8))+
      ylab("Difference in likelihood from the minimum")+
      labs(color = "Survey components")+
      scale_x_continuous(name = paste(parmnames[1],"and",parmnames[2]), 
                         breaks =par.df[,1], 
                         labels = paste0(par.df[,1],"\n",par.df[,2])) 
      ggsave(paste0(profile_dir,"/","survey_component_profile.png"),plot=LC_survey.plot,width=10,height=10,units="in")
    }


    #This reactive object is needed to get the plots to work
    plot.dat<-reactive({
      plot.dat<-reshape2::melt(par.df,id.vars=c( colnames(par.df)[1:2]),measure.vars=c("Likelihood_difference",paste0("SB",profilesummary$endyrs[1],"/SB0"),"SB0",paste0("SB",profilesummary$endyrs[1])))
      plot.dat
      })
    blank_data<- data.frame(variable = c("Likelihood_difference", "Likelihood_difference", paste0("SB",profilesummary$endyrs[1],"/SB0"), paste0("SB",profilesummary$endyrs[1],"/SB0"), "SB0", "SB0",paste0("SB",profilesummary$endyrs[1]),paste0("SB",profilesummary$endyrs[1])), x =min(par.df[,1]),y = c(min(par.df$Likelihood_difference),max(par.df$Likelihood_difference), 0, 1, 0, ceiling(max(par.df$SB0)),0,ceiling(SBcurrmax)))
    blank_data$variable<-factor(blank_data$variable,c("Likelihood_difference",paste0("SB",profilesummary$endyrs[1],"/SB0"),"SB0",paste0("SB",profilesummary$endyrs[1])))
    refmodel.dat<-data.frame(variable = c("Likelihood_difference",paste0("SB",profilesummary$endyrs[1],"/SB0"),"SB0",paste0("SB",profilesummary$endyrs[1])), x =ref.model$parameters[grep(prof_parms_names[1],ref.model$parameters$Label),3],y = c(0,ref.model$sprseries$Deplete[grep((profilesummary$endyrs[1]),profilesummary$Bratio$Label)+1],ref.model$SBzero,ref.model$derived_quants[grep((profilesummary$endyrs[1]),profilesummary$SpawnBio$Label),2]))
      #multiprofplotfun<-function(plot.dat)
      #{
      output$LikeProf_multiplot <- renderPlot({
      multiplot<-ggplot(plot.dat(),aes(plot.dat()[,1],value))+
      geom_line(lwd=1.25)+
      facet_wrap(~variable,scales="free_y")+
      geom_blank(data = blank_data, aes(x = x, y = y,z="variable"))+
      ylab("Difference in -log likelihood")+
      scale_x_continuous(name = paste(parmnames[1],"and",parmnames[2]), 
            breaks =par.df[,1], 
            labels = paste0(par.df[,1],"\n",par.df[,2]))+
      geom_hline(data = data.frame(yint=c(-1.96,0,1.96,0.4,0.25),variable=c("Likelihood_difference","Likelihood_difference","Likelihood_difference",paste0("SB",profilesummary$endyrs[1],"/SB0"),paste0("SB",profilesummary$endyrs[1],"/SB0"))), 
            aes(yintercept = yint), linetype = c("solid","dotted","solid","dotted","solid"),color=c("red","black","red","darkgreen","red"),lwd=1)+
      geom_point(data=refmodel.dat,aes(x=x,y=y),color="blue",size=4)+
      theme_bw()
      ggsave(paste0(profile_dir,"/","multilikelihood_profile.png"),width=10,height=10,units="in")
      multiplot      
      })
    #}
      
    
    # output$LikeProf_multiplot <- renderPlot({
    #   plotPNG(func=multiprofplotfun(plot.dat()),paste0(profile_dir,"/",paste(parmnames,collapse="_"),"_multilikelihood_profile.png"))
    #   })

#   plot.dat2<-reactive({
#       plot.dat2<-melt(par.df,id.vars=c( colnames(par.df)[1:2]),measure.vars=c("Likelihood_difference",paste0("SB",profilesummary$endyrs[1]-1,"/SB0"),"SB0",paste0("SB",profilesummary$endyrs[1]-1)))
#       plot.dat2
#       })
    
#     png(file = paste0(profile_dir,"/","multilikelihood_profile.png"),width = 10, height = 10, units = "in", res = 300, pointsize = pt)
# #    multiplot
#      ggplot(plot.dat2(),aes(plot.dat2()[,1],value))+
#       geom_line(lwd=1.25)+
#       facet_wrap(~variable,scales="free_y")+
#       #geom_blank(data = blank_data, aes(x = x, y = y,z="variable"))+
#       ylab("Difference in -log likelihood")+
#       #scale_x_continuous(name = paste(parmnames[1],"and",parmnames[2]), 
#       #      breaks =par.df[,1], 
#       #      labels = paste0(par.df[,1],"\n",par.df[,2]))+
#       geom_hline(data = data.frame(yint=c(-1.96,0,1.96,0.4,0.25),variable=c("Likelihood_difference","Likelihood_difference","Likelihood_difference",paste0("SB",profilesummary$endyrs[1]-1,"/SB0"),paste0("SB",profilesummary$endyrs[1]-1,"/SB0"))), 
#             aes(yintercept = yint), linetype = c("solid","dotted","solid","dotted","solid"),color=c("red","black","red","darkgreen","red"),lwd=1)+
#       #geom_point(data=refmodel.dat,aes(x=x,y=y),color="blue",size=3)+
#       theme_bw()      # multiprofplot
    #dev.off()
    # png(file = paste0(profile_dir,"/",paste(parmnames,collapse="_"),"_multilikelihood_profile.png"),width = 10, height = 10, units = "in", res = 300, pointsize = pt)
    
    # output$LikeProf_multiplot <- renderImage({
    #    image.path<-normalizePath(file.path(paste0(profile_dir,paste0("\\",paste(parmnames,collapse="_"),"_multilikelihood_profile.png"))),mustWork=FALSE)
    #    return(list(
    #     src = image.path,
    #     contentType = "image/png",
    #    #  width = 400,
    #    # height = 300,
    #    style='height:60vh'))
    #   },deleteFile=FALSE)

  
    # reshape data frame into a matrix for use with contour
    
    # pngfun(wd = mydir, file = paste0("contour_profile.png"), h = 7,w = 12)
    # contour(x = as.numeric(rownames(like_matrix)),
    #         y = as.numeric(colnames(like_matrix)),
    #         z = like_matrix)
    # dev.off()
           
    
    # make contour plot

      #  output$LikeProf_multi_contour <- renderPlot({
      #   like_matrix <- reshape2::acast(par.df, colnames(par.df)[1]~colnames()[2], value.var="like")
      #   pngfun(wd = mydir, file = paste0("contour_profile.png"), h = 7,w = 12)
      #   contour(x = as.numeric(rownames(like_matrix)),
      #       y = as.numeric(colnames(like_matrix)),
      #       z = like_matrix)
      #       dev.off()  
      # })
      remove_modal_spinner()
  })

#################

###############################
####### Retrospectives ########
###############################
  shinyDirChoose(input,"Retro_dir", roots=roots,session=session, filetypes=c('', 'txt'))
  pathRetro <- reactive({
        return(parseDirPath(roots, input$Retro_dir))
      })

  observeEvent(input$Retro_dir,{
  output$RetroPath <- renderText({paste0("Selected model folder:\n", pathRetro())})
  })

  observeEvent(input$run_Retro_comps,{
    if(.Platform[["OS.type"]] == "windows"){os_exe <- "ss3"} 
    if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){os_exe <- "ss3_osx"} 
    if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){os_exe <- "ss3_osx_arm64"} 
    if(R.version[["os"]] == "linux-gnu"){os_exe <- "ss3_linux"}
   #if(input$run_Retro_comps){           
     show_modal_spinner(spin="flower",color=wes_palettes$Royal1[1],text="Running retrospectives")
     mydir_in<-dirname(pathRetro())
     scenario_in<-basename(pathRetro())
     model_settings = get_settings(settings = list(
                        base_name = scenario_in,
                        run = "retro",
                        retro_yrs = input$first_retro_year_in:input$final_retro_year_in,
                        exe = os_exe)
                        )
     run_diagnostics(mydir = mydir_in, model_settings = model_settings)
    # tryCatch({
    #     run_diagnostics(mydir = mydir_in, model_settings = model_settings)
    # },
    # warning = function(warn){
    #     showNotification(paste0(warn), type = 'warning')
    # },
    # error = function(err){
    #     showNotification(paste0(err), type = 'err')
    # })
  #} 

      output$Retro_comp_plotSB <- renderImage({
       image.path<-normalizePath(file.path(paste0(pathRetro(),"_retro/compare2_spawnbio_uncertainty.png")),mustWork=FALSE)
       return(list(
        src = image.path,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

      output$Retro_comp_plotBratio <- renderImage({
       image.path<-normalizePath(file.path(paste0(pathRetro(),"_retro/compare4_Bratio_uncertainty.png")),mustWork=FALSE)
       return(list(
        src = image.path,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

    remove_modal_spinner()
  })

##############################

###############################
### Sensitivity comparisons ###
###############################

  pathSensi <- reactive({
  shinyDirChoose(input, "Sensi_dir", roots=roots,session=session, filetypes=c('', 'txt'))
     return(parseDirPath(roots, input$Sensi_dir))
   })

  observeEvent(input$Sensi_dir,{
  output$SensiPath <- renderText({paste0("Selected model scenario folder:\n", pathSensi())})
  })

  observeEvent(as.numeric(input$tabs)==6,{
  output$Sensi_model_Ref<-renderUI({
      #dirinfo <- parseDirPath(roots, input$Sensi_dir)
      pickerInput(
      inputId = "myPicker_Ref",
      label = "Choose reference model",
      #choices = list.files(dirinfo),
      choices = list.files(pathSensi()),
      options = list(
        `actions-box` = TRUE,
        size = 12,
        `selected-text-format` = "count > 3"
        ),
      multiple = FALSE
    )
 })    
  })

 observeEvent(!is.null(input$myPicker_Ref),{
#   observeEvent(as.numeric(input$tabs)==6,{
  output$Sensi_model_picks<-renderUI({
      #dirinfo <- parseDirPath(roots, input$Sensi_dir)
      pickerInput(
      inputId = "myPicker",
      label = "Choose scenarios to compare to reference model",
      #choices = list.files(dirinfo),
      choices = list.files(pathSensi()),
      options = list(
        `actions-box` = TRUE,
        size = 12,
        `selected-text-format` = "count > 3"
        ),
      multiple = TRUE
    )
 })    
  })


#SS.comparisons<-observeEvent(as.numeric(input$tabs)==5,{
Sensi_model_dir_out<-eventReactive(req(input$run_Sensi_comps&!is.null(input$myPicker)&as.numeric(input$tabs)==6),{
    if(!file.exists(paste0(pathSensi(),"/Sensitivity Comparison Plots")))
      {
        dir.create(paste0(pathSensi(),"/Sensitivity Comparison Plots"))
      }
    Sensi_model_dir_out_Ref<-paste0(pathSensi(),"/",input$myPicker_Ref)
    Sensi_model_dir_sensi<-paste0(pathSensi(),"/",input$myPicker)
    Sensi_model_dir<-c(Sensi_model_dir_out_Ref,Sensi_model_dir_sensi)
    Sensi_model_dir
  })

#&exists(Sensi_model_dir_out())
  observeEvent(req(input$run_Sensi_comps),{
      show_modal_spinner(spin="flower",color=wes_palettes$Rushmore[1],text="Comparisons running")
      
       modelnames<-c(input$myPicker_Ref,input$myPicker)
       zz<-list()
       Runs<-length(Sensi_model_dir_out())
       for(i in 1:Runs) {zz[[i]]<-SS_output(paste0(Sensi_model_dir_out()[i]))}
       modsummary.sensi<- SSsummarize(zz)

       col.vec = rc(n=length(modelnames), alpha = 1)
       shade = adjustcolor(col.vec[1], alpha.f = 0.10)

       TRP.in<-input$Sensi_TRP
       LRP.in<-input$Sensi_LRP
       if(is.na(TRP.in)){TRP.in<-0}
       if(is.na(LRP.in)){LRP.in<-0}
       
       dir.create(paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file))
       #Sensi_uncertainty_choice<-input$Sensi_uncertainty_choice
       #if (all(is.na(quantsSD[, i]) | quantsSD[, i] == 0))
       Sensi_uncertainty_choice<-TRUE

       pngfun(wd = paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file), file = paste0(input$Sensi_comp_file,".png"), h = 7,w = 12)
       par(mfrow = c(1,3))
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30, subplot = c(2,4),col = col.vec, new = FALSE,btarg=TRP.in,minbthresh=LRP.in,uncertainty=Sensi_uncertainty_choice))
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30, subplot = 11,col = col.vec, new = FALSE, legendloc = 'topleft',btarg=TRP.in,minbthresh=LRP.in,uncertainty=Sensi_uncertainty_choice))
       dev.off()
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30,col = col.vec, new = FALSE,print=TRUE, legendloc = 'topleft',btarg=TRP.in,minbthresh=LRP.in,uncertainty=Sensi_uncertainty_choice,plotdir=paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file)))
       save(modsummary.sensi,file=paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/",input$Sensi_comp_file,".DMP"))

       pngfun(wd = paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file), file = paste0(input$Sensi_comp_file,"_no_uncertainty.png"), h = 7,w = 12)
       par(mfrow = c(1,3))
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30, subplot = c(1,3),col = col.vec, new = FALSE,btarg=TRP.in,minbthresh=LRP.in,uncertainty=Sensi_uncertainty_choice))
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30, subplot = 11,col = col.vec, new = FALSE, legendloc = 'topleft',btarg=TRP.in,minbthresh=LRP.in,uncertainty=Sensi_uncertainty_choice))
       dev.off()
       
       output$Sensi_comp_plot <- renderImage({
       if (all(is.na(modsummary.sensi$quantsSD[, 1]) | modsummary.sensi$quantsSD[, 1] == 0))
       {
        image.path<-normalizePath(file.path(paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/",input$Sensi_comp_file, '_no_uncertainty.png')),mustWork=FALSE)
          return(list(
          src = image.path,
          contentType = "image/png",
          # width = 400,
          # height = 300,
          style='height:60vh'))
        }
        else
        {
        image.path<-normalizePath(file.path(paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/",input$Sensi_comp_file, '.png')),mustWork=FALSE)
          return(list(
          src = image.path,
          contentType = "image/png",
          # width = 400,
          # height = 300,
          style='height:60vh'))          
        }
      },deleteFile=FALSE)

#Relative error sensitivity plots
SensiRE_breaks_in<-as.numeric(trimws(unlist(strsplit(input$SensiRE_breaks,","))))
SensiRE_xcenter_in<-as.numeric(trimws(unlist(strsplit(input$SensiRE_xcenter,","))))
SensiRE_ycenter_in<-as.numeric(trimws(unlist(strsplit(input$SensiRE_ycenter,","))))
SensiRE_headers_in<-trimws(unlist(strsplit(input$SensiRE_headers,",")))
yminmax_sensi<-rep(c(input$SensiRE_ymin,input$SensiRE_ymax),5)
#r4ss::SS_Sensi_plot(dir=paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/"),
Sensi_plot_horiz(model.summaries=modsummary.sensi,
              dir=paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/"),
              current.year=modsummary.sensi$endyrs[1]+1,
              mod.names=modelnames, #List the names of the sensitivity runs
              #likelihood.out=c(0,0,0),
              #Sensi.RE.out="Sensi_RE_out.DMP", #Saved file of relative errors
              #CI=0.95, #Confidence interval box based on the reference model
              TRP.in=input$Sensi_TRP, #Target relative abundance value
              LRP.in=input$Sensi_LRP, #Limit relative abundance value
              sensi_xlab="Sensitivity scenarios", #X-axis label
              ylims.in=yminmax_sensi, #Y-axis label
              plot.figs=c(1,1,1,1,1,1), #Which plots to make/save? 
              sensi.type.breaks=SensiRE_breaks_in, #vertical breaks that can separate out types of sensitivities
              anno.x=SensiRE_xcenter_in, # Vertical positioning of the sensitivity types labels
              anno.y=SensiRE_ycenter_in, # Horizontal positioning of the sensitivity types labels
              anno.lab=SensiRE_headers_in, #Sensitivity types labels
              header.text=input$SensiRE_headers_text,
              horizontal = TRUE
)

       output$SensiRE_comp_plot <- renderImage({
       image.path<-normalizePath(file.path(paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/Sensi_REplot_SB_Dep_F_Yield.png")),mustWork=FALSE)
       return(list(
        src = image.path,
        contentType = "image/png",
         width = 800,
         height = 1200,
       style='height:60vh'))
      },deleteFile=FALSE)

       output$SensiRElog_comp_plot <- renderImage({
       image.path<-normalizePath(file.path(paste0(pathSensi(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,"/Sensi_logREplot_SB_Dep_F_Yield.png")),mustWork=FALSE)
       return(list(
        src = image.path,
        contentType = "image/png",
         width = 800,
         height = 1200,
       style='height:60vh'))
      },deleteFile=FALSE)

    remove_modal_spinner()
  })
#############################

#############################

# image.path<-eventReactive(exists(file.path(paste0(path1(),"/Sensitivity Comparison Plots/",
#                input$Sensi_comp_file, '.png'))),{
#   image.path<-normalizePath(file.path(paste0(path1(),"/Sensitivity Comparison Plots/",
#                input$Sensi_comp_file, '.png')),mustWork=FALSE)
#   })

# output$Sensi_comp_plot <- renderImage({
#        image.path<-normalizePath(file.path(paste0(path1(),"/Sensitivity Comparison Plots/",
#                input$Sensi_comp_file, '.png')),mustWork=FALSE)
#        return(list(
#         src = image.path,
#         contentType = "image/png",
#        #  width = 400,
#        # height = 300,
#        style='height:60vh'))
#   print(input$run_Sensi_comps[1])
# },deleteFile=FALSE)
####################################


##########################
### Ensemble modelling ###
##########################

pathEnsemble <- reactive({
      shinyDirChoose(input, "Ensemble_dir", roots=roots, filetypes=c('', 'txt'))
      return(parseDirPath(roots, input$Ensemble_dir))
  })

observeEvent(input$Ensemble_dir,{
  output$EnsemblePath <- renderText({paste0("Selected model scenario folder:\n", pathEnsemble())})
  })

#Used to have as.numeric(input$tabs)==4
 observeEvent(as.numeric(input$tabs)==7,{      
  output$Ensemble_model_picks<-renderUI({
      pickerInput(
      inputId = "myEnsemble",
      label = "Choose scenarios to ensemble",
      choices = list.files(pathEnsemble()),
      options = list(
        `actions-box` = TRUE,
        size = 12,
        `selected-text-format` = "count > 3"
        ),
      multiple = TRUE
      )
    })
  })
#Ensemble_model_dir_out<-eventReactive(req(input$run_Ensemble&!is.null(input$myEnsemble)&as.numeric(input$tabs)==6),{
observeEvent(req(input$run_Ensemble&!is.null(input$myEnsemble)&as.numeric(input$tabs)==7),{
show_modal_spinner(spin="flower",color=wes_palettes$Rushmore[1],text="Prepare models to combine into ensembles")
#Ensemble_model_dir_out<-eventReactive(input$run_Ensemble,{
#Ensemble.outputs<-eventReactive(input$run_Ensemble,{
    if(!file.exists(paste0(pathEnsemble(),"/Ensemble outputs")))
      {
        dir.create(paste0(pathEnsemble(),"/Ensemble outputs"))
      }
    Ensemble_model_dir_out<-paste0(pathEnsemble(),"/Ensemble outputs/",input$Ensemble_file)
    dir.create(Ensemble_model_dir_out)

#  })


# print(Ensemble_model_dir_out())
# exists("Ensemble_model_dir_out()")
#Ensemble_model_dir_out
#})
#exists(Ensemble_model_dir_out())
#  observeEvent(req(input$run_Ensemble&!is.null(input$myEnsemble)),{
#  Ensemble.outputs<-eventReactive(input$run_Ensemble,{
       modelnames<-input$myEnsemble
       zz<-list()
       Runs<-length(input$myEnsemble)
       for(i in 1:Runs) {zz[[i]]<-SS_output(paste0(pathEnsemble(),"/",input$myEnsemble[i]))}
       modsummary.ensemble<- SSsummarize(zz)
       Ensemble_wts<-as.numeric(trimws(unlist(strsplit(input$Ensemble_wts,","))))
       Stand_ensemble_wts<-Ensemble_wts/sum(Ensemble_wts)
       Nsamps_ensemble<-10000
       Nsamps_ensemble_wts<-round(Nsamps_ensemble*Stand_ensemble_wts)
       #Calculate weighted values
       mean.fxn <- function(x, y) rnorm(numdraws, mean = x, sd = y)
       #Spawning outputs
       #Bratio
       SpOt_en<-Bratio_en<-F_en<-SPR_en<-list()
       SO_0<-SO_t<-Bratio_t<-F_t<-SPR_t<-data.frame(Year=NA,Metric=NA,Model=NA)
       #Create weighted ensembles
       for (i in 1:length(Nsamps_ensemble_wts))
       {
         numdraws<-Nsamps_ensemble_wts[i]
         SpOt_en[[i]]<-Map(mean.fxn,modsummary.ensemble$SpawnBio[,i],modsummary.ensemble$SpawnBioSD[,i])
         names(SpOt_en[[i]])<-modsummary.ensemble$SpawnBio$Yr
         SO_0<-rbind(SO_0,data.frame(Year=as.numeric(names(SpOt_en[[i]][1])),Metric=unlist(SpOt_en[[i]][1]),Model=input$myEnsemble[i]))
         SO_t<-rbind(SO_t,data.frame(Year=names(SpOt_en[[i]][nrow(modsummary.ensemble$SpawnBio)]),Metric=unlist(SpOt_en[[i]][nrow(modsummary.ensemble$SpawnBio)]),Model=input$myEnsemble[i]))
         Bratio_en[[i]]<-Map(mean.fxn,modsummary.ensemble$Bratio[,i],modsummary.ensemble$BratioSD[,i])               
         names(Bratio_en[[i]])<-modsummary.ensemble$Bratio$Yr       
         Bratio_t<-rbind(Bratio_t,data.frame(Year=names(Bratio_en[[i]][nrow(modsummary.ensemble$Bratio)]),Metric=unlist(Bratio_en[[i]][nrow(modsummary.ensemble$Bratio)]),Model=input$myEnsemble[i]))
         F_en[[i]]<-Map(mean.fxn,modsummary.ensemble$Fvalue[,i],modsummary.ensemble$FvalueSD[,i])               
         names(F_en[[i]])<-modsummary.ensemble$Fvalue$Yr       
         F_t<-rbind(F_t,data.frame(Year=names(F_en[[i]][nrow(modsummary.ensemble$Fvalue)]),Metric=unlist(F_en[[i]][nrow(modsummary.ensemble$Fvalue)]),Model=input$myEnsemble[i]))
         SPR_en[[i]]<-Map(mean.fxn,modsummary.ensemble$SPRratio[,i],modsummary.ensemble$SPRratioSD[,i])               
         names(SPR_en[[i]])<-modsummary.ensemble$SPRratio$Yr       
         SPR_t<-rbind(SPR_t,data.frame(Year=names(SPR_en[[i]][nrow(modsummary.ensemble$SPRratio)]),Metric=unlist(SPR_en[[i]][nrow(modsummary.ensemble$SPRratio)]),Model=input$myEnsemble[i]))
       }

       #Reduce(intersect,list(names(list1),names(list2),names(list3))) # Code to find matches in multiple vectors. For future option of mixing models with different dimensions.

       #Assemble ensembles
       
       Ensemble_SO<-do.call(rbind.data.frame, SpOt_en)
       colnames(Ensemble_SO)<-names(SpOt_en[[1]])
       Ensemble_Bratio<-do.call(rbind.data.frame, Bratio_en)
       Ensemble_F<-do.call(rbind.data.frame, F_en)
       Ensemble_SPR<-do.call(rbind.data.frame, SPR_en)
       colnames(Ensemble_Bratio)<-colnames(Ensemble_F)<-colnames(Ensemble_SPR)<-names(Bratio_en[[1]])
       # for(ii in 2:length(Nsamps_ensemble_wts))
       # {
       #   Ensemble_SO<-mapply(c,Ensemble_SO,SpOt_en[[ii]])
       #   Ensemble_Bratio<-mapply(c,Ensemble_Bratio,Bratio_en[[ii]])
       #   Ensemble_F<-mapply(c,Ensemble_F,F_en[[ii]])
       #   Ensemble_SPR<-mapply(c,Ensemble_SPR,SPR_en[[ii]])
       # }
       
       SO_0<-rbind(SO_0[-1,],data.frame(Year=as.numeric(colnames(Ensemble_SO)[1]),Metric=Ensemble_SO[,1],Model="Ensemble"))
       SO_t<-rbind(SO_t[-1,],data.frame(Year=as.numeric(colnames(Ensemble_SO)[ncol(Ensemble_SO)]),Metric=Ensemble_SO[,ncol(Ensemble_SO)],Model="Ensemble"))
       Bratio_t<-rbind(Bratio_t[-1,],data.frame(Year=as.numeric(colnames(Ensemble_Bratio)[ncol(Ensemble_Bratio)]),Metric=Ensemble_Bratio[,ncol(Ensemble_Bratio)],Model="Ensemble"))
       F_t<-rbind(F_t[-1,],data.frame(Year=as.numeric(colnames(Ensemble_F)[ncol(Ensemble_F)]),Metric=Ensemble_F[,ncol(Ensemble_F)],Model="Ensemble"))
       SPR_t<-rbind(SPR_t[-1,],data.frame(Year=as.numeric(colnames(Ensemble_SPR)[ncol(Ensemble_SPR)]),Metric=Ensemble_SPR[,ncol(Ensemble_SPR)],Model="Ensemble"))

       SO_0$Year<-as.factor(SO_0$Year)
       SO_t$Year<-as.factor(SO_t$Year)
       Bratio_t$Year<-as.factor(Bratio_t$Year)
       F_t$Year<-as.factor(F_t$Year)
       SPR_t$Year<-as.factor(SPR_t$Year)

#       mean_cl_quantile <- function(x, q = c(0.1, 0.9), na.rm = TRUE){
#                    dat <- data.frame(y = mean(x, na.rm = na.rm),
#                    ymin = quantile(x, probs = q[1], na.rm = na.rm),
#                    ymax = quantile(x, probs = q[2], na.rm = na.rm))
#                    return(dat)
#        }

       
      show_modal_spinner(spin="flower",color=wes_palettes$Rushmore[2],text="Preparing ensemble plots")

       #Boxplots
       SO_0$Model.labs<-SO_t$Model.labs<-Bratio_t$Model.labs<-F_t$Model.labs<-SPR_t$Model.labs<- str_wrap(SO_0$Model, width = 5)
       gg1<-ggplot(SO_0,aes(Model.labs,Metric,fill=Model))+
        geom_violin()+
        ylab("Initial Spawning Output")+
        theme(legend.position = "none")
       gg2<-ggplot(SO_t,aes(Model.labs,Metric,fill=Model))+
        geom_violin()+
        ylab("Terminal Year Spawning Output")+
        theme(legend.position = "none")
       gg3<-ggplot(Bratio_t,aes(Model.labs,Metric,fill=Model))+
        geom_violin()+
        ylab("Relative stock status")+
        theme(legend.position = "none")
       gg4<-ggplot(F_t,aes(Model.labs,Metric,fill=Model))+
        geom_violin()+
        ylab("Fishing mortality")+
        theme(legend.position = "none")
       gg5<-ggplot(SPR_t,aes(Model.labs,Metric,fill=Model))+
        geom_violin()+
        ylab("1-SPR")+
        theme(legend.position = "none")

        ggarrange(gg1,gg2,gg3,gg4,gg5)
        ggsave(paste0(Ensemble_model_dir_out,"/Ensemble_comp_plots.png"),width=20,height=10)
             output$Ensemble_plots <- renderPlot({ 
       ggarrange(gg1,gg2,gg3,gg4,gg5)})

      #Spawning Output plot
      Ensemble_SO_plot<-reshape2::melt(Ensemble_SO,value.name="SO")
      colnames(Ensemble_SO_plot)<-c("Year","SO")
      Ensemble_SO_plot$Year<-as.factor(Ensemble_SO_plot$Year)
      SO.ts.plot<-ggplot(Ensemble_SO_plot,aes(Year,SO,fill=Year))+
        geom_violin(scale="count")+
        theme(legend.position="none")+
        theme(axis.text.x = element_text(angle = 65, hjust = 0.1,vjust=0))+
        ylab("Spawning Output")
      ggsave(paste0(Ensemble_model_dir_out,"/Ensemble_SO.png"),width=20,height=10)
         output$Ensemble_plots_SO_ts <- renderPlot({ 
       SO.ts.plot})

      #Relative stock status plot
      Ensemble_Bratio_plot<-reshape2::melt(Ensemble_Bratio,value.name="Bratio")
      colnames(Ensemble_Bratio_plot)<-c("Year","Bratio")
      Ensemble_Bratio_plot$Year<-as.factor(Ensemble_Bratio_plot$Year)
      Bratio.ts.plot<-ggplot(Ensemble_Bratio_plot,aes(Year,Bratio,fill=Year))+
        geom_violin(bw=0.01)+
        theme(legend.position="none")+
        theme(axis.text.x = element_text(angle = 65, hjust = 0.1,vjust=0))+
        ylab("SBt/SO0")
      ggsave(paste0(Ensemble_model_dir_out,"/Ensemble_Bratio.png"),width=20,height=10)
         output$Ensemble_plots_Bratio_ts <- renderPlot({ 
       Bratio.ts.plot})

      #F plot
      Ensemble_F_plot<-reshape2::melt(Ensemble_F,value.name="F")
      colnames(Ensemble_F_plot)<-c("Year","F")
      Ensemble_F_plot$Year<-as.factor(Ensemble_F_plot$Year)
      ggplot(Ensemble_F_plot,aes(Year,F,fill=Year))+
        geom_violin(bw=0.001)+
        theme(legend.position="none")+
        theme(axis.text.x = element_text(angle = 65, hjust = 0.1,vjust=0))+
        ylab("Fishing mortality")
      ggsave(paste0(Ensemble_model_dir_out,"/Ensemble_F.png"),width=20,height=10)

      #1-SPR plot
      Ensemble_SPR_plot<-reshape2::melt(Ensemble_SPR,value.name="SPR")
      colnames(Ensemble_SPR_plot)<-c("Year","SPR")
      Ensemble_SPR_plot$Year<-as.factor(Ensemble_SPR_plot$Year)
      ggplot(Ensemble_SPR_plot,aes(Year,SPR,fill=Year))+
        geom_violin(bw=0.01)+
        theme(legend.position="none")+
        theme(axis.text.x = element_text(angle = 65, hjust = 0.1,vjust=0))+
        ylab("1-SPR")
      ggsave(paste0(Ensemble_model_dir_out,"/Ensemble_SPR.png"),width=20,height=10)

      #Get simpler plots for SB0, SBcurrent, RSS, F, and SPR in terminal year

#      ggplot(reshape2::melt(Ensemble_Bratio,value.name="Bratio"),aes(Var2,Bratio))+
#        stat_summary(geom = "line", fun  = median)+
#        ylim(0,1)+
#        stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.3)
      #Make outputs
      show_modal_spinner(spin="flower",color=wes_palettes$Rushmore[3],text="Saving ensemble objects")
       Model.outputs<-list("Spawning Output"=SpOt_en,"Relative Stock Status"=Bratio_en,"Fishing mortality"=F_en,"1-SPR"=SPR_en)
       Ensemble.outputs<-list("Spawning Output"=Ensemble_SO,"Relative Stock Status"=Ensemble_Bratio,"Fishing mortality"=Ensemble_F,"1-SPR"=Ensemble_SPR)
       Ensemble.outputs.plots<-list("Spawning Output"=Ensemble_SO_plot,"Relative Stock Status"=Ensemble_Bratio_plot,"Fishing mortality"=Ensemble_F_plot,"1-SPR"=Ensemble_SPR_plot)
       save(Model.outputs,file=paste0(Ensemble_model_dir_out,"/Model_results",".DMP"))
       save(Ensemble.outputs,file=paste0(Ensemble_model_dir_out,"/Ensemble_results",".DMP"))
       save(Ensemble.outputs.plots,file=paste0(Ensemble_model_dir_out,"/Ensemble_results_plots",".DMP"))
    remove_modal_spinner()       
#       return(Ensemble.outputs)
    })
  
  observeEvent(input$loadInputs, {
      sessionName <- file_path_sans_ext(input$loadInputs$name)
      sessionName <- str_replace_all(sessionName, "[^[:alnum:]]", "")
      #use doBookmark to get the file location of bookmarks
      session$doBookmark()
      targetPath <- file.path(dirname(bookmarkFilePath()), sessionName, "input.rds")
      dir_delete(bookmarkFilePath()) #delete bookmark created from session$doBookmark as we are just using it to grab the filepath

      if(!dir.exists(dirname(targetPath))) {
        dir.create(dirname(targetPath), recursive = TRUE)
      }

      file.copy(
        from = input$loadInputs$datapath,
        to = targetPath,
        overwrite = TRUE
      )

      restoreURL <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, session$clientData$url_pathname, "?_state_id_=", sessionName)

      # redirect user to restoreURL
      runjs(sprintf("window.location = '%s';", restoreURL))

    })

    latestBookmarkURL <- reactiveVal()
    bookmarkFilePath <- reactiveVal()
    
    onBookmark(function(state) {
      bookmarkFilePath(state$dir) #don't use dirname here as we need the specific folder name for deletion
    })

    onBookmarked(
      fun = function(url) {
        latestBookmarkURL(parseQueryString(url))
      }
    )
    
    setBookmarkExclude(c("file1","file2","file3","file4", "run_SS", "run_SSS"))

    onRestored(function(state) {

      output$input_file_text <- renderText({
        paste0("<span style=\"color:#5D9741\">Restored session from ",basename(state$dir),".rds</span>")
        })
    #   showNotification(paste("Restored session:", basename(state$dir)), duration = 10, type = "message")
    #   showModal(modalDialog(
    #     title = "Inputs Loaded",
    #     paste("Restored session:", basename(state$dir))
    #   ))
    })

  #})

#observeEvent(req(input$run_Ensemble&exists("Ensemble.outputs()")),{
 #   
 # })
       #Create figures of weighted values


      #  output$Sensi_comp_plot <- renderImage({
      #  image.path<-normalizePath(file.path(paste0(path1(),"/Sensitivity Comparison Plots/",
      #          input$Sensi_comp_file, '.png')),mustWork=FALSE)
      #  return(list(
      #   src = image.path,
      #   contentType = "image/png",
      #  #  width = 400,
      #  # height = 300,
      #  style='height:60vh'))
      # },deleteFile=FALSE)
})

#enableBookmarking(store = "server")
