require(shiny)
require(shinyjs)
require(r4ss)
require(dplyr)
require(ggplot2)
require(reshape2)
require(dplyr) 
require(tidyr)
require(rlist)
require(viridis)
require(sss)
require(shinyWidgets)
require(shinyFiles)
#require(paletteer)
#require(RColorBrewer)
#require(ggthemes)

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
VBGF<-function(Linf, k, t0, ages){ 
   Linf * (1 - exp(-k * (ages - t0))) 
  } 


VBGF.age<-function(Linf,k,t0,lt){ 
    t0 - (log(1 - (lt / Linf)) / k) 
  } 
  

RUN.SS<-function(path,ss.cmd=" -nohess -nox",OS.in="Windows"){ 
  navigate <- paste("cd ", path, sep="") 
if(OS.in=="Windows") 
  {
    command <- paste0(navigate," & ", "ss", ss.cmd) 
    shell(command, invisible=TRUE, translate=TRUE)
  } 
if(OS.in=="Mac")  
  {
    
    command <- c(paste("cd", path), "chmod +x ./ss_mac","./ss_mac") 
    system(paste(command, collapse=";"),invisible=TRUE)
    
    #command <- paste0(path,"/./ss_mac", ss.cmd) 
    #system(command, invisible=TRUE)
  } 
if(OS.in=="Linux") 
  {
    command <- c(paste("cd", path), "chmod +x ./ss_linux","./ss_linux") 
    system(paste(command, collapse=";"), invisible=TRUE)
  } 
}  

pngfun <- function(wd, file,w=7,h=7,pt=12){
  file <- file.path(wd, file)
  cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}

rc <- function(n,alpha=1){
  # a subset of rich.colors by Arni Magnusson from the gregmisc package
  # a.k.a. rich.colors.short, but put directly in this function
  # to try to diagnose problem with transparency on one computer
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
}

########## Clear data files and plots ############
  rv.Lt <- reactiveValues(data = NULL,clear = FALSE)
  rv.Age <- reactiveValues(data = NULL,clear = FALSE)
  rv.Ct <- reactiveValues(data = NULL,clear = FALSE)
  

########
#Reset catches
  observe({
    req(input$file2)
    req(!rv.Ct$clear)
    rv.Ct$data <- read.csv(input$file2$datapath,check.names=FALSE)
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
    rv.Lt$data <- read.csv(input$file1$datapath,check.names=FALSE)
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
    rv.Age$data <- read.csv(input$file3$datapath,check.names=FALSE)
  })

  observeEvent(input$file3, {
    rv.Age$clear <- FALSE
  }, priority = 1000)

  observeEvent(input$reset_age, {
    rv.Age$data <- NULL
    rv.Age$clear <- TRUE
    reset('file3')
  }, priority = 1000)


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
#shinyjs::hide("OS_choice")
#shinyjs::hide("run_SS")
#shinyjs::hide("run_SSS")
  })

#To get the ObserveEvent to work, each statement in req needs to be unique.
#This explains the workaround of ((as.numeric(input$tabs)*x)/x)<4, where x is the unique type of assessment being run
#This input allows other tabs to have different side panels.

#Switch back to data from different tabs
observeEvent(req(((as.numeric(input$tabs)*99)/99)<4), {
        shinyjs::show("Data_panel")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        

        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")
        
        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")        
  
        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")
  })

#SSS panels
observeEvent(req(((as.numeric(input$tabs)*1)/1)<4&is.null(rv.Lt$data)&!is.null(rv.Ct$data)&is.null(rv.Age$data)), {
        shinyjs::show("Data_panel")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::show("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::show("panel_SS_stock_status") 

        shinyjs::show("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::show("panel_selectivity")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        

        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::show("panel_Mod_dims")

        shinyjs::show("panel_SSS_reps")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")
        
        shinyjs::show("run_SSS")
        shinyjs::hide("run_SS")        

        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")
  })

#SS-LO panels
observeEvent(req(((as.numeric(input$tabs)*2)/2)<4&all(!is.null(c(rv.Lt$data,rv.Age$data)),is.null(rv.Ct$data))), {
        shinyjs::show("Data_panel")
        shinyjs::show("panel_data_wt_lt")
        if(length(unique(rv.Lt$data[,2]))>1){shinyjs::show("panel_ct_wt_LO")}
        if(length(unique(rv.Lt$data[,2]))==1){shinyjs::hide("panel_ct_wt_LO")}
        shinyjs::hide("panel_SSS")
        shinyjs::show("panel_SSLO_LH")
        shinyjs::show("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::show("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::show("panel_selectivity")

        shinyjs::show("panel_SS_recdevs")

        shinyjs::show("panel_SS_jitter")        

        shinyjs::show("panel_RPs")
        shinyjs::show("panel_Forecasts")

        shinyjs::show("panel_Mod_dims")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::show("OS_choice")
        shinyjs::show("Scenario_panel")
   
        shinyjs::hide("run_SSS")
        shinyjs::show("run_SS")

        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")
  })	


observeEvent(req(((as.numeric(input$tabs)*3)/3)<4&all(any(input$est_parms==FALSE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data))))), {
      shinyjs::show("Data_panel")
      shinyjs::show("panel_data_wt_lt")
      shinyjs::hide("panel_ct_wt_LO")
       
      shinyjs::hide("panel_SSS")
      shinyjs::hide("panel_SSLO_LH")
      shinyjs::hide("panel_SSLO_fixed")
      shinyjs::show("panel_SS_LH_fixed_est_tog")
      shinyjs::show("panel_SS_LH_fixed")
      shinyjs::show("panel_SS_fixed")
      shinyjs::hide("panel_SS_LH_est")
      shinyjs::hide("panel_SS_est")

      shinyjs::hide("panel_SS_stock_status") 

      shinyjs::hide("panel_SSS_prod")
      shinyjs::hide("panel_SS_LO_prod")
      shinyjs::show("panel_SS_prod_fixed")
      shinyjs::hide("panel_SS_prod_est")

      shinyjs::show("panel_selectivity")

      shinyjs::show("panel_SS_recdevs")

      shinyjs::show("panel_SS_jitter")        
 
      shinyjs::show("panel_RPs")
      shinyjs::show("panel_Forecasts")

      shinyjs::show("panel_Mod_dims")

      shinyjs::show("OS_choice")
      shinyjs::show("Scenario_panel")

      shinyjs::hide("panel_SSS_reps")

      shinyjs::hide("run_SSS")
      shinyjs::show("run_SS")

      shinyjs::hide("Sensi_Comparison_panel")
      shinyjs::hide("Ensemble_panel")
   })


observeEvent(req(((as.numeric(input$tabs)*4)/4)<4&all(input$est_parms==TRUE,any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data))))), {
      shinyjs::show("Data_panel")
      shinyjs::show("panel_data_wt_lt")
      shinyjs::hide("panel_ct_wt_LO")
      
      shinyjs::hide("panel_SSS")
      shinyjs::hide("panel_SSLO_LH")
      shinyjs::hide("panel_SSLO_fixed")
      shinyjs::show("panel_SS_LH_fixed_est_tog")
      shinyjs::hide("panel_SS_LH_fixed")
      shinyjs::hide("panel_SS_fixed")
      shinyjs::show("panel_SS_LH_est")
      shinyjs::show("panel_SS_est")

      shinyjs::hide("panel_SS_stock_status") 

      shinyjs::hide("panel_SSS_prod")
      shinyjs::hide("panel_SS_LO_prod")
      shinyjs::hide("panel_SS_prod_fixed")
      shinyjs::show("panel_SS_prod_est")

      shinyjs::show("panel_selectivity")

      shinyjs::show("panel_SS_recdevs")

      shinyjs::show("panel_SS_jitter")        
 
      shinyjs::show("panel_RPs")
      shinyjs::show("panel_Forecasts")

      shinyjs::show("panel_Mod_dims")
      
      shinyjs::show("OS_choice")
      shinyjs::show("Scenario_panel")

      shinyjs::hide("panel_SSS_reps")

      shinyjs::hide("run_SSS")
      shinyjs::show("run_SS")

      shinyjs::hide("Sensi_Comparison_panel")
      shinyjs::hide("Ensemble_panel")
   })

observeEvent(req((as.numeric(input$tabs)*5/5)==5), {
        shinyjs::hide("Data_panel")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::show("Sensi_Comparison_panel")
        shinyjs::hide("Ensemble_panel")
   })

observeEvent(req((as.numeric(input$tabs)*6/6)==6), {
        shinyjs::hide("Data_panel")
        shinyjs::hide("panel_data_wt_lt")
        shinyjs::hide("panel_ct_wt_LO")
        
        shinyjs::hide("panel_SSS")
        shinyjs::hide("panel_SSLO_LH")
        shinyjs::hide("panel_SSLO_fixed")
        shinyjs::hide("panel_SS_LH_fixed_est_tog")
        shinyjs::hide("panel_SS_LH_fixed")
        shinyjs::hide("panel_SS_fixed")
        shinyjs::hide("panel_SS_LH_est")
        shinyjs::hide("panel_SS_est")

        shinyjs::hide("panel_SS_stock_status") 

        shinyjs::hide("panel_SSS_prod")
        shinyjs::hide("panel_SS_LO_prod")
        shinyjs::hide("panel_SS_prod_fixed")
        shinyjs::hide("panel_SS_prod_est")

        shinyjs::hide("panel_selectivity")

        shinyjs::hide("panel_SS_recdevs")

        shinyjs::hide("panel_SS_jitter")        
   
        shinyjs::hide("panel_RPs")
        shinyjs::hide("panel_Forecasts")

        shinyjs::hide("panel_Mod_dims")
        
        shinyjs::hide("panel_SSS_reps")

        shinyjs::hide("OS_choice")
        shinyjs::hide("Scenario_panel")

        shinyjs::hide("run_SSS")
        shinyjs::hide("run_SS")

        shinyjs::hide("Sensi_Comparison_panel")
        shinyjs::show("Ensemble_panel")
   })

########################################

	
# User activated pop-up parameter values ---------------
#Model dimensions
output$Model_dims1 <- renderUI({ 
        inFile1 = rv.Lt$data 
        inFile2 = rv.Ct$data 
        if (is.null(inFile1) & is.null(inFile2)) return(NULL) 
        if (!is.null(inFile1) & is.null(inFile2)){ 
              styr.in =  min(inFile1[,1]) 
              endyr.in = max(inFile1[,1])
              if(!(anyNA(c(Linf(), k_vbgf(),t0_vbgf())))){ 
                styr.in = min(inFile1[,1])-round(VBGF.age(Linf(), k_vbgf(), t0_vbgf(), Linf()*0.95)) 
              }
               
            fluidRow(column(width=4, numericInput("styr", "Starting year",  
                                                  value=styr.in, min=1, max=10000, step=1)), 
                    column(width=4, numericInput("endyr","Ending year",  
                                                 value=endyr.in, min=1, max=10000, step=1)))            
            } 

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
    fluidRow(column(width=6, numericInput("CV_lt_m", "CV at length",  
                                          value=0.1, min=0, max=10000, step=0.01))) 
    	} 
	})  

output$Male_parms_inputs4 <- renderUI({ 
	if(input$male_parms){ 
      fluidRow(column(width=6, numericInput("WLa_m", "Weight-Length alpha",  
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
    fluidRow(column(width=6, numericInput("CV_lt_m_fix", "CV at length",  
                                          value=0.1, min=0, max=10000, step=0.01))) 
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
        numericInput("t0_m_mean", "Mean", value=NA,min=0, max=10000, step=0.001),
        numericInput("t0_m_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("t0_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("baby-carriage"), width = "300px",label="t0: Age at size 0"
          )
  } 
}) 

output$Male_parms_inputs_CV_est <- renderUI({ 
  if(input$male_parms_est){ 
      dropdownButton(
        selectInput("CV_lt_m_prior","Prior type",c("no prior","symmetric beta", "beta","lognormal","gamma","normal")),
        numericInput("CV_lt_m_mean", "Mean", value=0.1,min=0, max=10000, step=0.001),
        numericInput("CV_lt_m_SD", "SD", value=0,min=0, max=10000, step=0.001),
        numericInput("CV_lt_m_phase", "Phase", value=-1,min=-999, max=10, step=0.001),
        circle = FALSE, right=TRUE, status = "danger", icon = icon("dice"), width = "300px",label="CV at length"
          )
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

output$Male_parms_inputs1_SSS<- renderUI({
  if(input$male_parms_SSS){
       dropdownButton(
          selectInput("M_m_prior_sss","Prior type",c("no prior","lognormal","normal","uniform")),
          numericInput("M_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("M_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("skull-crossbones"), width = "300px",label="Natural mortality"
            )
    }
}) 

output$Male_parms_inputs_space1_SSS <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 
output$Male_parms_inputs_space2_SSS <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 
output$Male_parms_inputs_space3_SSS <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 
output$Male_parms_inputs_space4_SSS <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 

output$Male_parms_inputs_space5_SSS <- renderUI({
if(input$male_parms_est){ 
  br()
  } 
}) 

output$Male_parms_inputs_Growth_label_SSS <- renderUI({
  if(input$male_parms_est){ 
    h5(strong("Growth")) 
    }
}) 

output$Male_parms_inputs_Linf_SSS <- renderUI({ 
  if(input$male_parms_est){ 
        dropdownButton(
          selectInput("Linf_m_prior_sss","Prior type",c("no prior","normal")),
          numericInput("Linf_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("Linf_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("infinity"), width = "300px",label="Linf: Asymptotic size"
          )
    } 
}) 

output$Male_parms_inputs_k_SSS <- renderUI({ 
  if(input$male_parms_est){ 
    dropdownButton(
          selectInput("k_m_prior_sss","Prior type",c("no prior","normal")),
          numericInput("k_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("k_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("ruler-horizontal"), width = "300px",label="k: VB growth coefficient"
            )
    } 
}) 

output$Male_parms_inputs_t0_SSS <- renderUI({ 
  if(input$male_parms_est){ 
    dropdownButton(
          selectInput("t0_m_prior_sss","Prior type",c("no prior","normal")),
          numericInput("t0_m_mean_sss", "Mean", value=NA,min=0, max=10000, step=0.001),
          numericInput("t0_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("baby-carriage"), width = "300px",label="t0: Age at size 0"
            )
    } 
}) 

output$Male_parms_inputs_CV_SSS <- renderUI({ 
  if(input$male_parms_est){ 
    dropdownButton(
          selectInput("CV_lt_m_prior_sss","Prior type",c("no prior")),
          numericInput("CV_lt_m_mean_sss", "Mean", value=0.1,min=0, max=10000, step=0.001),
          numericInput("CV_lt_m_SD_sss", "SD", value=0,min=0, max=10000, step=0.001),
          circle = FALSE, right=TRUE, status = "danger", icon = icon("dice"), width = "300px",label="CV at length"
            )
    } 

	})


output$Male_parms_inputs4_SSS<- renderUI({
	if(input$male_parms_SSS){
      fluidRow(column(width=6,numericInput("WLa_m_sss", "Weight-Length alpha", 
      										value=0.00001,min=0, max=10000, step=0.000000001)),
              column(width=6,numericInput("WLb_m_sss","Weight-length beta", 
              								value=3,min=0, max=10000, step=0.01)))    
    	}
	})

#Selectivity paramters
output$Sel_parms1 <- renderUI({ 
    fluidRow(column(width=8, textInput("Sel50", "Length at 50% Selectivity",value="")), 
            column(width=4, textInput("Sel50_phase", "Est. phase", value="")))     
	}) 
 
output$Sel_parms2<- renderUI({ 
    	fluidRow(column(width=8, textInput("Selpeak", "Length at Peak Selectvity", value="")), 
            	 column(width=4, textInput("Selpeak_phase", "Est. phase", value=""))) 
	}) 
 
output$Sel_parms3 <- renderUI({ 
  		if(input$Sel_choice=="Dome-shaped"){ 			 
    	fluidRow(column(width=8, textInput("PeakDesc", "Length at 1st declining selectivity",value="1000")), 
            	 column(width=4, textInput("PeakDesc_phase", "Est. phase",value="",))) 
 		} 
	}) 
 
output$Sel_parms4 <- renderUI({ 
 		if(input$Sel_choice=="Dome-shaped"){ 			 
	    fluidRow(column(width=8, textInput("LtPeakFinal", "Width of declining selectivity",value="0.0001")), 
	             column(width=4, textInput("LtPeakFinal_phase", "Est. phase",value="")))    			 
 		} 
	}) 
 
output$Sel_parms5 <- renderUI({ 
 		if(input$Sel_choice=="Dome-shaped"){ 			 
    	fluidRow(column(width=8, textInput("FinalSel", "Selectivity at max bin size",value="0.999")), 
            	column(width=4, textInput("FinalSel_phase", "Est. phase",value=""))) 
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

#Jitter value
output$Jitter_value <- renderUI({ 
    if(input$jitter_choice){ 
        fluidRow(column(width=6, numericInput("jitter_fraction", "Jitter value",  
                                             value=0.1, min=0, max=10, step=0.001)), 
        	       column(width=6, numericInput("Njitter", "# of jitters",  
        	                                   value=1, min=1, max=10000, step=1)))    
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
        fluidRow(column(width=4, numericInput("CR_Ct_F", "Control rule type",  
                                              value=1, min=0, max=1, step=0.001)), 
        	       column(width=4, numericInput("slope_hi", "Upper ratio value",  
        	                                    value=0.4, min=0, max=1, step=0.001)),    
                 column(width=4, numericInput("slope_low", "Lower ratio value",  
                                              value=0.1, min=0, max=1, step=0.001)))
    	} 
	}) 
 
output$Forecasts<- renderUI({ 
    if(input$Forecast_choice){ 
        fluidRow(column(width=6, numericInput("forecast_num", "# of forecast years",  
                                              value=2, min=1, max=1000, step=1)), 
        	       column(width=6, numericInput("forecast_buffer", "Control rule buffer",  
        	                                    value=0.913, min=0, max=1, step=0.001)))    
    	} 
	}) 




Nages<-reactive({
    Nages<-NA
    if(all(c(is.null(input$M_f),is.null(input$M_f_fix),is.null(input$M_f_mean),is.null(input$M_f_mean_sss)))) return(NULL)
    if(!is.na(input$M_f)) {Nages<-ceiling(5.4/input$M_f)}
    if(!is.na(input$M_f_fix)) {Nages<-ceiling(5.4/input$M_f_fix)}
    if(!is.na(input$M_f_mean)) {Nages<-ceiling(5.4/input$M_f_mean)}
    if(!is.na(input$M_f_mean_sss)) {Nages<-ceiling(5.4/input$M_f_mean_sss)}
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
    if(all(c(is.null(input$Linf_m),is.null(input$Linf_m_fix),is.null(input$Linf_m_mean),is.null(input$Linf_m_mean_sss)))) return(NULL)
    if(any(input$male_parms&!is.na(input$Linf_m))) {Linf_m_in<-input$Linf_m}
    if(any(input$male_parms&!is.na(input$Linf_m_fix))) {Linf_m_in<-input$Linf_m_fix}
    if(any(input$male_parms&!is.na(input$Linf_m_mean))) {Linf_m_in<-input$Linf_m_mean}
    if(any(input$male_parms&!is.na(input$Linf_m_mean_sss))) {Linf_m_in<-input$Linf_m_mean_sss}
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
    if(any(input$male_parms&!is.na(input$k_m_fix))) {k_vbgf_m_in<-input$k_m_fix}
    if(any(input$male_parms&!is.na(input$k_m_mean))) {k_vbgf_m_in<-input$k_m_mean}
    if(any(input$male_parms&!is.na(input$k_m_mean_sss))) {k_vbgf_m_in<-input$k_m_mean_sss}
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
    if(any(input$male_parms&!is.na(input$t0_m_fix))) {t0_vbgf_m_in<-input$t0_m_fix}
    if(any(input$male_parms&!is.na(input$t0_m_mean))) {t0_vbgf_m_in<-input$t0_m_mean}
    if(any(input$male_parms&!is.na(input$t0_m_mean_sss))) {t0_vbgf_m_in<-input$t0_m_mean_sss}
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

#############
### PLOTS ###
#############

observeEvent(req(!is.null(rv.Lt$data)), {
    	shinyjs::show(output$lt_comp_plots_label<-renderText({"Length compositions"}))
  })


#Plot length compoistions
# length compositions 
observeEvent(req(!is.null(rv.Lt$data)), {
	output$Ltplot<-renderPlot({ 
		  inFile <- rv.Lt$data 
		  if (is.null(inFile)) return(NULL) 
		  rv.Lt$data %>%  
		    rename_all(tolower) %>%  
		    dplyr::select(-nsamps) %>%  
		    pivot_longer(c(-year, -fleet, -sex)) %>%  
		    mutate(Fleet = factor(fleet), 
		           name = as.numeric(gsub("[^0-9.-]", "", name))) %>%  
		    ggplot(aes(name, value, fill=Fleet)) + 
		    geom_col(position="dodge") + 
		    facet_wrap(~year, scales="free_y") + 
		    xlab("Length bin") + 
		    ylab("Frequency") + 
		    scale_fill_viridis_d() 
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

observeEvent(req(!is.null(rv.Age$data)), {
    	shinyjs::show(output$age_comp_plots_label<-renderText({"Age compositions"}))
  })

output$Ageplot <- renderPlot({ 
		inFile_age <- rv.Age$data 
		if (is.null(inFile_age)) return(NULL) 
		 
		  rv.Age$data %>%  
		      pivot_longer(-1, names_to = "year", values_to = "ltnum") %>%  
		      rename(bin = Bins) %>%  
		  ggplot(aes(bin, ltnum)) + 
					geom_col(fill="#1D252D", color="white") + 
					facet_wrap(~year) + 
					xlab("Age bin") + 
					ylab("Frequency")			 
	}) 
 

 observeEvent(req(!is.null(rv.Ct$data)), {
    	shinyjs::show(output$catch_comp_plots_label<-renderText({"Removal history"}))
  })

output$Ctplot <- renderPlot({ 
		#inCatch <- input$file2 
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

#Plot M by age
output$Mplot<-renderPlot({ 
			mf.in = M_f_in() 
			mm.in = M_f_in() 
#      if(input$male_parms|input$male_parms_fix)
			if(input$male_parms|input$male_parms_SSS|input$male_parms_fix|input$male_parms_est)
        { 
			     mm.in = M_m_in() 
			  }		 
      if(any(is.na(c(mf.in, mm.in)))|any(is.null(c(mf.in, mm.in)))) return(NULL) 
      Female_M = data.frame(Ages = 0:Nages(), PopN = exp(-mf.in * 0:Nages()), Sex="Female") 
			Male_M = data.frame(Ages = 0:Nages(), PopN=exp(-mm.in * 0:Nages()), Sex="Male") 
			M_sexes <- rbind(Female_M, Male_M) 
			ggplot(M_sexes,aes(Ages, PopN, color=Sex))+ 
					geom_line(aes(linetype=Sex), lwd=2)+ 
					ylab("Cohort decline by M") 
		}) 

#Plot VBGF and maturity
output$VBGFplot<-renderPlot({ 
   	f_Linf = m_Linf = Linf() 
   	f_k = m_k = k_vbgf() 
   	f_t0 = m_t0 = t0_vbgf() 
	  f_L50 = L50() 
	  f_L95 = L95() 
	  maxage = Nages() 
	if(any(input$male_parms,input$male_parms_SSS,input$male_parms_fix,input$male_parms_est))
      { 
				  m_Linf = Linf_m_in() 
			   	m_k = k_vbgf_m_in() 
			   	m_t0 = t0_vbgf_m_in() 
			}		 
   if(any(is.na(c(f_Linf, f_k, f_t0)))=="FALSE"){ 
		vbgf_female = data.frame(Age = 0:Nages(),  
		                         Length = VBGF(f_Linf, f_k, f_t0, 0:Nages()), Sex="Female") 
    vbgf_male = data.frame(Age = 0:Nages(),  
                           Length=VBGF(m_Linf, m_k, f_t0, 0:Nages()), Sex="Male") 
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

#############################################
######## PREPARE FILES andD RUN SSS #########
#############################################
SSS.run<-observeEvent(input$run_SSS,{
		progress <- shiny::Progress$new(session, min=1, max=2)
           on.exit(progress$close())
       
           progress$set(message = 'Model run in progress',
                        detail = '')
       
           for (i in 1:2) {
             progress$set(value = i)
             Sys.sleep(0.5)
           }

  	#Copy and move files
	  	if(file.exists(paste0(getwd(),"/Scenarios/",input$Scenario_name)))
			{
				unlink(paste0(getwd(),"/Scenarios/",input$Scenario_name),recursive=TRUE)
#				file.remove(paste0(getwd(),"/Scenarios/",input$Scenario_name))
			}
	  	#if(input$)
	  		{
	  			file.copy(paste0(getwd(),"/SSS_files/sssexample_BH"),paste0(getwd(),"/Scenarios"),recursive=TRUE,overwrite=TRUE)
				file.rename(paste0(getwd(),"/Scenarios/sssexample_BH"), paste0(getwd(),"/Scenarios/",input$Scenario_name))
			}
	
	  	#if()
#	  		{
#	  			file.copy(paste0(getwd(),"/SSS_files/sssexample_RickPow"),paste0(getwd(),"/Scenarios"),recursive=TRUE,overwrite=TRUE)
#				file.rename(paste0(getwd(),"/Scenarios/sssexample_RickPow"), paste0(getwd(),"/Scenarios/",input$Scenario_name))
#			}
		
		#Read data and control files
		data.file<-SS_readdat(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/sss_example.dat")) 
		ctl.file<-SS_readctl(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/sss_example.ctl"),use_datlist = TRUE, datlist=data.file) 
		#Read, edit then write new DATA file
		data.file$styr<-input$styr
		data.file$endyr<-input$endyr
		data.file$Nages<-Nages()

	#Catches
    #inCatch<- input$file2
		Catch.data<-rv.Ct$data
		data.file$Nfleets<-ncol(Catch.data)
		if((data.file$Nfleets-1)>1){
			for(i in 1:(data.file$Nfleets-2))
			{
				data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
				data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
			}
			data.file$fleetinfo$fleetname<-c(paste0("Fishery",1:(data.file$Nfleets-1)),"Depl")
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
		
	#Age composition data
		# if (is.null(inFile_age)){
		# data.file$N_agebins<-Nages()
		# data.file$agebin_vector<-1:Nages()		
		# data.file$ageerror<-data.frame(matrix(c(rep(-1,(Nages()+1)),rep(0.001,(Nages()+1))),2,(Nages()+1),byrow=TRUE))
		# colnames(data.file$ageerror)<-paste0("age",1:Nages())		
		# 	}
		
		SS_writedat(data.file,paste0(getwd(),"/Scenarios/",input$Scenario_name,"/sss_example.dat"),overwrite=TRUE)			
		####################### END DATA FILE #####################################

    ####################### START SSS CTL FILE #####################################
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
    if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[2,3:4]<-c(fem_vbgf[1],log(fem_vbgf[1]))}
    else {ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]}
    
    #Linf
    if(input$Linf_f_prior=="lognormal"){ctl.file$MG_parms[3,3:4]<-c(input$Linf_f_mean_sss,log(input$Linf_f_mean_sss))}     
    else{ctl.file$MG_parms[3,3:4]<-input$Linf_f_mean_sss}
    
    #k
    if(input$k_f_prior=="lognormal"){ctl.file$MG_parms[4,3:4]<-c(input$k_f_mean_sss,log(input$k_f_mean_sss))}        
    else {ctl.file$MG_parms[4,3:4]<-input$k_f_mean_sss}
    
    #CV young
    if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[5,3:4]<-c(input$CV_lt_f_mean,log(input$CV_lt_f_mean))}     
    else{ctl.file$MG_parms[5,3:4]<-input$CV_lt_f_mean}
    
    #CV old
    if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[6,3:4]<-c(input$CV_lt_f_mean,log(input$CV_lt_f_mean))}
    else{ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_mean}
    
    #Maturity
    ctl.file$MG_parms[9,3:4]<-input$L50_f_sss                                     #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f_sss-input$L50_f_sss)  #Maturity slope
    
    #Males
    ctl.file$MG_parms[13,3:4]<-c(input$M_f_mean_sss,log(input$M_f_mean_sss))    #M
    ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]                                      #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f_mean_sss                            #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f_mean_sss                               #k
    ctl.file$MG_parms[17,3:4]<-input$CV_lt_f_mean_sss                           #CV
    ctl.file$MG_parms[18,3:4]<-input$CV_lt_f_mean_sss                           #CV
    
    if(input$male_parms_SSS)
      {   
        male_vbgf_sss<-VBGF(input$Linf_m_mean_sss,input$k_m_mean_sss,input$t0_m_mean_sss,c(0:Nages()))

              # ctl.file$MG_parms[13,3]<-input$M_m_mean        #M
        # ctl.file$MG_parms[14,3:4]<-male_vbgf_est[1]    #L0
        # ctl.file$MG_parms[15,3:4]<-input$Linf_m_mean   #Linf
        # ctl.file$MG_parms[16,3:4]<-input$k_m_mean      #k
        # ctl.file$MG_parms[17,3:4]<-input$CV_lt_m_mean  #CV
        # ctl.file$MG_parms[18,3:4]<-input$CV_lt_m_mean  #CV

        #M
        if(input$M_m_prior=="lognormal"){ctl.file$MG_parms[13,2:4]<-c(input$M_m_mean,log(input$M_m_mean))}
        else {ctl.file$MG_parms[13,2:4]<-c(input$M_f_mean,input$M_f_mean)}
            
        #L0    
        if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[14,2:4]<-c(male_vbgf_sss[1],log(male_vbgf_sss[1]))}
        else {ctl.file$MG_parms[14,2:4]<-male_vbgf_sss[1]}
        
        #Linf
        if(input$Linf_f_prior=="lognormal"){ctl.file$MG_parms[15,2:4]<-c(input$Linf_m_mean,log(input$Linf_m_mean))}     
        else{ctl.file$MG_parms[15,2:4]<-input$Linf_m_mean}
        
        #k
        if(input$k_f_prior=="lognormal"){ctl.file$MG_parms[16,2:4]<-c(input$k_m_mean,log(input$k_m_mean))}        
        else {ctl.file$MG_parms[16,2:4]<-input$k_m_mean}
        
        #CV young
        if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[17,2:4]<-c(input$CV_lt_m_mean,log(input$CV_lt_m_mean))}     
        else{ctl.file$MG_parms[17,2:4]<-input$CV_lt_m_mean}
        
        #CV old
        if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[18,2:4]<-c(input$CV_lt_m_mean,log(input$CV_lt_m_mean))}
        else{ctl.file$MG_parms[18,2:4]<-input$CV_lt_m_mean}
      }     

    #S-R
    ctl.file$SR_parms[1,3:4]<-input$lnR0  #lnR0
    
    if(input$h_ss_prior=="lognormal"){ctl.file$SR_parms[2,3:4]<-c(input$h_mean_ss,log(h_mean_ss))}
    else{ctl.file$SR_parms[2,3:4]<-input$h_mean_ss}        
    #}
    
    #
      ctl.file$Q_options[1]<-data.file$Nfleets
    
    #Selectivity
      Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50,","))))
      Sel50_phase<-as.numeric(trimws(unlist(strsplit(input$Sel50_phase,","))))
      Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))
      Selpeak_phase<-as.numeric(trimws(unlist(strsplit(input$Selpeak_phase,","))))
      bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]

    if(input$Sel_choice=="Logistic")
    {
      ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
      ctl.file$size_selex_parms[2,3:4]<- 15
      ctl.file$size_selex_parms[3,3:4]<- log(-((Sel50[1]-Selpeak[1])^2/log(0.5)))
      ctl.file$size_selex_parms[4,3:4]<- 15
      ctl.file$size_selex_parms[6,3:4]<- 15
      }
    if(input$Sel_choice=="Dome-shaped")
    {     
      PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))
      PeakDesc_phase<-as.numeric(trimws(unlist(strsplit(input$PeakDesc_phase,","))))
      LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))
      LtPeakFinal_phase<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_phase,","))))
      FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))
      FinalSel_phase<-as.numeric(trimws(unlist(strsplit(input$FinalSel_phase,","))))
      
      ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
      ctl.file$size_selex_parms[2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[1]-bin.width)/(PeakDesc[1]-Selpeak[1]-bin.width))
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
        
        if(input$Sel_choice=="Logistic")
        {
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
          ctl.file$size_selex_parms[6*i+2,3:4]<- 15
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+4,3:4]<- -15
          ctl.file$size_selex_parms[6*i+6,3:4]<- 15
        }

        if(input$Sel_choice=="Dome-shaped")
        {
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
          ctl.file$size_selex_parms[6*i+2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[i+1]-bin.width)/(PeakDesc[i+1]-Selpeak[i+1]-bin.width))
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


    SS_writectl(ctl.file,paste0(getwd(),"/Scenarios/",input$Scenario_name,"/sss_example.ctl"),overwrite=TRUE)

#Forecast file modfications
#Reference points
forecast.file<-SS_readforecast(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/forecast.ss"))

if(input$RP_choices){
    forecast.file$SPRtarget<-input$SPR_target
    forecast.file$Btarget<-input$B_target

    forecast.file$ControlRuleMethod<-input$CR_Ct_F
    forecast.file$SBforconstantF<-input$slope_hi
    forecast.file$BfornoF<-input$slope_low  
  }

if(input$Forecast_choice)
  {
    forecast.file$Nforecastyrs<-input$forecast_num
    forecast.file$Flimitfraction<-input$forecast_buffer
  }

#Set prior inputs
SS_writeforecast(forecast.file,paste0(getwd(),"/Scenarios/",input$Scenario_name),overwrite=TRUE)  

    sss.prior.name<-c("no prior","beta","symmetric beta","normal","truncated normal","lognormal","truncated lognormal","uniform")
    sss.prior.type<-c(-1,1,2,0,10,3,30,4)
    Dep.in_sss<-c(sss.prior.type[sss.prior.name==input$Depl_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss)
    M.in_sss<-c(sss.prior.type[sss.prior.name==input$M_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss,sss.prior.type[sss.prior.name==input$M_prior_sss],input$M_f_mean_sss,input$M_f_SD_sss)
    h.in_sss<-c(sss.prior.type[sss.prior.name==input$h_prior_sss],input$h_f_mean_sss,input$M_f_SD_sss)
    L1.in<-c(ctl.file$MG_parms[2,3],input$M_f_SD_sss,ctl.file$MG_parms[14,3],input$M_f_SD_sss)
    Linf.in<-c(input$Linf_f_mean_sss,input$Linf_f_SD_sss,input$Linf_f_mean_sss,input$Linf_f_SD_sss)
    k.in<-c(input$k_f_mean_sss,input$k_f_SD_sss,input$k_f_mean_sss,input$k_f_SD_sss)
        
#Run SSS
  SSS.out<-SSS(paste0(getwd(),"/Scenarios/",input$Scenario_name),
      file.name=c("sss_example.dat","sss_example.ctl"),
      reps=input$SSS_reps,
      seed.in=19,
      Dep.in=Dep.in_sss,
      M.in=M.in_sss,
      SR_type=3,
      h.in=h.in_sss,
      FMSY_M.in=c(-1,0.5,0.1),
      BMSY_B0.in=c(-1,0.5,0.1),
      L1.in=L1.in,
      Linf.in=Linf.in,
      k.in=k.in,
      Zfrac.Beta.in=c(-99,0.2,0.6,-99,0.5,2),
      R_start=c(0,8),
      doR0.loop=c(1,4.1,12.1,0.5),
      sum_age=0,
      ts_yrs=c(input$styr,input$endyr),
      pop.ltbins=NA,
      ofl_yrs=c(input$endyr+1,input$endyr+2),
      sexes=F,
      BH_FMSY_comp=F,
      OStype="Windows")
#save(SSS.out)
})


#############################################
### PREPARE FILES andD RUN Length and Age-based Stock Synthsis ###
#############################################
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
		progress <- shiny::Progress$new(session, min=1, max=2)
           on.exit(progress$close())
       
           progress$set(message = 'Model run in progress',
                        detail = '')
       
           for (i in 1:2) {
             progress$set(value = i)
             Sys.sleep(0.5)
           }
  	#Copy and move files
	  	if(file.exists(paste0(getwd(),"/Scenarios/",input$Scenario_name)))
			{
				unlink(paste0(getwd(),"/Scenarios/",input$Scenario_name),recursive=TRUE)
#				file.remove(paste0(getwd(),"/Scenarios/",input$Scenario_name))
			}
	  	file.copy(paste0(getwd(),"/SS_LB_files"),paste0(getwd(),"/Scenarios"),recursive=TRUE,overwrite=TRUE)
		file.rename(paste0(getwd(),"/Scenarios/SS_LB_files"), paste0(getwd(),"/Scenarios/",input$Scenario_name))

		#Read data and control files
		data.file<-SS_readdat(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.dat")) 
		ctl.file<-SS_readctl(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.ctl"),use_datlist = TRUE, datlist=data.file) 
		
		#Read, edit then write new DATA file
		data.file$styr<-input$styr
		data.file$endyr<-input$endyr
		data.file$Nages<-Nages()

	#Catches
		#inCatch<- input$file2
		if (is.null(rv.Ct$data)) 
		{
		inFile<- rv.Lt$data
		Lt.comp.data<-rv.Lt$data
		data.file$Nfleets<-max(Lt.comp.data[,2])
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
				data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
				data.file$len_info<-rbind(data.file$len_info,data.file$len_info[1,])
				data.file$age_info<-rbind(data.file$age_info,data.file$age_info[1,])
			}
      data.file$len_info[,5]<-1
      data.file$len_info[,6]<-c(1:data.file$Nfleets)
			data.file$fleetinfo$fleetname<-paste0("Fishery",1:data.file$Nfleets)
			data.file$CPUEinfo[,1]<-1:data.file$Nfleets
		}
		year.in<-input$styr:input$endyr
		catch.cols<-colnames(data.file$catch)
		catch_temp<-list()
		if(data.file$Nfleets==1){catch.level<-1000}
    if(data.file$Nfleets>1){
        catch.level<-as.numeric(trimws(unlist(strsplit(input$Wt_fleet_Ct,","))))
        catch.level<-catch.level/sum(catch.level)*1000
      }
    for(i in 1:data.file$Nfleets)
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
		data.file$Nfleets<-ncol(Catch.data)-1
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
				data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
				data.file$len_info<-rbind(data.file$len_info,data.file$len_info[1,])
				data.file$age_info<-rbind(data.file$age_info,data.file$age_info[1,])
			}
      data.file$len_info[,5]<-1
      data.file$len_info[,6]<-c(1:data.file$Nfleets)
			data.file$fleetinfo$fleetname<-paste0("Fishery",1:data.file$Nfleets)
			data.file$CPUEinfo[,1]<-1:data.file$Nfleets
		}
		year.in<-Catch.data[,1]
		catch.cols<-colnames(data.file$catch)
		catch_temp<-list()
		for(i in 1:data.file$Nfleets)
		{
			catch_temp[[i]]<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(i,length(year.in)+1),
						c(0.0001,Catch.data[,i+1]),
						rep(0.01,length(year.in)+1)
						)
		}
		data.file$catch<-list.rbind(catch_temp)
		colnames(data.file$catch)<-catch.cols
		}

	#Length composition data
		inFile<- rv.Lt$data
		if (is.null(inFile)) return(NULL)
		Lt.comp.data<-rv.Lt$data
		data.file$N_lbins<-ncol(Lt.comp.data)-4
		data.file$lbin_vector<-as.numeric(colnames(Lt.comp.data[,5:ncol(Lt.comp.data)]))
		lt.data.names<-c(colnames(data.file$lencomp[,1:6]),paste0("f",data.file$lbin_vector),paste0("m",data.file$lbin_vector))
		lt.data.females<-lt.data.males<-lt.data.unknowns<-data.frame(matrix(rep(NA,length(lt.data.names)),nrow=1))
		colnames(Lt.comp.data)[1:4]<-c("Year","Fleet","Sex","Nsamps")
		#female lengths
		if(nrow(subset(Lt.comp.data,Sex==1))>0){
		Lt.comp.data_female<-subset(Lt.comp.data,Sex==1 & Nsamps>0)	
		samp.yrs<-Lt.comp.data_female[,1]
		lt.data.females<-data.frame(cbind(samp.yrs,
				rep(1,length(samp.yrs)),
				Lt.comp.data_female[,2],
				Lt.comp.data_female[,3],
				rep(0,length(samp.yrs)),
				Lt.comp.data_female[,4],
				Lt.comp.data_female[,5:ncol(Lt.comp.data_female)],
				Lt.comp.data_female[,5:ncol(Lt.comp.data_female)]*0)
				)
		}
		#male lengths
		if(nrow(subset(Lt.comp.data,Sex==2))>0){
			Lt.comp.data_male<-subset(Lt.comp.data,Sex==2 & Nsamps>0)
			samp.yrs_males<-Lt.comp.data_male[,1]
			lt.data.males<-data.frame(cbind(samp.yrs_males,
				rep(1,length(samp.yrs_males)),
				Lt.comp.data_male[,2],
				Lt.comp.data_male[,3],
				rep(0,length(samp.yrs_males)),
				Lt.comp.data_male[,4],
				Lt.comp.data_male[,5:ncol(Lt.comp.data_male)]*0,
				Lt.comp.data_male[,5:ncol(Lt.comp.data_male)])
				)
			}
		#unknown sex lengths
		if(nrow(subset(Lt.comp.data,Sex==0))>0){
			Lt.comp.data_unknown<-subset(Lt.comp.data,Sex==0 & Nsamps>0)
			samp.yrs_unknown<-Lt.comp.data_unknown[,1]
			lt.data.unknowns<-data.frame(cbind(samp.yrs_unknown,
				rep(1,length(samp.yrs_unknown)),
				Lt.comp.data_unknown[,2],
				Lt.comp.data_unknown[,3],
				rep(0,length(samp.yrs_unknown)),
				Lt.comp.data_unknown[,4],
				Lt.comp.data_unknown[,5:ncol(Lt.comp.data_unknown)],
				Lt.comp.data_unknown[,5:ncol(Lt.comp.data_unknown)]*0)
				)
			}
		colnames(lt.data.females)<-colnames(lt.data.males)<-colnames(lt.data.unknowns)<-lt.data.names
		data.file$lencomp<-na.omit(rbind(lt.data.females,lt.data.males,lt.data.unknowns))
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
	
	#Age composition data
		inFile_age<- rv.Age$data
		if (is.null(inFile_age)){
		data.file$N_agebins<-Nages()
		data.file$agebin_vector<-1:Nages()		
		data.file$ageerror<-data.frame(matrix(c(rep(-1,(Nages()+1)),rep(0.001,(Nages()+1))),2,(Nages()+1),byrow=TRUE))
		colnames(data.file$ageerror)<-paste0("age",1:Nages())		
			}
		if (!is.null(inFile_age)){
		Age.comp.data<-rv.Age$data
		age.classes<-nrow(Age.comp.data)
		data.file$N_agebins<-age.classes
		data.file$agebin_vector<-Age.comp.data[,1]
		data.file$ageerror<-data.frame(matrix(c(rep(-1,(age.classes+1)),rep(0.001,(age.classes+1))),2,(age.classes+1),byrow=TRUE))		
		colnames(data.file$ageerror)<-paste0("age",1:Nages())		
		age.samp.yrs<-as.numeric(colnames(Age.comp.data)[-1])
		age.data.names<-c(c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi","Nsamp"),paste0("f",Age.comp.data[,1]),paste0("m",Age.comp.data[,1]))
		if(length(age.samp.yrs)==1){
			data.file$agecomp<-data.frame(matrix(c(samp.yrs,
			rep(1,length(age.samp.yrs)),
			rep(1,length(age.samp.yrs)),
			rep(1,length(age.samp.yrs)),
			rep(0,length(age.samp.yrs)),
			rep(-1,length(age.samp.yrs)),
			rep(-1,length(age.samp.yrs)),
			rep(-1,length(age.samp.yrs)),
			colSums(Age.comp.data[-1]),
			t(Age.comp.data)[-1,],
			t(Age.comp.data)[-1,]*0),
			nrow=length(age.samp.yrs),
			ncol=9+length(Age.comp.data[,1])*2,
			byrow=FALSE))[,,drop=FALSE]
		}
		else{
		data.file$agecomp<-data.frame(matrix(cbind(samp.yrs,
			rep(1,length(age.samp.yrs)),
			rep(1,length(age.samp.yrs)),
			rep(1,length(age.samp.yrs)),
			rep(0,length(age.samp.yrs)),
			rep(-1,length(age.samp.yrs)),
			rep(-1,length(age.samp.yrs)),
			rep(-1,length(age.samp.yrs)),
			colSums(Age.comp.data[-1]),
			t(Age.comp.data)[-1,],
			t(Age.comp.data)[-1,]*0),
			nrow=length(age.samp.yrs),
			ncol=9+length(Age.comp.data[,1])*2,
			byrow=FALSE))[,,drop=FALSE]			
		}
		colnames(data.file$agecomp)<-age.data.names
		}
		
		SS_writedat(data.file,paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.dat"),overwrite=TRUE)			

		####################### END DATA FILE #####################################
##################################################################################
		####################### START CTL FILE ####################################

		#Read, edit then write new CONTROL file
    #LENGTH or AGE-ONLY
		if(all(!is.null(c(rv.Lt$data,rv.Age$data)),is.null(rv.Ct$data))==TRUE)
    {
    fem_vbgf<-VBGF(input$Linf_f,input$k_f,input$t0_f,c(0:Nages()))
    #Females
    ctl.file$MG_parms[1,3]<-input$M_f     #M
    ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]   #L0
    ctl.file$MG_parms[3,3:4]<-input$Linf_f    #Linf
    ctl.file$MG_parms[4,3:4]<-input$k_f     #k
    ctl.file$MG_parms[5,3:4]<-input$CV_lt_f   #CV
    ctl.file$MG_parms[6,3:4]<-input$CV_lt_f   #CV
    #Maturity6
    ctl.file$MG_parms[9,3:4]<-input$L50_f                 #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f-input$L50_f)  #Maturity slope
    #Males
    ctl.file$MG_parms[13,3]<-input$M_f      #M
    ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]    #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f   #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f    #k
    ctl.file$MG_parms[17,3:4]<-input$CV_lt_f  #CV
    ctl.file$MG_parms[18,3:4]<-input$CV_lt_f  #CV
    if(input$male_parms)
      {   
        male_vbgf<-VBGF(input$Linf_m,input$k_m,input$t0_m,c(0:Nages()))
        ctl.file$MG_parms[13,3]<-input$M_m      #M
        ctl.file$MG_parms[14,3:4]<-male_vbgf[1]   #L0
        ctl.file$MG_parms[15,3:4]<-input$Linf_m   #Linf
        ctl.file$MG_parms[16,3:4]<-input$k_m    #k
        ctl.file$MG_parms[17,3:4]<-input$CV_lt_m  #CV
        ctl.file$MG_parms[18,3:4]<-input$CV_lt_m  #CV
      }
        ctl.file$SR_parms[2,3:4]<-input$h_LO     #steepnes

    }

    #LENGTH and CATCH with fixed parameters
    if(all(any(input$est_parms==FALSE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data))))==TRUE)
    {
    fem_vbgf<-VBGF(input$Linf_f_fix,input$k_f_fix,input$t0_f_fix,c(0:Nages()))
    #Females
    ctl.file$MG_parms[1,3]<-input$M_f_fix           #M
    ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]           #L0
    ctl.file$MG_parms[3,3:4]<-input$Linf_f_fix      #Linf
    ctl.file$MG_parms[4,3:4]<-input$k_f_fix         #k
    ctl.file$MG_parms[5,3:4]<-input$CV_lt_f_fix     #CV
    ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_fix     #CV
    #Maturity6
    ctl.file$MG_parms[9,3:4]<-input$L50_f_fix                                     #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f_fix-input$L50_f_fix)  #Maturity slope
    #Males
    ctl.file$MG_parms[13,3]<-input$M_f_fix          #M
    ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]          #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f_fix     #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f_fix        #k
    ctl.file$MG_parms[17,3:4]<-input$CV_lt_f_fix    #CV
    ctl.file$MG_parms[18,3:4]<-input$CV_lt_f_fix    #CV
    if(input$male_parms)
      {   
        male_vbgf<-VBGF(input$Linf_m_fix,input$k_m_fix,input$t0_m_fix,c(0:Nages()))
        ctl.file$MG_parms[13,3]<-input$M_m_fix        #M
        ctl.file$MG_parms[14,3:4]<-male_vbgf[1]       #L0
        ctl.file$MG_parms[15,3:4]<-input$Linf_m_fix   #Linf
        ctl.file$MG_parms[16,3:4]<-input$k_m_fix      #k
        ctl.file$MG_parms[17,3:4]<-input$CV_lt_m_fix  #CV
        ctl.file$MG_parms[18,3:4]<-input$CV_lt_m_fix  #CV
      }
      
    #S-R
    ctl.file$SR_parms[1,3:4]<-input$lnR0  #lnR0
    ctl.file$SR_parms[2,3:4]<-input$h     #steepnes
    }

    #LENGTH and CATCH with estimated parameters
    if(all(any(input$est_parms==TRUE,input$est_parms2==FALSE),any(all(!is.null(rv.Lt$data),!is.null(rv.Ct$data)),all(!is.null(rv.Age$data),!is.null(rv.Ct$data))))==TRUE)
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
    if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[2,3:4]<-c(fem_vbgf[1],log(fem_vbgf[1]))}
    else {ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]}
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
    if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[5,3:4]<-c(input$CV_lt_f_mean,log(input$CV_lt_f_mean))}     
    else{ctl.file$MG_parms[5,3:4]<-input$CV_lt_f_mean}
    ctl.file$MG_parms[5,5]<-input$CV_lt_f_SD       
    ctl.file$MG_parms[5,6]<-prior.type[prior.name==input$CV_lt_f_prior]       
    ctl.file$MG_parms[5,7]<-input$CV_lt_f_phase       
    
    #CV old
    if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[6,3:4]<-c(input$CV_lt_f_mean,log(input$CV_lt_f_mean))}
    else{ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_mean}
    ctl.file$MG_parms[6,3:4]<-input$CV_lt_f_mean     
    ctl.file$MG_parms[6,5]<-input$CV_lt_f_SD       
    ctl.file$MG_parms[6,6]<-prior.type[prior.name==input$CV_lt_f_prior]  
    ctl.file$MG_parms[6,7]<-input$CV_lt_f_phase 

    #Maturity
    ctl.file$MG_parms[9,3:4]<-input$L50_f_est                                     #Lmat50%
    ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f_est-input$L50_f_est)  #Maturity slope
    
    #Males
    ctl.file$MG_parms[13,3:4]<-c(input$M_f_mean,log(input$M_f_mean))    #M
    ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]                              #L0
    ctl.file$MG_parms[15,3:4]<-input$Linf_f_mean                        #Linf
    ctl.file$MG_parms[16,3:4]<-input$k_f_mean                           #k
    ctl.file$MG_parms[17,3:4]<-input$CV_lt_f_mean                       #CV
    ctl.file$MG_parms[18,3:4]<-input$CV_lt_f_mean                       #CV
    
    if(input$male_parms_est)
      {   
        male_vbgf_est<-VBGF(input$Linf_m_mean,input$k_m_mean,input$t0_m_mean,c(0:Nages()))

              # ctl.file$MG_parms[13,3]<-input$M_m_mean        #M
        # ctl.file$MG_parms[14,3:4]<-male_vbgf_est[1]    #L0
        # ctl.file$MG_parms[15,3:4]<-input$Linf_m_mean   #Linf
        # ctl.file$MG_parms[16,3:4]<-input$k_m_mean      #k
        # ctl.file$MG_parms[17,3:4]<-input$CV_lt_m_mean  #CV
        # ctl.file$MG_parms[18,3:4]<-input$CV_lt_m_mean  #CV

        #M
        if(input$M_m_prior=="lognormal"){ctl.file$MG_parms[13,3:4]<-c(input$M_m_mean,log(input$M_m_mean))}
        else {ctl.file$MG_parms[13,3:4]<-c(input$M_f_mean,input$M_f_mean)}
        ctl.file$MG_parms[13,5]<-input$M_m_SD                            
        ctl.file$MG_parms[13,6]<-prior.type[prior.name==input$M_m_prior] 
        ctl.file$MG_parms[13,7]<-input$M_m_phase                         
            
        #L0    
        if(input$t0_f_prior=="lognormal"){ctl.file$MG_parms[14,3:4]<-c(male_vbgf_est[1],log(male_vbgf_est[1]))}
        else {ctl.file$MG_parms[14,3:4]<-male_vbgf_est[1]}
        ctl.file$MG_parms[14,5]<-input$t0_m_SD             
        ctl.file$MG_parms[14,6]<-prior.type[prior.name==input$t0_m_prior]
        ctl.file$MG_parms[14,7]<-input$t0_m_phase

        #Linf
        if(input$Linf_f_prior=="lognormal"){ctl.file$MG_parms[15,3:4]<-c(input$Linf_m_mean,log(input$Linf_m_mean))}     
        else{ctl.file$MG_parms[15,3:4]<-input$Linf_m_mean}
        ctl.file$MG_parms[15,5]<-input$Linf_m_SD         
        ctl.file$MG_parms[15,6]<-prior.type[prior.name==input$Linf_m_prior]      
        ctl.file$MG_parms[15,7]<-input$Linf_m_phase      

        #k
        if(input$k_f_prior=="lognormal"){ctl.file$MG_parms[16,3:4]<-c(input$k_m_mean,log(input$k_m_mean))}        
        else {ctl.file$MG_parms[16,3:4]<-input$k_m_mean}
        ctl.file$MG_parms[16,5]<-input$k_m_SD            
        ctl.file$MG_parms[16,6]<-prior.type[prior.name==input$k_m_prior]        
        ctl.file$MG_parms[16,7]<-input$k_m_phase         
        
        #CV young
        if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[17,3:4]<-c(input$CV_lt_m_mean,log(input$CV_lt_m_mean))}     
        else{ctl.file$MG_parms[17,3:4]<-input$CV_lt_m_mean}
        ctl.file$MG_parms[17,5]<-input$CV_lt_m_SD       
        ctl.file$MG_parms[17,6]<-prior.type[prior.name==input$CV_lt_m_prior]       
        ctl.file$MG_parms[17,7]<-input$CV_lt_m_phase       
        
        #CV old
        if(input$CV_lt_f_prior=="lognormal"){ctl.file$MG_parms[18,3:4]<-c(input$CV_lt_m_mean,log(input$CV_lt_m_mean))}
        else{ctl.file$MG_parms[18,3:4]<-input$CV_lt_m_mean}
        ctl.file$MG_parms[18,5]<-input$CV_lt_m_SD       
        ctl.file$MG_parms[18,6]<-prior.type[prior.name==input$CV_lt_m_prior]  
        ctl.file$MG_parms[18,7]<-input$CV_lt_m_phase 
      }     

    #S-R
    ctl.file$SR_parms[1,3:4]<-input$lnR0  #lnR0
    
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
				ctl.file$do_recdev<-1
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

		#Selectivity
		  Sel50<-as.numeric(trimws(unlist(strsplit(input$Sel50,","))))
      Sel50_phase<-as.numeric(trimws(unlist(strsplit(input$Sel50_phase,","))))
      Selpeak<-as.numeric(trimws(unlist(strsplit(input$Selpeak,","))))
      Selpeak_phase<-as.numeric(trimws(unlist(strsplit(input$Selpeak_phase,","))))
      bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]

    if(input$Sel_choice=="Logistic")
		{
			ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
      ctl.file$size_selex_parms[2,3:4]<- 15
      ctl.file$size_selex_parms[3,3:4]<- log(-((Sel50[1]-Selpeak[1])^2/log(0.5)))
			ctl.file$size_selex_parms[4,3:4]<- 15
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
      PeakDesc<-as.numeric(trimws(unlist(strsplit(input$PeakDesc,","))))
      PeakDesc_phase<-as.numeric(trimws(unlist(strsplit(input$PeakDesc_phase,","))))
      LtPeakFinal<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal,","))))
      LtPeakFinal_phase<-as.numeric(trimws(unlist(strsplit(input$LtPeakFinal_phase,","))))
      FinalSel<-as.numeric(trimws(unlist(strsplit(input$FinalSel,","))))
      FinalSel_phase<-as.numeric(trimws(unlist(strsplit(input$FinalSel_phase,","))))
			
      ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
      ctl.file$size_selex_parms[1,3:4]<- Selpeak[1]
			ctl.file$size_selex_parms[2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[1]-bin.width)/(PeakDesc[1]-Selpeak[1]-bin.width))
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

if(input$dirichlet)
{
  ctl.file$dirichlet_parms[,3:4]<-0
  ctl.file$dirichlet_parms[,7]<-2
}
    #Add other fleets
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				ctl.file$init_F<-rbind(ctl.file$init_F,ctl.file$init_F[1,])
				ctl.file$size_selex_types<-rbind(ctl.file$size_selex_types,ctl.file$size_selex_types[1,])
				ctl.file$age_selex_types<-rbind(ctl.file$age_selex_types,ctl.file$age_selex_types[1,])
				ctl.file$size_selex_parms<-rbind(ctl.file$size_selex_parms,ctl.file$size_selex_parms[1:6,])
        
        if(input$Sel_choice=="Logistic")
        {
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+3,7]<- Sel50_phase[i+1]
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
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
          ctl.file$size_selex_parms[6*i+1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
          ctl.file$size_selex_parms[6*i+1,3:4]<- Selpeak[i+1]
          ctl.file$size_selex_parms[6*i+1,7]<- Selpeak_phase[i+1]
          ctl.file$size_selex_parms[6*i+2,3:4]<- -log((max(data.file$lbin_vector)-Selpeak[i+1]-bin.width)/(PeakDesc[i+1]-Selpeak[i+1]-bin.width))
          ctl.file$size_selex_parms[6*i+2,7]<- PeakDesc_phase[i+1]
          ctl.file$size_selex_parms[6*i+3,3:4]<- log(-((Sel50[i+1]-Selpeak[i+1])^2/log(0.5)))
          ctl.file$size_selex_parms[6*i+3,7]<- Sel50_phase[i+1]
          ctl.file$size_selex_parms[6*i+4,3:4]<- log(LtPeakFinal[i+1])
          ctl.file$size_selex_parms[6*i+4,7]<- LtPeakFinal_phase[i+1]
          ctl.file$size_selex_parms[6*i+6,3:4]<- -log((1/(FinalSel[i+1]+0.000000001)-1))
          ctl.file$size_selex_parms[6*i+6,7]<- FinalSel_phase[i+1]          
        }
    
    #Dirichlet data-weighting
        ctl.file$dirichlet_parms<-rbind(ctl.file$dirichlet_parms,ctl.file$dirichlet_parms[1,])
			}

if(input$dirichlet)
{
  ctl.file$dirichlet_parms[,3:4]<-0
  ctl.file$dirichlet_parms[,7]<-2
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

    #Change data weights
    # Lt_dat_wts<-as.numeric(trimws(unlist(strsplit(input$Lt_datawts,","))))
    # ctl.file$Variance_adjustments[1,]<-Lt_dat_wts

		#Change likelihood component weight of catch
		if (is.null(rv.Ct$data))
			{
				ctl.file$lambdas[1,4]<-0
			}
		if(!is.null(rv.Ct$data))
			{
				ctl.file$lambdas[1,4]<-1
				ctl.file$lambdas[2,4]<-0
				ctl.file$init_F[,3]<-0.000001
				ctl.file$init_F[,7]<--1
			}

		SS_writectl(ctl.file,paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.ctl"),overwrite=TRUE)
		####################### END CTL FILE ####################################
	#Jitter 
				starter.file<-SS_readstarter(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/starter.ss"))
				starter.file$jitter_fraction<-0
		
		if(input$jitter_choice)
			{
				starter.file$jitter_fraction<-input$jitter_fraction
			}
 				SS_writestarter(starter.file,paste0(getwd(),"/Scenarios/",input$Scenario_name),overwrite=TRUE)


#Forecast file modfications
#Reference points
forecast.file<-SS_readforecast(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/forecast.ss"))

if(input$RP_choices){
    forecast.file$SPRtarget<-input$SPR_target
    forecast.file$Btarget<-input$B_target

    forecast.file$ControlRuleMethod<-input$CR_Ct_F
    forecast.file$SBforconstantF<-input$slope_hi
    forecast.file$BfornoF<-input$slope_low  
  }

if(input$Forecast_choice)
  {
    forecast.file$Nforecastyrs<-input$forecast_num
    forecast.file$Flimitfraction<-input$forecast_buffer
  }

SS_writeforecast(forecast.file,paste0(getwd(),"/Scenarios/",input$Scenario_name),overwrite=TRUE)  

########
	#Run Stock Synthesis and plot output
		RUN.SS(paste0(getwd(),"/Scenarios/",input$Scenario_name),ss.cmd="",OS.in=input$OS_choice)
		Model.output<-try(SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE))
		if(class(Model.output)=="try-error")
			{
				Model.output<-SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
			}
		#Make SS plots	
		SS_plots(Model.output,maxyr=data.file$endyr,verbose=FALSE)
		#Make SS tables
		try(SSexecutivesummary(Model.output))		
			 
		#Run multiple jitters
		if(input$jitter_choice)
		{
			if(input$Njitter>1)
			{
				 jits<-SS_RunJitter(paste0(getwd(),"/Scenarios/",input$Scenario_name),Njitter=input$Njitter,printlikes = TRUE)
				 profilemodels <- SSgetoutput(dirvec=paste0(getwd(),"/Scenarios/",input$Scenario_name), keyvec=0:input$Njitter, getcovar=FALSE)
				 profilesummary <- SSsummarize(profilemodels)
	       minlikes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]==min(profilesummary$likelihoods[1,-length(profilesummary$likelihoods)])
				 #Find best fit model
				 index.minlikes<-c(1:length(minlikes))[minlikes]
				 file.copy(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/ss.par_",(index.minlikes[1]-1),".sso"),paste0(getwd(),"/Scenarios/",input$Scenario_name,"/ss.par"),overwrite = TRUE)
		         starter.file$init_values_src<-1
             starter.file$jitter_fraction<-0
			 	 SS_writestarter(starter.file,paste0(getwd(),"/Scenarios/",input$Scenario_name),overwrite=TRUE)
			 	 #R-run to get new best fit model
				 RUN.SS(paste0(getwd(),"/Scenarios/",input$Scenario_name),ss.cmd="",OS.in=input$OS_choice)
         Model.output<-try(SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE))
          if(class(Model.output)=="try-error")
          {
            Model.output<-SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
          }
				 SS_plots(Model.output,maxyr=data.file$endyr,verbose=FALSE)
				 try(SSexecutivesummary(Model.output))		
				 jitter.likes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]
				 ref.like<-min(jitter.likes)
		    	 #Make plot and save to folder
		    	 main.dir<-getwd()
           if(!file.exists(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/Jitter Results")))
          {
              dir.create(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/Jitter Results"))
          }
           setwd(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/Jitter Results"))
		     	 png("jitterplot.png")
				 jitterplot<-plot(c(1:length(jitter.likes)),jitter.likes,type="p",col="black",bg="blue",pch=21,xlab="Jitter run",ylab="-log likelihood value",cex=1.25)
				 points(c(1:length(jitter.likes))[jitter.likes>min(jitter.likes)],jitter.likes[jitter.likes>min(jitter.likes)],type="p",col="black",bg="red",pch=21,cex=1.25)
				 abline(h=ref.like)
				 # likebc<-round((length(jitter.likes[ref.like==jitter.likes])/(input$Njitter+1))*100,0)
				 # likelessbc<-round((length(jitter.likes[ref.like>jitter.likes])/(input$Njitter+1))*100,0)
				 # like10<-round((length(jitter.likes[(ref.like+10)<jitter.likes])/(input$Njitter+1))*100,0)
				 # like2<-round(((length(jitter.likes[(ref.like+2)>jitter.likes])-(length(jitter.likes[ref.like==jitter.likes])))/(input$Njitter+1))*100,0)
				 # like_2_10<-round(100-(likebc+like10+like2),0)
				 # legend("topright",c(paste("  ",likelessbc,"% < BC",sep=""),paste(likebc,"% = BC",sep=""),paste(like2,"% < BC+2",sep=""),paste(like_2_10,"% > BC+2 & < BC+10",sep=""),paste(like10,"% > BC+10",sep="")),bty="n")
				 dev.off()
          save(profilesummary,file=paste0(getwd(),"/jitter_summary.DMP"))
          SSplotComparisons(profilesummary, legendlabels = c(0:input$Njitter), ylimAdj = 1.30, subplot = c(1), new = FALSE,print=TRUE,plotdir=getwd())
          SSplotComparisons(profilesummary, legendlabels = c(0:input$Njitter), ylimAdj = 1.30, subplot = c(3), new = FALSE,print=TRUE,plotdir=getwd())
        
				output$Jitterplot<-renderPlot({
				#	if(input$Njitter==1){return(NULL)}
				#	if(input$Njitter>1)
				# {
				 #jitter.likes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]
				 #ref.like<-min(jitter.likes)
		    	 jitterplot<-plot(c(1:length(jitter.likes)),jitter.likes,type="p",col="black",bg="blue",pch=21,xlab="Jitter run",ylab="-log likelihood value",cex=1.25)
				 points(c(1:length(jitter.likes))[jitter.likes>min(jitter.likes)],jitter.likes[jitter.likes>min(jitter.likes)],type="p",col="black",bg="red",pch=21,cex=1.25)
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
		}		
	setwd(main.dir)
           
  }
		
	

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
 				else{converge.dec<-"Model may not have converged. Please use the Jitter option or change starting values before re-running model."}
			})
 		
 		#Relative biomass
		output$SSout_relSB_table <- renderTable({
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
				Output_relSB_table[,1]<-c(paste0("SO",input$endyr+1,"/SO_0"),
										  "SO_MSY/SO_0",
										  paste0("1-SPR",input$endyr+1),
										  paste0("OFL",(input$endyr+1)),
										  paste0("ABC",(input$endyr+1))
										  )
				Output_relSB_table	
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
		output$SSout_F_table <- renderTable({
				F_indices<-c(which(rownames(Model.output$derived_quants)==paste0("F_",input$endyr)),
							which(rownames(Model.output$derived_quants)=="annF_Btgt"),
							which(rownames(Model.output$derived_quants)=="annF_SPR"),
							which(rownames(Model.output$derived_quants)=="annF_MSY")
							)
				F_values<-Model.output$derived_quants[F_indices,1:3]
 			})
		#Time series output
 		output$SSout_table <- renderTable({
# 				Output_table<-Model.output$sprseries[-nrow(Model.output$sprseries),c(1,5,6,7,8,9,11,12,13,25,37)]
        Output_table<-Model.output$sprseries[,c(1,5,6,7,8,9,11,12,13,25,37)]
			})
 		
 		#Paramters
 		
 		output$Parameters_table <- renderTable({
 				Model.output$estimated_non_dev_parameters
			})

	})

    #Sensitivity comparisons
      roots <- getVolumes()()  
      shinyDirChoose(input, "Sensi_dir", roots=roots, filetypes=c('', 'txt'))
      path1 <- reactive({
        return(parseDirPath(roots, input$Sensi_dir))
      })

  output$Sensi_model_picks<-renderUI({
      #dirinfo <- parseDirPath(roots, input$Sensi_dir)
      pickerInput(
      inputId = "myPicker",
      label = "Choose scenarios to compare",
      #choices = list.files(dirinfo),
      choices = list.files(path1()),
      options = list(
        `actions-box` = TRUE,
        size = 12,
        `selected-text-format` = "count > 3"
        ),
      multiple = TRUE
    )
 })

#SS.comparisons<-observeEvent(as.numeric(input$tabs)==5,{
Sensi_model_dir_out<-eventReactive(req(input$run_Sensi_comps&!is.null(input$myPicker)&as.numeric(input$tabs)==5),{
    if(!file.exists(paste0(path1(),"/Sensitivity Comparison Plots")))
      {
        dir.create(paste0(path1(),"/Sensitivity Comparison Plots"))
      }
    Sensi_model_dir_out<-paste0(path1(),"/",input$myPicker)
  })

  observeEvent(exists(Sensi_model_dir_out()),{
       modelnames<-input$myPicker
       zz<-list()
       Runs<-length(Sensi_model_dir_out())
       for(i in 1:Runs) {zz[[i]]<-SS_output(paste0(Sensi_model_dir_out()[i]))}
       modsummary.sensi<- SSsummarize(zz)

       col.vec = rc(n=length(modelnames), alpha = 1)
       shade = adjustcolor(col.vec[1], alpha.f = 0.10)

       pngfun(wd = paste0(path1(),"/Sensitivity Comparison Plots"), file = paste0(input$Sensi_comp_file,".png"), h = 7,w = 12)
       par(mfrow = c(1,3))
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30, subplot = c(2,4),col = col.vec, shadecol = shade, new = FALSE))
       try(SSplotComparisons(modsummary.sensi, legendlabels = modelnames, ylimAdj = 1.30, subplot = 11,col = col.vec, shadecol = shade, new = FALSE, legendloc = 'topleft'))
       dev.off()
       save(modsummary.sensi,file=paste0(path1(),"/Sensitivity Comparison Plots/",input$Sensi_comp_file,".DMP"))
       output$Sensi_comp_plot <- renderImage({
       image.path<-normalizePath(file.path(paste0(path1(),"/Sensitivity Comparison Plots/",
               input$Sensi_comp_file, '.png')),mustWork=FALSE)
       return(list(
        src = image.path,
        contentType = "image/png",
       #  width = 400,
       # height = 300,
       style='height:60vh'))
      },deleteFile=FALSE)

  })

test.in<-reactive({
  print("A")
#  print(input$run_Ensemble)
#  print(as.numeric(input$tabs))
 # print(input$myEnsemble)
  return(10)
  })
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

    #Ensemble modelling
      #roots <- getVolumes()()  
      shinyDirChoose(input, "Ensemble_dir", roots=roots, filetypes=c('', 'txt'))
      path2 <- reactive({
        return(parseDirPath(roots, input$Ensemble_dir))
      })

  output$Ensemble_model_picks<-renderUI({
      pickerInput(
      inputId = "myEnsemble",
      label = "Choose scenarios to ensemble",
      choices = list.files(path2()),
      options = list(
        `actions-box` = TRUE,
        size = 12,
        `selected-text-format` = "count > 3"
        ),
      multiple = TRUE
    )
 })


#Ensemble_model_dir_out<-eventReactive(req(input$run_Ensemble&!is.null(input$myEnsemble)&as.numeric(input$tabs)==6),{
observeEvent(req(input$run_Ensemble&!is.null(input$myEnsemble)&as.numeric(input$tabs)==6),{
Ensemble_model_dir_out<-eventReactive(input$run_Ensemble,{
#print(as.numeric(input$tabs))
#print(input$run_Ensemble)
    if(!file.exists(paste0(path2(),"/Ensemble outputs")))
      {
        dir.create(paste0(path2(),"/Ensemble outputs"))
      }
    Ensemble_model_dir_out<-paste0(path2(),"/",input$myEnsemble)
  })
print(Ensemble_model_dir_out())
exists("Ensemble_model_dir_out()")
})

#exists(Ensemble_model_dir_out())
  observeEvent(req(input$run_Ensemble&!is.null(input$myEnsemble)),{
  Ensemble.outputs<-eventReactive(input$run_Ensemble,{
       print(length(Ensemble_model_dir_out()))
       modelnames<-input$myEnsemble
       zz<-list()
       Runs<-length(Ensemble_model_dir_out())
       for(i in 1:Runs) {zz[[i]]<-SS_output(paste0(Ensemble_model_dir_out()[i]))}
       modsummary.ensemble<- SSsummarize(zz)
       Ensemble_wts<-as.numeric(trimws(unlist(strsplit(input$Ensemble_wts,","))))
       Stand_ensemble_wts<-Ensemble_wts/sum(Ensemble_wts)
       Nsamps_ensemble<-100000
       Nsamps_ensemble_wts<-Nsamps_ensemble*Stand_ensemble_wts
       #Calculate weighted values
       #Unfished spawning outputs
       SO.init.mods<-modsummary.ensemble$SpawnBio[2,1:(ncol(modsummary.ensemble$SpawnBio)-2)] 
       SO.init.sd.mods<-modsummary.ensemble$SpawnBioSD[2,1:(ncol(modsummary.ensemble$SpawnBio)-2)] 
       #Current spawning output
       SO.cur.mods<-modsummary.ensemble$SpawnBio[nrow(modsummary.ensemble$SpawnBio),1:(ncol(modsummary.ensemble$SpawnBio)-2)] 
       SO.cur.sd.mods<-modsummary.ensemble$SpawnBioSD[nrow(modsummary.ensemble$SpawnBio),1:(ncol(modsummary.ensemble$SpawnBio)-2)] 
       #Current reatlive stock status
       Bratio.mods<-modsummary.ensemble$Bratio[2,1:(ncol(modsummary.ensemble$Bratio)-2)] 
       Bratio.sd.mods<-modsummary.ensemble$BratioSD[2,1:(ncol(modsummary.ensemble$Bratio)-2)] 
       #SPR
       SPR.mods<-modsummary.ensemble$SPRratio[2,1:(ncol(modsummary.ensemble$SPRratio)-2)] 
       SPR.sd.mods<-modsummary.ensemble$SPRratioSD[2,1:(ncol(modsummary.ensemble$SPRratio)-2)] 
       #Overfishing levels
       
       #Forecasted catches

       #Create weighted ensembles
       Ensemble.outputs<-list()
       Ensemble.outputs[[1]]<-list.rbind(mapply(function(x) data.frame(SO=rrnorm(Nsamps_ensemble_wts[x],
                                  SO.init.mods[x],
                                  SO.init.sd.mods[x]),
                                  Label=modelnames[x]),
                                  x=1:length(SO.init.mods),
                                  SIMPLIFY=FALSE))
       Ensemble.outputs[[2]]<-list.rbind(mapply(function(x) data.frame(SO=rrnorm(Nsamps_ensemble_wts[x],
                                  SO.cur.mods[x],
                                  SO.cur.sd.mods[x]),
                                  Label=modelnames[x]),
                                  x=1:length(SO.cur.mods),
                                  SIMPLIFY=FALSE))
       Ensemble.outputs[[3]]<-list.rbind(mapply(function(x) data.frame(SO=rrnorm(Nsamps_ensemble_wts[x],
                                  Bratio.mods[x],
                                  Bratio.sd.mods[x]),
                                  Label=modelnames[x]),
                                  x=1:length(Bratio.mods),
                                  SIMPLIFY=FALSE))
       Ensemble.outputs[[4]]<-list.rbind(mapply(function(x) data.frame(SO=rrnorm(Nsamps_ensemble_wts[x],
                                  SPR.mods[x],
                                  SPR.sd.mods[x]),
                                  Label=modelnames[x]),
                                  x=1:length(SPR.mods),
                                  SIMPLIFY=FALSE))
       names(Ensemble.outputs)<-c("SOinitial","SOcurrent","RelativeSO_curr","SPR_curr")
       save(Ensemble.outputs,file=paste0(path2(),"/Ensemble outputs/",input$Ensemble_file,".DMP"))
       return(Ensemble.outputs)
    })
  })

observeEvent(req(input$run_Ensemble&exists("Ensemble.outputs()")),{
output$Ensemble_plots <- renderPlot({ 
      hist(Ensemble.outputs()[[1]][,1])
    })
  })
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

