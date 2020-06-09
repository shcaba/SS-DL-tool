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
  

RUN.SS<-function(path, ss.exe="ss",ss.cmd=" -nohess -nox"){ 
  navigate <- paste("cd ", path, sep="") 
  command <- paste(navigate," & ", ss.exe, ss.cmd,sep="") 
  shell(command, invisible=TRUE, translate=TRUE) 
}  


########## Clear data files and plots ############

  rv.Lt <- reactiveValues(data = NULL)
  rv.Age <- reactiveValues(data = NULL)
  rv.Ct <- reactiveValues(data = NULL)
   
 
  observeEvent(input$reset_lt, {
    rv.Lt$data <- NULL
    reset('file1')
  })

  # observeEvent(input$reset_lt, {
		# output$Ltplot<-renderPlot({
		# rv.Lt$data <- NULL
		# if (is.null(rv.Lt$data)) return(NULL)  
		# })
  # })

  observeEvent(input$reset_age, {
    rv.Age$data <- NULL
    reset('file3')
  })

  observeEvent(input$reset_ct, {
    rv.Ct$data <- NULL
    reset('file2')
  })

# observeEvent(, {
#     if(is.null(input$file1))
#     {
#    		shinyjs::hideElement(id= "panel_LH")
#     }
#   })
observeEvent(req(is.null(input$file1)&!is.null(input$file2)&is.null(input$file3)), {
    	shinyjs::showElement(id= "panel_SSS")
  })


observeEvent(req(!is.null(input$file1)), {
    	shinyjs::showElement(id= "panel_SS")
  })


########################################

	
# User activated pop-up parameter values ---------------

#Model dimensions
output$Model_dims1 <- renderUI({ 
			  inFile1 = input$file1 
				inFile2 = input$file2 
				 
				if (is.null(inFile1) & is.null(inFile2)) return(NULL) 
				if (!is.null(inFile1) & is.null(inFile2)){ 
						Lt.comp.data = read.csv(inFile1$datapath,check.names=FALSE) 
			    		styr.in =  min(Lt.comp.data[,1]) 
			    		if(!(anyNA(c(input$Linf_f, input$k_f,input$t0_f)))){ 
			    			styr.in = min(Lt.comp.data[,1])-round(VBGF.age(input$Linf_f, input$k_f, input$t0_f, input$Linf_f*0.95)) 
			    		print(styr.in) 
			    		print(max(Lt.comp.data[,1])) 
			    		} 
						fluidRow(column(width=4, numericInput("styr", "Starting year",  
						                                      value=styr.in, min=1, max=10000, step=1)), 
              			column(width=4, numericInput("endyr","Ending year",  
              			                             value=max(Lt.comp.data[,1]), min=1, max=10000, step=1)))						 
					} 
		})

output$Model_dims2 <- renderUI({ 
				inFile2 <- input$file2 
				if (is.null(inFile2)) return(NULL) 
				if (!is.null(inFile2)){ 				   
						Ct.data = read.csv(inFile2$datapath,check.names=FALSE) 
			    		fluidRow(column(width=4, numericInput("styr", "Starting year",  
			    		                                     value=min(Ct.data[,1]), min=1, max=10000, step=1)), 
              			    column(width=4, numericInput("endyr", "Ending year",  
              			                            value=max(Ct.data[,1]), min=1, max=10000, step=1)))						 
					}	 
		}) 


output$Female_parms_inputs_label <- reactive({
if(!is.null(input$file1))
	{
		(output$Female_parms_inputs_label<- renderUI({
			      fluidRow(column(width=6,numericInput("Nages","Max. age", value=NA,min=1, max=1000, step=1)),
		             column(width=6,numericInput("M_f", "Natural mortality", value=NA,min=0, max=10000, step=0.01)))    
			      }))
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
 


#Male life history parameters
output$Male_parms_inputs_label_SSS<- renderUI({
	if(input$male_parms_SSS){
   		 h5(em("Male"))
   			}		
		})

output$Male_parms_inputs1_SSS<- renderUI({
	if(input$male_parms_SSS){
    fluidRow(column(width=6,numericInput("M_m", "Natural mortality", 
    									value=NA,min=0, max=10000, step=0.01)),
            column(width=6,numericInput("Linf_m", "Asymptotic size (Linf)", 
            							value=NA,min=0, max=10000, step=0.01)))    
		}
	})

output$Male_parms_inputs2_SSS<- renderUI({
	if(input$male_parms_SSS){
    fluidRow(column(width=6,numericInput("k_m","Growth coefficient k", 
    									value=NA,min=0, max=10000, step=0.01)),
            column(width=6,numericInput("t0_m","Age at length 0 (t0)", 
            							value=NA,min=0, max=10000, step=0.01)))    
    	}
	})

output$Male_parms_inputs3_SSS<- renderUI({
	if(input$male_parms_SSS){
    fluidRow(column(width=6,numericInput("CV_lt_m","CV at length", 
    									value=0.1,min=0, max=10000, step=0.01)))
    	}
	})

output$Male_parms_inputs4_SSS<- renderUI({
	if(input$male_parms_SSS){
      fluidRow(column(width=6,numericInput("WLa_m", "Weight-Length alpha", 
      										value=0.00001,min=0, max=10000, step=0.000000001)),
              column(width=6,numericInput("WLb_m","Weight-length beta", 
              								value=3,min=0, max=10000, step=0.01)))    
    	}
	})

#Selectivity paramters
output$Sel_parms1 <- renderUI({ 
    fluidRow(column(width=8, numericInput("Sel50", "Length at 50% Selectivity",  
                                          value=NA, min=0, max=10000, step=0.01)), 
            column(width=4, numericInput("Sel50_phase", "Est. phase",  
                                         value=1, min=-1000, max=10, step=1)))     
	}) 
 
output$Sel_parms2<- renderUI({ 
    	fluidRow(column(width=8, numericInput("Selpeak", "Length at Peak Selectvity",  
    	                                      value=NA, min=0, max=10000, step=0.01)), 
            	 column(width=4, numericInput("Selpeak_phase", "Est. phase",  
            	                             value=1, min=-1000, max=10, step=1))) 
	}) 
 
output$Sel_parms3 <- renderUI({ 
  		if(input$Sel_choice=="Dome-shaped"){ 			 
    	fluidRow(column(width=8, numericInput("PeakDesc", "Length at first declining selectivity",  
    	                                      value=NA, min=0, max=10000, step=0.01)), 
            	 column(width=4, numericInput("PeakDesc_phase", "Est. phase",  
            	                             value=1, min=-1000, max=10, step=1))) 
 		} 
	}) 
 
 
output$Sel_parms4 <- renderUI({ 
 		if(input$Sel_choice=="Dome-shaped"){ 			 
	    fluidRow(column(width=8, numericInput("LtPeakFinal", "Width of declining selectivity",  
	                                          value=1, min=0, max=10000, step=0.01)), 
	             column(width=4, numericInput("LtPeakFinal_phase", "Est. phase",   
	                                          value=1, min=-1000, max=10, step=1)))    			 
 		} 
	}) 
 
output$Sel_parms5 <- renderUI({ 
 		if(input$Sel_choice=="Dome-shaped"){ 			 
    	fluidRow(column(width=8, numericInput("FinalSel", "Selectivity at max bin size",  
    	                                      value=0.9999999, min=0, max=0.9999999, step=0.0000001)), 
            	column(width=4, numericInput("FinalSel_phase", "Est. phase",  
            	                             value=1, min=-1000, max=10, step=1))) 
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
        fluidRow(column(width=6, numericInput("slope_hi", "Control rule: Upper ratio value",  
                                              value=0.4, min=0, max=1, step=0.001)), 
        	       column(width=6, numericInput("slope_low", "Control rule: Lower ratio value",  
        	                                    value=0.1, min=0, max=1, step=0.001)))    
    	} 
	}) 
 
output$Forecasts<- renderUI({ 
    if(input$Forecast_choice){ 
        fluidRow(column(width=6, numericInput("forecast_num", "# of forecast years",  
                                              value=1, min=1, max=1000, step=1)), 
        	       column(width=6, numericInput("forecast_buffer", "Control rule buffer",  
        	                                    value=0.913, min=0, max=1, step=0.001)))    
    	} 
	}) 

#############
### PLOTS ###
#############

observeEvent(req(!is.null(input$file1)), {
    	shinyjs::show(output$lt_comp_plots_label<-renderText({"Length compositions"}))
  })


#Plot length compoistions
# length compositions 
observeEvent(req(!is.null(input$file1)), {
	output$Ltplot<-renderPlot({ 
		  inFile <- input$file1 
		  if (is.null(inFile)) return(NULL) 
		   
		  read.csv(inFile$datapath, check.names=FALSE) %>%  
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

observeEvent(req(!is.null(input$file3)), {
    	shinyjs::show(output$age_comp_plots_label<-renderText({"Age compositions"}))
  })

output$Ageplot <- renderPlot({ 
		inFile_age <- input$file3 
		if (is.null(inFile_age)) return(NULL) 
		 
		read.csv(inFile_age$datapath, check.names=FALSE) %>%  
		      pivot_longer(-1, names_to = "year", values_to = "ltnum") %>%  
		      rename(bin = Bins) %>%  
		  ggplot(aes(bin, ltnum)) + 
					geom_col(fill="#1D252D", color="white") + 
					facet_wrap(~year) + 
					xlab("Age bin") + 
					ylab("Frequency")			 
	}) 
 

 observeEvent(req(!is.null(input$file2)), {
    	shinyjs::show(output$catch_comp_plots_label<-renderText({"Removal history"}))
  })

output$Ctplot <- renderPlot({ 
		inCatch <- input$file2 
		if (is.null(inCatch)) return(NULL) 
 
		read.csv(inCatch$datapath, check.names=FALSE) %>%  
		    pivot_longer(-1, names_to = "Fleet", values_to = "catch") %>%  
		    ggplot(aes_string(names(.)[1], "catch", color = "Fleet")) +  
		    geom_point() +  
		    geom_line() + 
		    ylab("Removals") + 
		    xlab("Year") +  
		    scale_color_viridis_d() 
		}) 


# output$Ctplot<-renderPlot({
# 		inCatch<- input$file2
# 		if (is.null(inCatch)) {
# 			return(NULL)
# 			shinyjs::hide("Ctplot")}
# 		Catch.data<-read.csv(inCatch$datapath,header=TRUE)
# 		colnames(Catch.data)[1]<-"year"
# 		if(ncol(Catch.data)==2){
# 		ggplot(Catch.data,aes(get(colnames(Catch.data)[1]),get(colnames(Catch.data)[2])))+
# 			geom_col(fill="#658D1B",color="white")+
# 			xlab("Year")+
# 			ylab("Removals")					
# 		}
# 		if(ncol(Catch.data)>2){
# 			Catch.data<-melt(Catch.data,id=c("year"))
# 			colnames(Catch.data)<-c("year","fleet","catch")
# 			ggplot(Catch.data,aes(year,catch,fill=fleet))+
# 			geom_col(color="white",position="dodge")+
# 			#geom_col(fill="#658D1B",color="white")+
# 			xlab("Year")+
# 			ylab("Removals")+
# 			scale_fill_viridis(discrete=TRUE, option="viridis")				
# 			}
# 		})

#Plot M by age
output$Mplot<-renderPlot({ 
			mf.in = input$M_f 
			mm.in = input$M_f 
			if(input$male_parms){ 
			  mm.in = input$M_m 
			  }		 
			if(any(is.na(c(mf.in, mm.in)))) return(NULL) 
			 
			Female_M = data.frame(Ages = 0:input$Nages, PopN = exp(-mf.in * 0:input$Nages), Sex="Female") 
			Male_M = data.frame(Ages = 0:input$Nages, PopN=exp(-mm.in * 0:input$Nages), Sex="Male") 
			M_sexes <- rbind(Female_M, Male_M) 
			ggplot(M_sexes,aes(Ages, PopN, color=Sex))+ 
					geom_line(aes(linetype=Sex), lwd=2)+ 
					ylab("Cohort decline by M") 
		}) 

#Plot VBGF and maturity
output$VBGFplot<-renderPlot({ 
   	f_Linf = m_Linf = input$Linf_f 
   	f_k = m_k = input$k_f 
   	f_t0 = m_t0 = input$t0_f 
	f_L50 = input$L50_f 
	f_L95 = input$L95_f 
	maxage = input$Nages 
	if(input$male_parms){ 
				m_Linf = input$Linf_m 
			   	m_k = input$k_m 
			   	m_t0 = input$t0_m 
			}		 
   if(any(is.na(c(f_Linf, f_k, f_t0)))=="FALSE"){ 
		vbgf_female = data.frame(Age = 0:input$Nages,  
		                         Length = VBGF(f_Linf, f_k, f_t0, 0:input$Nages), Sex="Female") 
    vbgf_male = data.frame(Age = 0:input$Nages,  
                           Length=VBGF(m_Linf, m_k, f_t0, 0:input$Nages), Sex="Male") 
      	rbind(vbgf_female,vbgf_male) %>%  
      	  ggplot(aes(Age, Length, color=Sex)) + 
      				geom_line(aes(linetype=Sex), lwd=2) -> vbgf.plot  
      	 
      if(any(is.na(c(f_L50, f_L95)))=="FALSE"){ 
        age.mat = data.frame(Age = VBGF.age(f_Linf, f_k, f_t0, c(f_L50, f_L95)), 
                             Length = c(f_L50, f_L95), Sex="Female") 
        vbgf.plot + 
        	geom_point(data = age.mat, aes(Age, Length), color = "darkorange", size=6) + 
        	geom_text(data = age.mat,label=c("Lmat50%", "Lmat95%"), 
        	          nudge_x = -0.1 * input$Nages, color="black") -> vbgf.plot 
       } 
  	 vbgf.plot 
  	 } 
	}) 

#############################################
### PREPARE FILES andD RUN Stock Synthsis ###
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
		data.file$Nages<-input$Nages

	#Catches
		inCatch<- input$file2
		if (is.null(inCatch)) 
		{
		inFile<- input$file1
		Lt.comp.data<-read.csv(inFile$datapath,check.names=FALSE)
		data.file$Nfleets<-max(Lt.comp.data[,2])
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
				data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
				data.file$len_info<-rbind(data.file$len_info,data.file$len_info[1,])
				data.file$age_info<-rbind(data.file$age_info,data.file$age_info[1,])
			}
			data.file$fleetinfo$fleetname<-paste0("Fishery",1:data.file$Nfleets)
			data.file$CPUEinfo[,1]<-1:data.file$Nfleets
		}
		year.in<-input$styr:input$endyr
		catch.cols<-colnames(data.file$catch)
		catch_temp<-list()
		for(i in 1:data.file$Nfleets)
		{
		catch_temp[[i]]<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(i,length(year.in)+1),
						c(100,rep(100,length(year.in))),
						c(0.01,rep(1000,length(year.in)))
						)			
		}
		data.file$catch<-list.rbind(catch_temp)
		colnames(data.file$catch)<-catch.cols
		}

		if(!is.null(inCatch))
		{
		Catch.data<-read.csv(inCatch$datapath,header=TRUE)
		data.file$Nfleets<-ncol(Catch.data)-1
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				data.file$fleetinfo<-rbind(data.file$fleetinfo,data.file$fleetinfo[1,])
				data.file$CPUEinfo<-rbind(data.file$CPUEinfo,data.file$CPUEinfo[1,])
				data.file$len_info<-rbind(data.file$len_info,data.file$len_info[1,])
				data.file$age_info<-rbind(data.file$age_info,data.file$age_info[1,])
			}
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
		inFile<- input$file1
		if (is.null(inFile)) return(NULL)
		Lt.comp.data<-read.csv(inFile$datapath,check.names=FALSE)
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
		inFile_age<- input$file3
		if (is.null(inFile_age)){
		data.file$N_agebins<-input$Nages
		data.file$agebin_vector<-1:input$Nages		
		data.file$ageerror<-data.frame(matrix(c(rep(-1,(input$Nages+1)),rep(0.001,(input$Nages+1))),2,(input$Nages+1),byrow=TRUE))
		colnames(data.file$ageerror)<-paste0("age",1:input$Nages)		
			}
		if (!is.null(inFile_age)){
		Age.comp.data<-read.csv(inFile_age$datapath,check.names=FALSE)
		age.classes<-nrow(Age.comp.data)
		data.file$N_agebins<-age.classes
		data.file$agebin_vector<-Age.comp.data[,1]
		data.file$ageerror<-data.frame(matrix(c(rep(-1,(age.classes+1)),rep(0.001,(age.classes+1))),2,(age.classes+1),byrow=TRUE))		
		colnames(data.file$ageerror)<-paste0("age",1:input$Nages)		
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

		####################### START CTL FILE ####################################
		#Read, edit then write new CONTROL file
		fem_vbgf<-VBGF(input$Linf_f,input$k_f,input$t0_f,c(0:input$Nages))
		#Females
		ctl.file$MG_parms[1,3]<-input$M_f			#M
		ctl.file$MG_parms[2,3:4]<-fem_vbgf[1]		#L0
		ctl.file$MG_parms[3,3:4]<-input$Linf_f		#Linf
		ctl.file$MG_parms[4,3:4]<-input$k_f			#k
		ctl.file$MG_parms[5,3:4]<-input$CV_lt_f		#CV
		ctl.file$MG_parms[6,3:4]<-input$CV_lt_f		#CV
		#Maturity6
		ctl.file$MG_parms[9,3:4]<-input$L50_f									#Lmat50%
		ctl.file$MG_parms[10,3:4]<- log(0.05/0.95)/(input$L95_f-input$L50_f)	#Maturity slope
		#Males
		ctl.file$MG_parms[13,3]<-input$M_f			#M
		ctl.file$MG_parms[14,3:4]<-fem_vbgf[1]		#L0
		ctl.file$MG_parms[15,3:4]<-input$Linf_f		#Linf
		ctl.file$MG_parms[16,3:4]<-input$k_f		#k
		ctl.file$MG_parms[17,3:4]<-input$CV_lt_f	#CV
		ctl.file$MG_parms[18,3:4]<-input$CV_lt_f	#CV
		if(input$male_parms)
			{		
				male_vbgf<-VBGF(input$Linf_m,input$k_m,input$t0_m,c(0:input$Nages))
				ctl.file$MG_parms[13,3]<-input$M_m			#M
				ctl.file$MG_parms[14,3:4]<-male_vbgf[1]		#L0
				ctl.file$MG_parms[15,3:4]<-input$Linf_m		#Linf
				ctl.file$MG_parms[16,3:4]<-input$k_m		#k
				ctl.file$MG_parms[17,3:4]<-input$CV_lt_m	#CV
				ctl.file$MG_parms[18,3:4]<-input$CV_lt_m	#CV
			}

		#S-R
		ctl.file$SR_parms[1,3:4]<-input$lnR0	#lnR0
		ctl.file$SR_parms[2,3:4]<-input$h 		#steepnes

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
		if(input$Sel_choice=="Logistic")
		{
			bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]
			ctl.file$size_selex_parms[3,3:4]<- log(-((input$Sel50-input$Selpeak)^2/log(0.5)))
			ctl.file$size_selex_parms[3,7]<- input$Sel50_phase
			ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector)+2*bin.width,max(data.file$lbin_vector)-2*bin.width)
			ctl.file$size_selex_parms[1,3:4]<- input$Selpeak
			ctl.file$size_selex_parms[1,7]<- input$Selpeak_phase
			ctl.file$size_selex_parms[2,3:4]<- 15
			ctl.file$size_selex_parms[2,7]<- -1
			ctl.file$size_selex_parms[4,3:4]<- 15
			ctl.file$size_selex_parms[4,7]<- -1
			ctl.file$size_selex_parms[6,3:4]<- 15
			ctl.file$size_selex_parms[6,7]<- -1
			}
		if(input$Sel_choice=="Dome-shaped")
		{
			bin.width<-data.file$lbin_vector[2]-data.file$lbin_vector[1]
			ctl.file$size_selex_parms[1,1:2]<-c(min(data.file$lbin_vector),max(data.file$lbin_vector))
			ctl.file$size_selex_parms[1,3:4]<- input$Selpeak
			ctl.file$size_selex_parms[1,7]<- input$Selpeak_phase
			ctl.file$size_selex_parms[2,3:4]<- -log((max(data.file$lbin_vector)-input$Selpeak-bin.width)/(input$PeakDesc-input$Selpeak-bin.width))
			ctl.file$size_selex_parms[2,7]<- input$PeakDesc_phase
			ctl.file$size_selex_parms[3,3:4]<- log(-((input$Sel50-input$Selpeak)^2/log(0.5)))
			ctl.file$size_selex_parms[3,7]<- input$Sel50_phase
			ctl.file$size_selex_parms[4,3:4]<- log(input$LtPeakFinal)
			ctl.file$size_selex_parms[4,7]<- input$LtPeakFinal_phase
			ctl.file$size_selex_parms[6,3:4]<- -log((1/(input$FinalSel+0.000000001)-1))
			ctl.file$size_selex_parms[6,7]<- input$FinalSel_phase
		}
		#Add other fleets
		if(data.file$Nfleets>1){
			for(i in 1:(data.file$Nfleets-1))
			{
				ctl.file$init_F<-rbind(ctl.file$init_F,ctl.file$init_F[1,])
				ctl.file$size_selex_types<-rbind(ctl.file$size_selex_types,ctl.file$size_selex_types[1,])
				ctl.file$age_selex_types<-rbind(ctl.file$age_selex_types,ctl.file$age_selex_types[1,])
				ctl.file$size_selex_parms<-rbind(ctl.file$size_selex_parms,ctl.file$size_selex_parms[1:6,])
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
		#Change likelihood component weight of catch
		if (is.null(inCatch))
			{
				ctl.file$lambdas[1,4]<-0
			}
		if(!is.null(inCatch))
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

	#Run Stock Synthesis and plot output
		RUN.SS(paste0(getwd(),"/Scenarios/",input$Scenario_name), ss.exe="ss",ss.cmd="")
		Model.output<-try(SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE))
		if(class(Model.output)=="try-error")
			{
				Model.output<-SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
			}
		#Make SS plots	
		SS_plots(Model.output,maxyr=data.file$endyr,verbose=FALSE)
		#Make SS tables
		SSexecutivesummary(Model.output)		
			 
		#Run multiple jitters
		if(input$jitter_choice)
		{
	if(input$Njitter>1)
		{
			 jits<-SS_RunJitter(paste0(getwd(),"/Scenarios/",input$Scenario_name),Njitter=input$Njitter,printlikes = FALSE)
			 profilemodels <- SSgetoutput(dirvec=paste0(getwd(),"/Scenarios/",input$Scenario_name), keyvec=0:input$Njitter, getcovar=FALSE)
			 profilesummary <- SSsummarize(profilemodels)
			 minlikes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]==min(profilesummary$likelihoods[1,-length(profilesummary$likelihoods)])
			 #Find best fit model
			 index.minlikes<-c(1:length(minlikes))[minlikes]
			 file.copy(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/ss.par_",(index.minlikes[1]-1),".sso"),paste0(getwd(),"/Scenarios/",input$Scenario_name,"/ss.par"),overwrite = TRUE)
	         starter.file$init_values_src<-1
		 	 SS_writestarter(starter.file,paste0(getwd(),"/Scenarios/",input$Scenario_name),overwrite=TRUE)
		 	 #R-run to get new best fit model
			 RUN.SS(paste0(getwd(),"/Scenarios/",input$Scenario_name), ss.exe="ss",ss.cmd="")
		 	 Model.output<-SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE,covar=FALSE)
			 SS_plots(Model.output,maxyr=data.file$endyr,verbose=FALSE)
			 SSexecutivesummary(Model.output)		
			 jitter.likes<-profilesummary$likelihoods[1,-length(profilesummary$likelihoods)]
			 ref.like<-min(jitter.likes)
	    	 #Make plot and save to folder
	    	 setwd(paste0(getwd(),"/Scenarios/",input$Scenario_name))
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
		}		
	
	output$Jitterplot<-renderPlot({
		if(input$Njitter==1){return(NULL)}
		if(input$Njitter>1)
		{
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
		}
	})
	
		output$Jitterplot<-renderPlot({
			SSplotComparisons(mysummary, legendlabels = c(1:input$Njitter), ylimAdj = 1.30, subplot = c(1,3), new = FALSE)
	
			})
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
 					{converge.dec<-"Model appears to have converged. Please check outputs for nonsense."}
 				else{converge.dec<-"Model has not converged. Please use the Jitter option or change starting values before re-running model."}
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
					# Label=c(paste0("SO",input$endyr,"/SO_0"),
					# 					  "SO_MSY/SO_0",
					# 					  paste0("SPR",input$endyr),
					# 					  paste0("OFL",(input$endyr+1)),
					# 					  paste0("ABC",(input$endyr+1))
					# 					  ))
				Output_relSB_table[,1]<-c(paste0("SO",input$endyr,"/SO_0"),
										  "SO_MSY/SO_0",
										  paste0("1-SPR",input$endyr),
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
 				Output_table<-Model.output$sprseries[-nrow(Model.output$sprseries),c(1,5,6,7,8,9,11,12,13,25,37)]
			})
 		
 		#Paramters
 		
 		output$Parameters_table <- renderTable({
 				Model.output$estimated_non_dev_parameters
			})

	})


})
