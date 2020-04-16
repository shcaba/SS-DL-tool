require(r4ss)
require(ggplot2)
require(reshape2)

shinyServer(function(input, output,session) {

#################
### FUNCTIONS ###
#################

VBGF<-function(Linf,k,t0,ages)
  {
    Lengths_exp<-Linf*(1-exp(-k*(ages-t0)))
    return(Lengths_exp)
  }

 VBGF.age<-function(Linf,k,t0,lt)
  {
    ages<-t0-(log(1-(lt/Linf))/k)
    return(ages)
  }
 
RUN.SS<-function(path, ss.exe="ss",ss.cmd=" -nohess -nox")
{
  navigate <- paste("cd ",path,sep="")
  command <- paste(navigate," & ",ss.exe,ss.cmd,sep="")
  shell(command,invisible=TRUE,translate=TRUE)
} 
	
########################################	
#User activated pop-up parameter values#
########################################
#Model dimensions
output$Model_dims1<- renderUI({
			  	inFile1<- input$file1
				inFile2<- input$file2
				if (is.null(inFile1)&is.null(inFile2)) return(NULL)
				if (!is.null(inFile1)&is.null(inFile2))
					{
						Lt.comp.data<-read.csv(inFile1$datapath,check.names=FALSE)
			    		fluidRow(column(width=4,numericInput("styr", "Starting year", value=as.numeric(colnames(Lt.comp.data)[-1])[1],min=1, max=10000, step=1)),
              			column(width=4,numericInput("endyr","Ending year", value=max(as.numeric(colnames(Lt.comp.data)[-1])),min=1, max=10000, step=1)))						
					}
		})

output$Model_dims2<- renderUI({
			  	inFile1<- input$file1
				inFile2<- input$file2
				if (is.null(inFile2)) return(NULL)
				if (!is.null(inFile2))
					{
						Ct.data<-read.csv(inFile2$datapath,check.names=FALSE)
			    		fluidRow(column(width=4,numericInput("styr", "Starting year", value=Ct.data[1,1],min=1, max=10000, step=1)),
              			column(width=4,numericInput("endyr","Ending year", value=max(Ct.data[,1]),min=1, max=10000, step=1)))						
					}	
		})

#Male life history parameters
output$Male_parms_inputs_label<- renderUI({
	if(input$male_parms){
   		 h5(em("Male"))
   			}		
		})

output$Male_parms_inputs1<- renderUI({
	if(input$male_parms){
    fluidRow(column(width=6,numericInput("M_m", "Natural mortality", value=NA,min=0, max=10000, step=0.01)),
            column(width=6,numericInput("Linf_m", "Asymptotic size (Linf)", value=NA,min=0, max=10000, step=0.01)))    
		}
	})

output$Male_parms_inputs2<- renderUI({
	if(input$male_parms){
    fluidRow(column(width=6,numericInput("k_m","Growth coefficient k", value=NA,min=0, max=10000, step=0.01)),
            column(width=6,numericInput("t0_m","Age at length 0 (t0)", value=NA,min=0, max=10000, step=0.01)))    
    	}
	})

output$Male_parms_inputs3<- renderUI({
	if(input$male_parms){
    fluidRow(column(width=6,numericInput("CV_lt_m","CV at length", value=NA,min=0, max=10000, step=0.01)))
    	}
	})

#Selectivity paramters
output$Sel_parms1<- renderUI({
    fluidRow(column(width=8,numericInput("Sel50", "Length at 50% Selectivity", value=NA,min=0, max=10000, step=0.01)),
            column(width=4,numericInput("Sel50_phase","Est. phase", value=1,min=-1000, max=10, step=1)))    
	})

output$Sel_parms2<- renderUI({
    	fluidRow(column(width=8,numericInput("Selpeak", "Length at Peak Selectvity", value=NA,min=0, max=10000, step=0.01)),
            	column(width=4,numericInput("Selpeak_phase","Est. phase", value=1,min=-1000, max=10, step=1)))
	})

output$Sel_parms3<- renderUI({
  		if(input$Sel_choice=="Dome-shaped")
		{ 			
    	fluidRow(column(width=8,numericInput("PeakDesc", "Length at first declining selectivity", value=NA,min=0, max=10000, step=0.01)),
            	column(width=4,numericInput("PeakDesc_phase","Est. phase", value=1,min=-1000, max=10, step=1)))
 		}
	})


output$Sel_parms4<- renderUI({
 		if(input$Sel_choice=="Dome-shaped")
 		{ 			
	    fluidRow(column(width=8,numericInput("LtPeakFinal", "Width of declining selectivity", value=NA,min=0, max=10000, step=0.01)),
	            column(width=4,numericInput("LtPeakFinal_phase","Est. phase", value=1,min=-1000, max=10, step=1)))    			
 		}
	})

output$Sel_parms5<- renderUI({
 		if(input$Sel_choice=="Dome-shaped")
 		{ 			
    	fluidRow(column(width=8,numericInput("FinalSel", "Selectivity at max bin size", value=NA,min=0, max=1, step=0.01)),
            	column(width=4,numericInput("FinalSel_phase","Est. phase", value=1,min=-1000, max=10, step=1)))
 		}
	})

			
#Recruitment parameter inputs
output$Rec_options1<- renderUI({
    if(input$rec_choice){
        fluidRow(column(width=6,numericInput("sigmaR","Rec. varaibility (sR)", value=0.5,min=0, max=10, step=0.01)))   
    	}
	})
output$Rec_options2<- renderUI({
    if(input$rec_choice){
          fluidRow(column(width=6,numericInput("Rdev_startyr","Rec. devs. start year", value=input$styr,min=1, max=10000, step=1)),
             column(width=6,numericInput("Rdev_endyr","Rec. devs. end year", value=input$endyr,min=1, max=10000, step=1)))    
    	}
	})
#Jitter value
output$Jitter_value<- renderUI({
    if(input$jitter_choice){
        fluidRow(column(width=6,numericInput("jitter_fraction","Jitter value", value=0,min=0, max=10, step=0.001)))   
    	}
	})

#############
### PLOTS ###
#############
#Plot length compoistions

	output$Ltplot<-renderPlot({
		inFile<- input$file1
		if (is.null(inFile)) return(NULL)
		Lt.comp.data<-read.csv(inFile$datapath,check.names=FALSE)
		dat.gg<-cbind(Lt.comp.data[,1],melt(Lt.comp.data[,-1]))
		if(ncol(dat.gg)==2)
			{
				dat.gg<-data.frame(dat.gg[,1],as.numeric(colnames(Lt.comp.data)[2]),dat.gg[,2])				
			}
		colnames(dat.gg)<-c("bin","year","ltnum")
		ggplot(dat.gg,aes(bin,ltnum))+
					geom_col(fill="#236192",color="white")+
					facet_wrap(~year)+
					xlab("Length bin")+
					ylab("Frequency")			
				#}
		# if(ncol(dat.gg)==2){
		# 	colnames(dat.gg)<-c("bin","ltnum")
		# 	ggplot(dat.gg,aes(bin,ltnum))+
		# 	geom_col(fill="#236192",color="white")+
		# 	xlab("Length bin")+
		# 	ylab("Frequency")
		# 	}
		})

output$Ctplot<-renderPlot({
		inCatch<- input$file2
		if (is.null(inCatch)) return(NULL)
		Catch.data<-read.csv(inCatch$datapath,header=TRUE)
		ggplot(Catch.data,aes(get(colnames(Catch.data)[1]),get(colnames(Catch.data)[2])))+
			geom_col(fill="#658D1B",color="white")+
			xlab("Year")+
			ylab("Removals")				
		})

output$Mplot<-renderPlot({
			mf.in<-input$M_f
			mm.in<-input$M_f
			if(input$male_parms)
			{mm.in<-input$M_m}		
			if(any(is.na(c(mf.in,mm.in)))) return(NULL)
			Female_M<-data.frame(Ages=c(0:input$Nages),PopN=exp(-mf.in*c(0:input$Nages)),Sex="Female")
			Male_M<-data.frame(Ages=c(0:input$Nages),PopN=exp(-mm.in*c(0:input$Nages)),Sex="Male")
			M_sexes<-rbind(Female_M,Male_M)
			ggplot(M_sexes,aes(Ages,PopN,color=Sex))+
					geom_line(aes(linetype=Sex),lwd=2)+
					ylab("Cohort decline by M")
		})

output$VBGFplot<-renderPlot({
   	f_Linf<-m_Linf<-input$Linf_f
   	f_k<-m_k<-input$k_f
   	f_t0<-m_t0<-input$t0_f
	f_L50<-input$L50_f
	f_L95<-input$L95_f
	maxage<-input$Nages
	if(input$male_parms)
			{
				m_Linf<-input$Linf_m
			   	m_k<-input$k_m
			   	m_t0<-input$t0_m
			}		
   if(any(is.na(c(f_Linf,f_k,f_t0)))=="FALSE")
    {
		vbgf_female<-data.frame(Age=c(0:input$Nages),Length=VBGF(f_Linf,f_k,f_t0,c(0:input$Nages)), Sex="Female")
      	vbgf_male<-data.frame(Age=c(0:input$Nages),Length=VBGF(m_Linf,m_k,f_t0,c(0:input$Nages)),Sex="Male")
      	sexes_vbgf<-rbind(vbgf_female,vbgf_male)
      	vbgf.plot<-ggplot(sexes_vbgf,aes(Age,Length, color=Sex))+
      				geom_line(aes(linetype=Sex),lwd=2)
      	#theme(axis.title = element_text(size=14))
      	#annotate("text",x=0.9*max(dlm_input.vbgf$Age),y=c(0.25*max(dlm_input.vbgf$Length),0.15*max(dlm_input.vbgf$Length),0.05*max(dlm_input.vbgf$Length)),label=c(paste0("Linf=",round(dlm_input@vbLinf,2)),paste0("k=",round(dlm_input@vbK,2)),paste0("t0=",round(dlm_input@vbt0,2))),ymin=0,size=5)
      if(any(is.na(c(f_L50,f_L95)))=="FALSE")
       {
        age.mat<-data.frame(Age=VBGF.age(f_Linf,f_k,f_t0,c(f_L50,f_L95)),Length=c(f_L50,f_L95),Sex="Female")
        vbgf.plot<-
        	vbgf.plot+
        	geom_point(data=age.mat,aes(Age,Length), color = "darkorange",size=6) +
        	geom_text(data=age.mat,label=c("Lmat50%","Lmat95%"),nudge_x = -0.1*input$Nages,color="black")
       }
  	 vbgf.plot
  	 }
	})


#############################################
### PREPARE FILES andD RUN Stock Synthsis ###
#############################################

SS.file.update<-observeEvent(input$run_SS,{
		# if(is.null(inFile) | !anyNA(input$styr,
		# 							input$endyr,
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
	  	file.copy(paste0(getwd(),"/SS_LB_files"),paste0(getwd(),"/Scenarios"),recursive=TRUE)
		file.rename(paste0(getwd(),"/Scenarios/SS_LB_files"), paste0(getwd(),"/Scenarios/",input$Scenario_name))
		if(file.exists(paste0(getwd(),"/Scenarios/SS_LB_files")))
		{file.remove(paste0(getwd(),"/Scenarios/SS_LB_files"))}

	#Read, edit then write new DATA file
		data.file<-SS_readdat(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.dat")) 
		data.file$styr<-input$styr
		data.file$endyr<-input$endyr
		data.file$Nages<-input$Nages
		
	#Catches
		inCatch<- input$file2
		if (is.null(inCatch)) 
		{
		year.in<-input$styr:input$endyr
		catch.cols<-colnames(data.file$catch)
		data.file$catch<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(1,length(year.in)+1),
						c(100,rep(100,length(year.in))),
						c(0.01,rep(1000,length(year.in)))
						)
		colnames(data.file$catch)<-catch.cols
		}

		if(!is.null(inCatch))
		{
		Catch.data<-read.csv(inCatch$datapath,header=TRUE)
		year.in<-Catch.data[,1]
		catch.cols<-colnames(data.file$catch)
		data.file$catch<-data.frame(
						c(-999,year.in),
						rep(1,length(year.in)+1),
						rep(1,length(year.in)+1),
						c(0,Catch.data[,2]),
						rep(0.01,length(year.in)+1)
						)
		colnames(data.file$catch)<-catch.cols
		}

	#Length composition data
		inFile<- input$file1
		if (is.null(inFile)) return(NULL)
		Lt.comp.data<-read.csv(inFile$datapath,check.names=FALSE)
		data.file$N_lbins<-nrow(Lt.comp.data)
		data.file$lbin_vector<-Lt.comp.data[,1]
		samp.yrs<-as.numeric(colnames(Lt.comp.data)[-1])
		lt.data.names<-c(colnames(data.file$lencomp[,1:6]),paste0("f",Lt.comp.data[,1]),paste0("m",Lt.comp.data[,1]))
		if(length(samp.yrs)==1){
			data.file$lencomp<-data.frame(matrix(c(samp.yrs,
			rep(1,length(samp.yrs)),
			rep(1,length(samp.yrs)),
			rep(1,length(samp.yrs)),
			rep(0,length(samp.yrs)),
			colSums(Lt.comp.data[-1]),
			t(Lt.comp.data)[-1,],
			t(Lt.comp.data)[-1,]*0),
			nrow=length(samp.yrs),
			ncol=6+length(Lt.comp.data[,1])*2,
			byrow=FALSE))[,,drop=FALSE]
		}
		else{
		data.file$lencomp<-data.frame(matrix(cbind(samp.yrs,
			rep(1,length(samp.yrs)),
			rep(1,length(samp.yrs)),
			rep(1,length(samp.yrs)),
			rep(0,length(samp.yrs)),
			colSums(Lt.comp.data[-1]),
			t(Lt.comp.data)[-1,],
			t(Lt.comp.data)[-1,]*0),
			nrow=length(samp.yrs),
			ncol=6+length(Lt.comp.data[,1])*2,
			byrow=FALSE))[,,drop=FALSE]			
		}
		colnames(data.file$lencomp)<-lt.data.names
		data.file$N_agebins<-input$Nages
		data.file$agebin_vector<-1:input$Nages
		SS_writedat(data.file,paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.dat"),overwrite=TRUE)			

		#Read, edit then write new CONTROL file
		ctl.file<-SS_readctl(paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.ctl"),use_datlist = TRUE, datlist=data.file) 
		
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
		
		if(input$rec_choice)
			{
				ctl.file$SR_parms[3,3:4]<-input$sigmaR 			#sigma R
				ctl.file$do_recdev<-1
				ctl.file$MainRdevYrFirst<-input$Rdev_startyr	#Start year of recruitment estimation
				ctl.file$MainRdevYrLast<-input$Rdev_endyr		#Last year of recruitment estimation
				ctl.file$recdev_phase<- 1
			}
		
		#Selectivity
		ctl.file$age_selex_type<-10
		if(input$Sel_choice=="Logistic")
		{
			ctl.file$size_selex_parms[3,3:4]<- log(-((input$Sel50-input$Selpeak)^2/log(0.5)))
			ctl.file$size_selex_parms[3,7]<- input$Sel50_phase
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

		#Change likelihood component weight of catch
		if (is.null(inCatch)){ctl.file$lambdas[1,4]<-0}
		if(!is.null(inCatch)){ctl.file$lambdas[1,4]<-1}

		SS_writectl(ctl.file,paste0(getwd(),"/Scenarios/",input$Scenario_name,"/SS_LB.ctl"),overwrite=TRUE)

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
		Model.output<-SS_output(paste0(getwd(),"/Scenarios/",input$Scenario_name),verbose=FALSE,printstats = FALSE)
		SS_plots(Model.output,verbose=FALSE)
		
 		output$SSout_table <- renderTable({
 				Output_table<-Model.output$sprseries[-nrow(Model.output$sprseries),c(1,5,6,7,8,9,11,12,13,25,37)]
 			})
		# }
	})






})
