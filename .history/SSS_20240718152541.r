##' This file contails all the SSS code 
##' @param filepath location where the SSS will look for model files and run the executable
##' @param file.name vector of file names for the data and control file where the expected input is c("data file", "control file") 
##' @param reps number of random draws to perform
##' @param seed.in seed number to fix where random sample are drawn.
##' @param Dep.in vector defining distribution, mean, sd, and bounds for depletion prior.  Expected input is c(distribution shape, mean, sd). The distribution options are 2 = 1 - beta, 4 = uniform, 10 = truncated normal.
##' @param M.in vector defining natural mortality distribuition, mean, and . Expected input is c(distrbution shape for females, mean for females, sd for females, distribution shape for males, mean for males, sd for males). The distibution options are 0 = normal, 3 = lognormal, and 4 = uniform.
##' @param SR_type The shape of the stock-recruitment curve. Options are based on SS stock-recruit options. Option 3 = Beverton-holt, 8 = Shepherd 3-parameter, 9 = Ricker 3-parameter
##' @param h.in vector defining the steepness distribution, mean, and sd. Expected input is c(distribution, mean, sd). Distribution options are 1 = truncated beta, 2 = beta, 10 = truncated normal, 30 = truncated lognormal, 4 = uniform.
##' @param FMSY_M.in vector defining the Fmsy/M ratio distribution, mean, and sd. Expected input is c(distribution, mean, sd). Distribution options are; negative value = ?, 2 = truncated beta, 10 = truncated normal, 30 = truncated lognormal, 4 = uniform.
##' @param BMSY_B0.in vector defining the Bmsy/B0 ratio distribution, mean, and sd. Expected input is c(distribution, mean, sd). Distribution options are; negative value = ?, 2 = truncated beta, 10 = truncated normal, 30 = truncated lognormal, 4 = uniform.
##' @param L1.in vector defining the minimum length for a given age. Setting this to 0 in the control file makes it the VBGF t0. A vector of zeros will not draw values for this value. Expected input values are c(female mean, female sd, male mean, male sd)
##' @param Linf.k.cor input defining the correlation between Linf and k when using the multivariate normal distribution
##' @param Linf.in vector defining the maximum length. This is an optional feature. Expected input values are c(prior type (only -1 and 1 (normal) options),female mean, female sd, prior type (only -1 and 1 (normal) options),male mean, male sd)
##' @param k.in vector defining the growth coefficient k. This is an optional feature. Expected input values are c(prior type (only -1 and 1 (normal) options), female mean, female sd, prior type(only -1 and 1 (normal) options),male mean, male sd)
##' @param t0.in vector defining the t0 parameter to calculate the L1 paremter for SS. Expected input values are c(prior type (only -1 and 1 (normal) options), female mean, female sd, prior type (only -1 and 1 (normal) options),male mean, male sd)
##' @param Zfrac.Beta.in is the Zfrac beta for stock recruit function 7 in Stock Synthesis (Survivorship function). Sometimes used with elasmobranchs. Inputs are prior type (- values skips the draws) and prior type inputs.
##' @param R_start vector allowing the user to control the starting R0 value in the control file. Expected value is c( switch option, input value) where the switch optionas are 1= draw from a random draw from a truncated lognormal distribution based on the input value and 0 = start from the input value.
##' @param doR0.loop allows for a profile over initial R0 values in order to find a converged model. It will stop the profile once a converged model is found. Inputs are feature on/off, staring profile value, ending profile, profile step. A 0 for the first value means you will only consider models that start at the given intial R0. Highly recommended to keep this as TRUE, though it can take more computational time.  
##' @param sum_age summary age for total biomass calculation used by SS
##' @param ts_yrs start and end years of model year
##' @param ofl_yrs the years for which OFL values should be calculated
##' @param sexes TRUE/FALSE allows for the user to specify whether or not sexeses should have the same values from drawn parameters (e.g., natural mortality, L1, Linf)
##' @param BH_FMSY_comp
##' @param OStype distinguishes operating system being used. "Windows" or "OSX_Linux"
##' @author Jason Cope and Chantel Wetzel
##' @export
##' @seealso \code{\link{Opt_s_prof}}, \code{\link{Change_SSfiles}}, \code{\link{rbeta}}, \code{\link{rbeta_solve}}, \code{\link{rtlnorm}}, \code{\link{Run_SS}}, \code{\link{loadnnames}},
##' @import msm
##' @import EnvStats
##' @import r4ss

SSS<-function(filepath,
              file.name,
              reps=1000,
              seed.in=19,
              Dep.in=c(2,0.4,0.1),
              M.in=c(3,0.1,0.4,3,0.1,0.4),
              SR_type=3,
              h.in=c(1,0.6,0.2),
              FMSY_M.in=c(-1,0.5,0.1),
              BMSY_B0.in=c(-1,0.5,0.1),
              Linf.k.cor=-0.9,
              Linf.in=c(-1,0,0,-1,0,0),
              k.in=c(-1,0,0,-1,0,0),
              t0.in=c(-1,0,0,-1,0,0),
              Zfrac.Beta.in=c(-99,0.2,0.6,-99,0.5,2),
              R_start=c(0,8),
              doR0.loop=c(1,4.1,12.1,0.5),
              sum_age=0,
              ts_yrs=NA,
              pop.ltbins=NA,
              sexes=F,
              BH_FMSY_comp=F,
              OStype="Windows")
{

  require(msm)
  require(EnvStats)
  require(r4ss)
  require(tmvtnorm)

  # VBGF<-function(Linf, k, t0, ages){ 
  #  Linf * (1 - exp(-k * (ages - t0))) 
  # } 
  
  # VBGF.age<-function(Linf,k,t0,lt){ 
  #   t0 - (log(1 - (lt / Linf)) / k) 
  # } 

# RUN.SS<-function(path,ss.cmd=" -nohess -nox",OS.in="Windows"){ 
#   navigate <- paste("cd ", path, sep="") 
# if(OS.in=="Windows") 
#   {
#     #command <- paste0(navigate," & ", "ss", ss.cmd) 
#     #shell(command, invisible=TRUE, translate=TRUE)
#     run(path,exe="ss3",extras=ss.cmd,skipfinished=FALSE)
#   } 
# if(OS.in=="Mac")  
#   {
    
#     command <- c(paste("cd", path), "chmod +x ./ss3_osx",paste("./ss3_osx", ss.cmd)) 
#     system(paste(command, collapse=";"),invisible=TRUE)
    
#     #command <- paste0(path,"/./ss_mac", ss.cmd) 
#     #system(command, invisible=TRUE)
#   } 
# if(OS.in=="Linux") 
#   {
#     command <- c(paste("cd", path), "chmod +x ./ss3_linux",paste("./ss3_linux", ss.cmd)) 
#     system(paste(command, collapse=";"), invisible=TRUE)
#   }   
# }  

  set.seed(seed.in)
  start.time<-Sys.time()
  SSS.output.list<-list()
  Spp.quant.out<-list()
  OFL.out<-ABC.out<-list()
  Input.draws<-as.data.frame(matrix(NA,nrow=reps,ncol=10))
  if(SR_type==7){Input.draws<-as.data.frame(matrix(NA,nrow=reps,ncol=11))}
  if(SR_type>=8 | h.in[1]==99 | BH_FMSY_comp==T)
  {
    SR_expo.out<-SRMQ_par<-rep(NA,reps)
    names(SR_expo.out)<-"gRicker_Beta"
    Input.draws.MQs<-as.data.frame(matrix(NA,nrow=reps,ncol=3))
    colnames(Input.draws.MQs)<-c("FMSY/M","BMSY/B0","FMSY")
  }
  sb.years<-c(ts_yrs[1]:ts_yrs[2])
  Quant.out<-as.data.frame(matrix(NA,nrow=reps,ncol=29))
  Quant.out.bad<-as.data.frame(matrix(NA,1,ncol=29))
  
#Input the file names and the summary age into the starter file
  starter.new <- SS_readstarter(file.path(filepath, 'starter.ss'),verbose=FALSE)
  starter.new$datfile <- file.name[1]
  starter.new$ctlfile <- file.name[2]
  starter.new$min_age_summary_bio <- sum_age
  SS_writestarter(starter.new, filepath, overwrite=TRUE,verbose=FALSE)

#Input the starting and ending years of the model
  if(all(!is.na(ts_yrs)))
  {
   #   dat.new<-readLines(paste(filepath,"/",file.name[1],sep=""))
    #   Styr.line<-strsplit(dat.new[grep("styr",dat.new)], " ")[[1]]
    #   Styr.line[1]<-ts_yrs[1]
    #   Endyr.line<-strsplit(dat.new[grep("endyr",dat.new)], " ")[[1]]
    #   Endyr.line[1]<-ts_yrs[2]
    #   dat.new[grep("styr",dat.new)]<-paste(Styr.line,collapse=" ")
    #   dat.new[grep("endyr",dat.new)]<-paste(Endyr.line,collapse=" ")
    #   write(dat.new,paste(filepath,"/",file.name[1],sep=""))
    dat.yrs <- SS_readdat(file.path(filepath, file.name[1]),verbose=FALSE)
    dat.yrs$styr<-ts_yrs[1]
    dat.yrs$endyr<-ts_yrs[2]
    SS_writedat(dat.yrs, file.path(filepath, file.name[1]), overwrite=TRUE,verbose=FALSE)    
  }
  
  # starter.new<-readLines(paste(filepath,"/starter.ss",sep=""))
  # sum_age_line<-strsplit(starter.new[grep("summary biomass",starter.new)], " ")[[1]]
  # sum_age_line[1]<-sum_age
  # starter.new[grep("summary biomass",starter.new)]<-paste(sum_age_line, collapse=" ")
  # write(starter.new,paste(filepath,"/starter.ss",sep=""))

  i<-1
  ii<-1
  xxx<-1
  n.bad<-0
  while(i<reps+1)
  {
    print(paste0("Attempt ",ii))
    if(ii%%10==0){print(file.name[1])}
    if(file.exists(paste(filepath,"/Report.sso",sep=""))){file.remove(paste(filepath,"/Report.sso",sep=""))}
    ### Draw random values ###
    #- values = no draws taken; one value fixed in the model
    #0 = normal
    #10 = truncated normal
    #1 = symmetric beta 
    #2 = beta (rbeta)
    #3 = lognormal
    #30 = truncated lognormal
    #4 = uniform
    #99 = used only for the steepness parameter. Indicates h will come from FMSY/M prior
    # dat.new<-readLines(paste(filepath,"/",file.name[1],sep=""))
    # ctl.new<-readLines(paste(filepath,"/",file.name[2],sep=""))
    dat.new<-SS_readdat(file.path(filepath, file.name[1]),verbose=FALSE)
    ctl.new<-SS_readctl(file.path(filepath, file.name[2]),use_datlist = TRUE, datlist=dat.new,verbose=FALSE)
    
    if(length(Dep.in)==3)
    {
      if(Dep.in[1]<0){Dep.draw<-Dep.in[2]}
      if(Dep.in[1]==2){Dep.draw<-round(1-rbeta.ab(1,1-Dep.in[2],Dep.in[3],0.05,0.95),2)}
      if(Dep.in[1]==3){Dep.draw<-round(rlnorm(1,log(Dep.in[2]),Dep.in[3]),2)}
      if(Dep.in[1]==4){Dep.draw<-round(runif(1,Dep.in[2],Dep.in[3]),2)}
      if(Dep.in[1]==10){Dep.draw<-round(rtnorm(1,Dep.in[2],Dep.in[3],0.01,1),2)}
      Input.draws[i,2]<-Dep.draw
    }
    if(length(Dep.in)>3)
    {
      Input.draws[i,2]<-Dep.draw<-Dep.in[i]    
    }
    
    
    #Natural mortality
    if(FMSY_M.in[1]<0 & BMSY_B0.in[1]<0)
    {
      if(M.in[1]>=0 & length(M.in)<=6)
        {
          M.draw<-0
          while(M.draw<=0)
          {
          if(M.in[1]<0){M.draw<-M.in[2]}
          if(M.in[1]==0){M.draw<-round(rnorm(1,M.in[2],M.in[3]),3)}
          if(M.in[1]==3){M.draw<-round(rlnorm(1,log(M.in[2]),M.in[3]),3)}
          if(M.in[1]==4){M.draw<-round(runif(1,M.in[2],M.in[3]),3)}
          Input.draws[i,3]<-M.draw
          }
        }
      else{Input.draws[i,3]<-M.draw<-M.in[i,1]}

      if(sexes==T)
      {
        if(length(M.in)==6)
        {
          M.draw.M<--1
          while(M.draw.M<0)
          {
          if(M.in[4]<0){M.draw.M<-M.in[5]}
          if(M.in[4]==0){M.draw.M<-round(rnorm(1,M.in[5],M.in[6]),3)}
          if(M.in[4]==3){M.draw.M<-round(rlnorm(1,log(M.in[5]),M.in[6]),3)}
          if(M.in[4]==4){M.draw.M<-round(runif(1,M.in[5],M.in[6]),3)}
          Input.draws[i,7]<-M.draw.M
        }
        }        
        if(length(M.in)>6){Input.draws[i,7]<-M.draw.M<-M.in[i,2]}
      }
    }
    #Growth parameters
#    if(Linf.in[1]>-1&k.in[1]>-1)
#    {
      Covar_Linf_k<-Linf.k.cor*((Linf.in[3]+0.000001)*(k.in[3]+0.000001)) #caluclate covariance matrix for multivariate sampling
      Linf_k_sigma <- matrix(c((Linf.in[3]+0.000001)^2,Covar_Linf_k,Covar_Linf_k,(k.in[3]+0.000001)^2),2,2) #Set-up the variance-covariance matrix for multivariate sampling
      Linf_k_samps<-rtmvnorm(1,c(Linf.in[2],k.in[2]),Linf_k_sigma,lower=rep(0,length(c(Linf.in[2],k.in[2]))))
      Linf.draw<-Input.draws[i,5]<-Linf_k_samps[1]
      k.draw<-Input.draws[i,6]<-Linf_k_samps[2]
      #if(sum(L1.in[1:2])>0){L1.draw<-round(rnorm(1,L1.in[2],L1.in[3]),2); Input.draws[i,4]<-L1.draw}
      t0.draw<-round(rnorm(1,t0.in[2],t0.in[3]),3)
      L1.draw<-Input.draws[i,4]<-Input.draws[i,8]<-0
    
#    }  

#     if(any(Linf.in[1]<1&k.in[1]<1)&!all(Linf.in[1]<1&k.in[1]<1))
#     {
# #      if(sum(L1.in[1:2])>0){L1.draw<-round(rnorm(1,L1.in[2],L1.in[3]),2); Input.draws[i,4]<-L1.draw}
#       Linf.draw<-Input.draws[i,5]<-round(rnorm(1,Linf.in[2],Linf.in[3]),2)
#       k.draw<-Input.draws[i,6]<-round(rnorm(1,k.in[2],k.in[3]),2)
#       t0.draw<-round(rnorm(1,t0.in[2],t0.in[3]),3)
#       L1.draw<-Input.draws[i,4]<-VBGF(Linf.draw,k.draw,t0.draw,0)
#    }

    #Male draws
    if(sexes==T)
    {
      # if(sum(L1.in[3:4])>0) { 
      #   L1.draw.M <- round(rnorm(1,L1.in[3], L1.in[4]), 2)
      # }

#        if(Linf.in[4]>-1&k.in[4]>-1)
 #         {
      if(ctl.new$parameter_offset_approach==1)
      {
            Covar_Linf_k<-Linf.k.cor*((Linf.in[6]+0.000001)*(k.in[6]+0.000001)) #caluclate covariance matrix for multivariate sampling
            Linf_k_sigma <- matrix(c((Linf.in[6]+0.000001)^2,Covar_Linf_k,Covar_Linf_k,(k.in[6]+0.000001)^2),2,2) #Set-up the variance-covariance matrix for multivariate sampling
            Linf_k_samps<-rtmvnorm(1,c(Linf.in[5],k.in[5]),Linf_k_sigma,lower=rep(0,length(c(Linf.in[5],k.in[5]))))
            Linf.draw.M<-Input.draws[i,9]<-Linf_k_samps[1]
            k.draw.M<-Input.draws[i,10]<-Linf_k_samps[2]
              #if(sum(L1.in[1:2])>0){L1.draw<-round(rnorm(1,L1.in[2],L1.in[3]),2); Input.draws[i,4]<-L1.draw}
              
            t0.draw.M<-round(rnorm(1,t0.in[5],t0.in[6]),3)
            L1.draw.M<-Input.draws[i,8]<-VBGF(Linf.draw.M,k.draw.M,t0.draw.M,t0.draw)
  #        }  
      }

      if(ctl.new$parameter_offset_approach==2)
      {
              Linf.draw.M<-Input.draws[i,9]<-Linf.in[5]
              k.draw.M<-Input.draws[i,10]<-k.in[5]
              #if(sum(L1.in[1:2])>0){L1.draw<-round(rnorm(1,L1.in[2],L1.in[3]),2); Input.draws[i,4]<-L1.draw}
              
              t0.draw.M<-round(rnorm(1,t0.in[5],t0.in[6]),3)
              L1.draw.M<-Input.draws[i,8]<-VBGF(Linf.draw.M,k.draw.M,t0.draw.M,t0.draw)
      } 
                              #Change to offset approach

      #    if(any(Linf.in[4]<1&k.in[4]<1)&!all(Linf.in[4]<1&k.in[4]<1))
      #    {
      #      if(sum(L1.in[1:2])>0){L1.draw<-round(rnorm(1,L1.in[2],L1.in[3]),2); Input.draws[i,4]<-L1.draw}
      #      Linf.draw.M<-Input.draws.M[i,3]<-round(rnorm(1,Linf.in[5],Linf.in[6]),2)
      #      k.draw.M<-Input.draws.M[i,4]<-round(rnorm(1,k.in[5],k.in[6]),2)
      #      t0.draw>M<-round(rnorm(1,t0.in[5],t0.in[6]),3)
      #      L1.draw>M<-Input.draws.M[i,2]<-VBGF(Linf.draw.M,k.draw.M,t0.draw.M,0)
      #   }

    }
    
    #Steenpess
    if(h.in[1]>=0 & length(h.in)==3 & FMSY_M.in[1]<0 & BMSY_B0.in[1]<0)
    {
      if(h.in[1]==1){h.draw<-round(rbeta.ab(1,h.in[2],h.in[3],0.21,0.99),3)}
      if(h.in[1]==2){h.draw<-round(rbeta(1,h.in[2],h.in[3]),3)}
      if(h.in[1]==10){h.draw<-round(rtnorm(1,h.in[2],h.in[3],0.21,0.99),3)}
      if(h.in[1]==30){h.draw<-round(rlnormTrunc(1,log(h.in[2]),h.in[3],0.21,0.99),2)}
      if(h.in[1]==4){h.draw<-round(runif(1,h.in[2],h.in[3]),2)}
      Input.draws[i,1]<-h.draw
    }
    if(length(h.in)>3 & FMSY_M.in[1]<0 & BMSY_B0.in[1]<0){Input.draws[i,1]<-h.draw<-h.in[i]}
    #Get steepness for BH from FMSY
    if(h.in[1]==99 | BH_FMSY_comp==T & FMSY_M.in[1]>=0)
        {
      SRdat.new<-readLines(paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
      x<-1
      while(x==1)
      {
      #Draw Ms
        if(M.in[1]>=0 & length(M.in)==6)
        {
          if(M.in[1]<0){M.draw<-M.in[2]}
          if(M.in[1]==0){M.draw<-round(rnorm(1,M.in[2],M.in[3]),3)}
          if(M.in[1]==3){M.draw<-round(rlnorm(1,log(M.in[2]),M.in[3]),3)}
          if(M.in[1]==4){M.draw<-round(rlnorm(1,log(M.in[2]),M.in[3]),3)}
          Input.draws[i,3]<-M.draw
        }
        else{Input.draws[i,3]<-M.draw<-M.in[i,1]}
        #Male draws
        if(sexes==T)
          {
            if(length(M.in)==6)
            {
              if(M.in[4]<0){M.draw.M<-M.in[5]}
              if(M.in[4]==0){M.draw.M<-round(rnorm(1,M.in[5],M.in[6]),3)}
              if(M.in[4]==3){M.draw.M<-round(rlnorm(1,log(M.in[5]),M.in[6]),3)}
              if(M.in[4]==4){M.draw.M<-round(runif(1,M.in[5],M.in[6]),3)}
              Input.draws[i,7]<-M.draw.M
            }
            if(length(M.in)>6){Input.draws[i,7]<-M.draw.M<-M.in[i,2]}
           }
        
        if(FMSY_M.in[1]==2){FMSY_M.draw<-round(rbeta.ab(1,FMSY_M.in[2],FMSY_M.in[3],0.0001,10),3)}
        if(FMSY_M.in[1]==10){FMSY_M.draw<-round(rtnorm(1,FMSY_M.in[2],FMSY_M.in[3],0.0001,10),3)}
        if(FMSY_M.in[1]==30){FMSY_M.draw<-round(rlnormTrunc(1,log(FMSY_M.in[2]),FMSY_M.in[3],0.0001,10),3)}
        if(FMSY_M.in[1]==4){FMSY_M.draw<-round(runif(1,FMSY_M.in[2],FMSY_M.in[3]),3)}
        if(length(FMSY_M.in)>3){FMSY_M.draw<-FMSY_M.in[i]}
        Input.draws.MQs[i,1]<-FMSY_M.draw

        if(sexes==F)
          {
            SRdat.new[5]<-paste(as.character(M.draw),as.character(M.draw),sep=" ")
            M_FMSY<-M.draw
          }
        if(sexes==T)
          {
            SRdat.new[5]<-paste(as.character(M.draw),as.character(M.draw.M),sep=" ")
            M_FMSY<-mean(c(M.draw,M.draw.M))
          }
        SRdat.new[17]<-as.character(FMSY_M.draw*M_FMSY)
        Input.draws.MQs[i,3]<-FMSY_M.draw*M_FMSY
        SRdat.new[21]<-"-0.51"
        write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
        RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
        if(file.exists(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))==TRUE)
        {
        
        SRMQ_rep.new<-readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
        SRMQ_par[i]<-as.numeric(strsplit(readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep="")),split=" ")[[1]][11])
        steep_expo.out<-as.numeric(strsplit(SRMQ_rep.new[1]," ")[[1]])
        if(steep_expo.out[1]<0.25|steep_expo.out[1]>=1|steep_expo.out[2]>10|SRMQ_par[i]>0.1)
          {
            SRdat.new[21]<-"-0.1"
            write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
            RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
            if(steep_expo.out[1]<0.25|steep_expo.out[1]>=1|steep_expo.out[2]>10|SRMQ_par[i]>0.1)
            {
            SRdat.new[21]<-"-1.51"
            write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
            RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
            }
            if(steep_expo.out[1]<0.25|steep_expo.out[1]>=1|steep_expo.out[2]>10|SRMQ_par[i]>0.1)
            {
            SRdat.new[21]<-"-0.51"
            SRdat.new[23]<-"-0.31"
            write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
            RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
            }
          }
        if(steep_expo.out[1]>=0.25&steep_expo.out[1]<1&steep_expo.out[2]<=10&SRMQ_par[i]<0.1){x<-2}          
        }
      }
      Input.draws[i,1]<-h.draw<-steep_expo.out[1]
      SR_expo.out[i]<-steep_expo.out[2]
      file.remove(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
      file.remove(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep=""))
    }
    

    if(FMSY_M.in[1]>=0 & BMSY_B0.in[1]>=0)
    {
      SRdat.new<-readLines(paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
      #SRdat.new.mod<-strsplit(SRdat.new[grep("Amax",SRdat.new)+1][1]," ")[[1]]
      #Amax.line<-strsplit(dat.new[grep("Number of ages",dat.new)], " ")[[1]]
      #k.line<-strsplit(ctl.new[grep("VonBert_K_Fem_GP_1",ctl.new)]," ")[[1]]
      #ltwt.exp.line<-strsplit(ctl.new[grep("Wtlen_2_Fem",ctl.new)]," ")[[1]]
      x<-1
      while(x==1)
      {
      #Draw Ms
        if(M.in[1]>=0 & length(M.in)==6)
        {
          if(M.in[1]<0){M.draw<-M.in[2]}
          if(M.in[1]==0){M.draw<-round(rnorm(1,M.in[2],M.in[3]),3)}
          if(M.in[1]==3){M.draw<-round(rlnorm(1,log(M.in[2]),M.in[3]),3)}
          if(M.in[1]==4){M.draw<-round(runif(1,M.in[2],M.in[3]),3)}
          Input.draws[i,3]<-M.draw
        }
        else{Input.draws[i,3]<-M.draw<-M.in[i,1]}
        #Male draws
        if(sexes==T)
          {
            if(length(M.in)==6)
            {
              if(M.in[4]<0){M.draw.M<-M.in[5]}
              if(M.in[4]==0){M.draw.M<-round(rnorm(1,M.in[5],M.in[6]),3)}
              if(M.in[4]==3){M.draw.M<-round(rlnorm(1,log(M.in[5]),M.in[6]),3)}
              if(M.in[4]==4){M.draw.M<-round(runif(1,M.in[5],M.in[6]),3)}
              Input.draws[i,7]<-M.draw.M
            }
            if(length(M.in)>6){Input.draws[i,7]<-M.draw.M<-M.in[i,2]}
           }
        
        if(FMSY_M.in[1]==2){FMSY_M.draw<-round(rbeta.ab(1,FMSY_M.in[2],FMSY_M.in[3],0.0001,10),3)}
        if(FMSY_M.in[1]==10){FMSY_M.draw<-round(rtnorm(1,FMSY_M.in[2],FMSY_M.in[3],0.0001,10),3)}
        if(FMSY_M.in[1]==30){FMSY_M.draw<-round(rlnormTrunc(1,log(FMSY_M.in[2]),FMSY_M.in[3],0.0001,10),3)}
        if(FMSY_M.in[1]==4){FMSY_M.draw<-round(runif(1,FMSY_M.in[2],FMSY_M.in[3]),3)}
        if(length(FMSY_M.in)>3){FMSY_M.draw<-FMSY_M.in[i]}
        if(BMSY_B0.in[1]==2){BMSY_B0.draw<-round(rbeta.ab(1,BMSY_B0.in[2],BMSY_B0.in[3],0.01,0.99),2)*100}
        if(BMSY_B0.in[1]==10){BMSY_B0.draw<-round(rtnorm(1,BMSY_B0.in[2],BMSY_B0.in[3],0.01,0.99),2)*100}
        if(BMSY_B0.in[1]==30){BMSY_B0.draw<-round(rlnormTrunc(1,log(BMSY_B0.in[2]),BMSY_B0.in[3],0.01,0.99),2)*100}
        if(BMSY_B0.in[1]==4){BMSY_B0.draw<-round(runif(1,BMSY_B0.in[2],BMSY_B0.in[3]),2)*100}
        if(length(BMSY_B0.in)>3){BMSY_B0.draw<-BMSY_B0.in[i]}
        Input.draws.MQs[i,1]<-FMSY_M.draw
        Input.draws.MQs[i,2]<-BMSY_B0.draw
        #SRdat.new.mod[1]<-as.numeric(Amax.line[1])
        if(sexes==F)
          {
            SRdat.new[5]<-paste(as.character(M.draw),as.character(M.draw),sep=" ")
            M_FMSY<-M.draw
          }
        if(sexes==T)
          {
            SRdat.new[5]<-paste(as.character(M.draw),as.character(M.draw.M),sep=" ")
            #M_FMSY<-mean(c(M.draw,M.draw.M))
            M_FMSY<-M.draw
          }
        #print(c(M.draw,M.draw.M,M_FMSY,FMSY_M.draw,BMSY_B0.draw))
        #SRdat.new.mod[3]<-Winf
        #SRdat.new.mod[4]<-as.numeric(k.line[3])
        #SRdat.new.mod[5]<-as.numeric(ltwt.exp.line[3])
        #SRdat.new.mod[6]<-SRdat.new.mod[7]<-Amat
        #SRdat.new.mod[8]<-SR_type-7 #-7 converts from SS SR_type to MQ SR_type
        SRdat.new[17]<-as.character(FMSY_M.draw*M_FMSY)
        SRdat.new[19]<-as.character(BMSY_B0.draw)
        Input.draws.MQs[i,3]<-FMSY_M.draw*M_FMSY
        #SRdat.new[grep("Amax",SRdat.new)+1][1]<-paste(SRdat.new.mod,collapse=" ")
        SRdat.new[21]<-"-0.51"
        write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
        RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
        if(file.exists(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))==TRUE)
        {
        
        SRMQ_rep.new<-readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
        SRMQ_par[i]<-as.numeric(strsplit(readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep="")),split=" ")[[1]][11])
        steep_expo.out<-as.numeric(strsplit(SRMQ_rep.new[1]," ")[[1]])
        if(steep_expo.out[1]<0.2|steep_expo.out[1]>=2|steep_expo.out[2]>10|SRMQ_par[i]>0.1)
          {
            #print("retry 1")
            SRdat.new[21]<-"-0.1"
            write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
            RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
        SRMQ_rep.new<-readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
        SRMQ_par[i]<-as.numeric(strsplit(readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep="")),split=" ")[[1]][11])
        steep_expo.out<-as.numeric(strsplit(SRMQ_rep.new[1]," ")[[1]])
            if(steep_expo.out[1]<0.25|steep_expo.out[1]>=1|steep_expo.out[2]>10|SRMQ_par[i]>0.1)
            {
            #print("retry 2")
            SRdat.new[21]<-"-1.51"
            write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
            RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
        SRMQ_rep.new<-readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
        SRMQ_par[i]<-as.numeric(strsplit(readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep="")),split=" ")[[1]][11])
        steep_expo.out<-as.numeric(strsplit(SRMQ_rep.new[1]," ")[[1]])
            }
            if(steep_expo.out[1]<0.25|steep_expo.out[1]>=1|steep_expo.out[2]>10|SRMQ_par[i]>0.1)
            {
            #print("retry 3")
            SRdat.new[21]<-"-0.51"
            SRdat.new[23]<-"-0.31"
            write(SRdat.new,paste(filepath,"/h_BMSY_FMSY/BMSYB0.dat",sep=""))
            RUN.SS(paste(filepath,"/h_BMSY_FMSY/",sep=""), ss.exe="BMSYB0",ss.cmd=" -nox > out.txt")
        SRMQ_rep.new<-readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
        SRMQ_par[i]<-as.numeric(strsplit(readLines(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep="")),split=" ")[[1]][11])
        steep_expo.out<-as.numeric(strsplit(SRMQ_rep.new[1]," ")[[1]])
            }
          } 
        if(steep_expo.out[1]>=0.2&steep_expo.out[1]<2&steep_expo.out[2]<=10&SRMQ_par[i]<0.1){x<-2}          
        }
      }
      Input.draws[i,1]<-h.draw<-steep_expo.out[1]
      SR_expo.out[i]<-steep_expo.out[2]
      file.remove(paste(filepath,"/h_BMSY_FMSY/bmsyb0.rep",sep=""))
      file.remove(paste(filepath,"/h_BMSY_FMSY/bmsyb0.par",sep=""))
    }
    
    #Three parameter S-R model
    if(Zfrac.Beta.in[1]>=0)
    {
      if(Zfrac.Beta.in[1]==10){Zfrac.draw<-round(rtnorm(1,Zfrac.Beta.in[2],Zfrac.Beta.in[3],0,1),2)}
      if(Zfrac.Beta.in[1]==30){Zfrac.draw<-round(rlnormTrunc(1,log(Zfrac.Beta.in[2]),Zfrac.Beta.in[3],0,1),2)}
      if(Zfrac.Beta.in[1]==4){Zfrac.draw<-round(runif(1,Zfrac.Beta.in[2],Zfrac.Beta.in[3]),2)}
      Input.draws[i,1]<-Zfrac.draw
    }
    if(Zfrac.Beta.in[4]>=0)
    {
      if(Zfrac.Beta.in[4]==10){Beta.draw<-round(rtnorm(1,Zfrac.Beta.in[5],Zfrac.Beta.in[6],0,10),2)}
      if(Zfrac.Beta.in[4]==30){Beta.draw<-round(rlnormTrunc(1,log(Zfrac.Beta.in[5]),Zfrac.Beta.in[6],0,10),2)}
      if(Zfrac.Beta.in[4]==4){Beta.draw<-round(runif(1,Zfrac.Beta.in[5],Zfrac.Beta.in[6]),2)}
      Input.draws[i,7]<-Beta.draw
    }
    

    ### Change DAT and CTL inputs ###
    #change Depletion
    if(Dep.in[1]>=0)
    {
      dat.new$CPUE[2,4]<-Dep.draw
      SS_writedat(dat.new, paste0(filepath,"/",file.name[1]), overwrite=TRUE,verbose=FALSE)      
    }

    #change M
    Sys.sleep(1)
    if(M.in[1]>=0){ctl.new$MG_parms[1,3]<-M.draw}
    #change growth parameters
      ctl.new$Growth_Age_for_L1<-t0.draw
      ctl.new$MG_parms[2,3:4]<-ctl.new$MG_parms[14,3]<-0
      ctl.new$MG_parms[3,3:4]<-Linf.draw
      ctl.new$MG_parms[4,3:4]<-k.draw

    #change male pararmeters
    if(sexes==T)
    {
      #change M
      if(M.in[4]>=0){ctl.new$MG_parms[13,3]<-M.draw.M}
      #change growth parameters
        ctl.new$MG_parms[14,3]<-L1.draw.M
        ctl.new$MG_parms[15,3]<-Linf.draw.M
        ctl.new$MG_parms[16,3]<-k.draw.M
    }
    
    ctl.new$SR_function<-SR_type
    # SRtype.line<-strsplit(ctl.new[grep("SR_function",ctl.new)], " ")[[1]]
    # SRtype.line[1]<-SR_type
    # ctl.new[grep("SR_function",ctl.new)]<-paste(SRtype.line,collapse=" ")
    
    #change R0
    if(R_start[1]==0)
    {
      R0.draw<-R_start[2]
      ctl.new$SR_parms[1,3:4]<-R0.draw
      # R0.line<-strsplit(ctl.new[grep("R0",ctl.new)], " ")[[1]]
      # R0.line[c(3,4)]<-R0.draw
      # ctl.new[grep("R0",ctl.new)]<-paste(R0.line,collapse=" ")
    }
  
    if(R_start[1]==1)
    {
      

      R0.draw<-round(rlnormTrunc(1,log(R_start[2]),0.5,3,15),2)
      ctl.new$SR_parms[1,3:4]<-R0.draw
      # R0.line<-strsplit(ctl.new[grep("R0",ctl.new)], " ")[[1]]
      # R0.line[c(3,4)]<-R0.draw
      # ctl.new[grep("R0",ctl.new)]<-paste(R0.line,collapse=" ")
    }
    
    #change h
    if(SR_type==3)
    {
      ctl.new$SR_parms[2,3:4]<-h.draw
      # h.line<-strsplit(ctl.new[grep("SR_BH_steep",ctl.new)], " ")[[1]]
      # h.line[c(3,4)]<-h.draw
      # ctl.new[grep("SR_BH_steep",ctl.new)]<-paste(h.line,collapse=" ")
    }
    
    if(SR_type==9)
    {
      ctl.new$SR_parms[2,3:4]<-h.draw
      ctl.new$SR_parms[3,3:4]<-SR_expo.out[i]
      h.line<-strsplit(ctl.new[grep("SR_RkrPower_steep",ctl.new)], " ")[[1]]
      # h.line[c(3,4)]<-h.draw
      # ctl.new[grep("SR_RkrPower_steep",ctl.new)]<-paste(h.line,collapse=" ")
      # SRexpo.line<-strsplit(ctl.new[grep("SR_RkrPower_gamma",ctl.new)], " ")[[1]]
      # SRexpo.line[c(3,4)]<-SR_expo.out[i]
      # ctl.new[grep("SR_RkrPower_gamma",ctl.new)]<-paste(SRexpo.line,collapse=" ")
    }  
    
    #change Sfrac and Beta
    if(SR_type==7 & Zfrac.Beta.in[1]>=0)
    {
      ctl.new$SR_parms[2,3:4]<-Zfrac.draw
      # Zfrac.line<-strsplit(ctl.new[grep("Zfrac",ctl.new)], " ")[[1]]
      # Zfrac.line[c(3,4)]<-Zfrac.draw
      # ctl.new[grep("Zfrac",ctl.new)]<-paste(Zfrac.line,collapse=" ")
    }
    if(SR_type==7 & Zfrac.Beta.in[4]>=0)
    {
      ctl.new$SR_parms[3,3:4]<-Beta.draw
      # Beta.line<-strsplit(ctl.new[grep("Beta",ctl.new)], " ")[[1]]
      # Beta.line[c(3,4)]<-Beta.draw
      # ctl.new[grep("Beta",ctl.new)]<-paste(Beta.line,collapse=" ")
    }
    
    #write(ctl.new,paste(filepath,"/",file.name[2],sep=""))
    SS_writectl(ctl.new, paste0(filepath,"/",file.name[2]), overwrite=TRUE,verbose=FALSE)      
    



    #Run model
    RUN.SS(paste(filepath,"/",sep=""),ss.cmd=" -nohess -nox > out.txt 2>&1",OS.in=OStype)
    #if(OStype=="Windows"){RUN.SS(paste(filepath,"/",sep=""), ss.exe="ss",ss.cmd=" -nohess -nox > out.txt 2>&1")}
    #if(OStype=="OSX_Linux"){RUN.SS(paste(filepath,"/",sep=""), ss.exe="./ss",ss.cmd=" -nohess -nox > out.txt 2>&1")}
    
    #Evaluate convergence and record values
    rep.new<-readLines(paste(filepath,"/Report.sso",sep=""))
    Sys.sleep(0.5)
    #The R0 loop. Makes sure R0 is changing from input and that the depletion match is happening. Ensures good models are kept.
    #Profiles over R0 when intial run fails.
    if(doR0.loop[1]>0)
    {
        xx<-1
        R0.explore<-seq(doR0.loop[2],doR0.loop[3],doR0.loop[4])
        if(is.na(as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2]))==TRUE|as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])==0){Dep.out.testR0<-10}
        if(is.na(as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2]))==FALSE&as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])>0){Dep.out.testR0<-as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])}
      #  while(abs(as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][3])-as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][8]))==0)
      while(abs(Dep.out.testR0-Dep.draw)>0.01)
      {
        print(paste0("IN THE LOOP: R0= ",R0.explore[xx]))
        ctl.new$SR_parms[1,3:4]<-R0.explore[xx]
        SS_writectl(ctl.new, paste0(filepath,"/",file.name[2]), overwrite=TRUE,verbose=FALSE)      
        # ctl.new<-readLines(paste(filepath,"/",file.name[2],sep=""))
        # R0.line<-strsplit(ctl.new[grep("R0",ctl.new)], " ")[[1]]
        # R0.line[c(3,4)]<-R0.explore[xx]
        # ctl.new[grep("R0",ctl.new)]<-paste(R0.line,collapse=" ")
        # write(ctl.new,paste(filepath,"/",file.name[2],sep=""))
        RUN.SS(paste(filepath,"/",sep=""),ss.cmd=" -nohess -nox > out.txt 2>&1",OS.in=OStype)
        # if(OStype=="Windows"){RUN.SS(paste(filepath,"/",sep=""), ss.exe="ss",ss.cmd=" -nohess -nox > out.txt 2>&1")}
        # if(OStype=="OSX_Linux"){RUN.SS(paste(filepath,"/",sep=""), ss.exe="./ss",ss.cmd=" -nohess -nox > out.txt 2>&1")}
        rep.new<-readLines(paste(filepath,"/Report.sso",sep=""))
        if(is.na(as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2]))==TRUE|as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])==0){Dep.out.testR0<-10}
        if(is.na(as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2]))==FALSE&as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])>0){Dep.out.testR0<-as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])}
        print(paste0("Is depletion difference < 0.01? ",(abs(as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])-Dep.draw)<0.01)))
        xx<-xx+1
        if(xx==length(R0.explore)){break}
      }
    }
    SSS.output.list[[i]]<-SS_output(paste(filepath,"/",sep=""), covar=FALSE,verbose=FALSE,printstats=FALSE)
  
    #Begin extracting info from run
    #forecast.file<-readLines(paste(filepath,"/Forecast-report.sso",sep=""))
 #    for(iii in 1:length(ts_yrs[1]:ts_yrs[2]))
 #      {
 #        SB.out[i,iii]<-as.numeric(strsplit(rep.new[grep(paste("SSB_",sb.years[iii],sep=""),rep.new)], " ")[[1]][3])
 #        TB.out[i,iii]<-as.numeric(strsplit(rep.new[grep("TIME_SERIES",rep.new)+3+iii], " ")[[2]][5])
 #        SumAge.out[i,iii]<-as.numeric(strsplit(rep.new[grep("TIME_SERIES",rep.new)+3+iii], " ")[[2]][6])
 #        SPR.out[i,iii]<-1-as.numeric(strsplit(rep.new[grep(paste("SPRratio_",sb.years[iii]+1,sep=""),rep.new)], " ")[[1]][3])
 # #       B_BMSY.out[i,iii]<-as.numeric(strsplit(rep.new[grep("Yr  B/Bmsy  F/Fmsy",rep.new)+iii], " ")[[1]][2])
 #      }
  if(i==1)
  {
    SB.out<-TB.out<-SumAge.out<-SPR.out<-as.data.frame(matrix(NA,nrow=reps,ncol=length(SSS.output.list[[i]]$timeseries$SpawnBio)))
    colnames(SB.out)<-colnames(TB.out)<-colnames(SumAge.out)<-colnames(SPR.out)<-sb.years    
    Dep.series.out<-as.data.frame(matrix(NA,nrow=reps,ncol=length(SSS.output.list[[i]]$sprseries$Deplete)))
    colnames(Dep.series.out)<-SSS.output.list[[i]]$sprseries$Yr
  }
      
    		SB.out[i,]<-SSS.output.list[[i]]$timeseries$SpawnBio
    		TB.out[i,]<-SSS.output.list[[i]]$timeseries$Bio_all
    		SumAge.out[i,]<-SSS.output.list[[i]]$timeseries$Bio_smry
        Dep.series.out[i,]<-SSS.output.list[[i]]$sprseries$Deplete
    		#SPR.out[i,]<-1-as.numeric(strsplit(rep.new[grep(paste("SPRratio_",sb.years[iii]+1,sep=""),rep.new)], " ")[[1]][3])
 	  
    nforecasts<- SSS.output.list[[i]]$nforecastyears
    OFL.out[[i]]<-SSS.output.list[[i]]$derived_quants[paste0("OFLCatch_", c((ts_yrs[2]+1):(ts_yrs[2]+nforecasts))), "Value"]
    ABC.out[[i]]<-SSS.output.list[[i]]$derived_quants[paste0("ForeCatch_", c((ts_yrs[2]+1):(ts_yrs[2]+nforecasts))), "Value"]
    
  
    #Dep.series.out<-SB.out/SB.out[,1]
    #Quant.out[i,1]<-as.numeric(strsplit(rep.new[grep("NatM_p_1_Fem_GP_1",rep.new)], " ")[[1]][3])
    # Quant.out[i,2]<-as.numeric(strsplit(rep.new[grep("L_at_Amin_Fem_GP_1",rep.new)], " ")[[1]][3])
    # Quant.out[i,3]<-as.numeric(strsplit(rep.new[grep("L_at_Amax_Fem_GP_1",rep.new)], " ")[[1]][3])
    # Quant.out[i,4]<-as.numeric(strsplit(rep.new[grep("VonBert_K_Fem_GP_1",rep.new)], " ")[[1]][3])
    Quant.out[i,1]<-SSS.output.list[[i]]$parameters[1,3]
    Quant.out[i,2]<-SSS.output.list[[i]]$parameters[2,3]
    Quant.out[i,3]<-SSS.output.list[[i]]$parameters[3,3]
    Quant.out[i,4]<-SSS.output.list[[i]]$parameters[4,3]
    if(sexes==T)
    {
      # Quant.out[i,5]<-as.numeric(strsplit(rep.new[grep("NatM_p_1_Mal_GP_1",rep.new)], " ")[[1]][3])
      # Quant.out[i,6]<-as.numeric(strsplit(rep.new[grep("L_at_Amin_Mal_GP_1",rep.new)], " ")[[1]][3])
      # Quant.out[i,7]<-as.numeric(strsplit(rep.new[grep("L_at_Amax_Mal_GP_1",rep.new)], " ")[[1]][3])
      # Quant.out[i,8]<-as.numeric(strsplit(rep.new[grep("VonBert_K_Mal_GP_1",rep.new)], " ")[[1]][3])
      Quant.out[i,5]<-SSS.output.list[[i]]$parameters[13,3]
      Quant.out[i,6]<-SSS.output.list[[i]]$parameters[14,3]
      Quant.out[i,7]<-SSS.output.list[[i]]$parameters[15,3]
      Quant.out[i,8]<-SSS.output.list[[i]]$parameters[16,3]
    }
    # if(SR_type==3){Quant.out[i,9]<-as.numeric(strsplit(rep.new[grep("SR_BH_steep",rep.new)], " ")[[1]][3])}
    if(SR_type==3){Quant.out[i,9]<-SSS.output.list[[i]]$parameters[24,3]}
    if(SR_type==7){Quant.out[i,9]<-as.numeric(strsplit(rep.new[grep("SR_surv_Beta",rep.new)-1], " ")[[1]][2])}
    if(SR_type==7){Quant.out[i,29]<-as.numeric(strsplit(rep.new[grep("SR_surv_Beta",rep.new)], " ")[[1]][2])}
    if(SR_type==8){Quant.out[i,9]<-as.numeric(strsplit(rep.new[grep("SR_RkrPower_steep",rep.new)], " ")[[1]][2])}
    if(SR_type==8){Quant.out[i,29]<-as.numeric(strsplit(rep.new[grep("SR_RkrPower_gamma",rep.new)], " ")[[1]][2])}
    #Quant.out[i,10]<-as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][3])
    Quant.out[i,10]<-SSS.output.list[[i]]$parameters[23,3]
    #Quant.out[i,11]<-as.numeric(strsplit(rep.new[grep("SSB_Virgin",rep.new)], " ")[[1]][3])
    Quant.out[i,11]<-SSS.output.list[[i]]$SBzero
    Quant.out[i,12]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep(paste("SSB_",ts_yrs[2],sep=""),SSS.output.list[[i]]$derived_quants[,1]),][2])
    Quant.out[i,13]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep(paste("Bratio_",ts_yrs[2],sep=""),SSS.output.list[[i]]$derived_quants[,1]),][2])
    Quant.out[i,14]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep("SSB_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2])
    Quant.out[i,15]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep("B_MSY/SSB_unfished",SSS.output.list[[i]]$derived_quants[,1]),][2])
    if(!is.na(as.numeric(SSS.output.list[[i]]$derived_quants[grep("annF_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2]))){Quant.out[i,16]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep("annF_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2])}
    Quant.out[i,17]<-SSS.output.list[[i]]$likelihoods_used[1,1]
    Quant.out[i,18]<-SSS.output.list[[i]]$likelihoods_used[4,1]
    Quant.out[i,19]<-SSS.output.list[[i]]$maximum_gradient_component
    if(Dep.in[1]>=0){Quant.out[i,20]<-as.numeric(dat.new$CPUE[2,4])}
    if(Dep.in[1]>=0){Quant.out[i,21]<-as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])}
    Quant.out[i,22]<-SSS.output.list[[i]]$parameters[23,8]
    Quant.out[i,23]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep(paste("F_",ts_yrs[2],sep=""),SSS.output.list[[i]]$derived_quants[,1]),][2])/as.numeric(SSS.output.list[[i]]$derived_quants[grep("annF_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2])
    Quant.out[i,24]<-SSS.output.list[[i]]$likelihoods_used[11,1]
    warnings.out<-readLines(paste(filepath,"/warning.sso",sep=""))
    num.warning<-as.numeric(strsplit(warnings.out[grep("N warnings",warnings.out)[1]], " ")[[1]][4])
    #Quant.out[i,12]<-as.numeric(strsplit(rep.new[grep(paste("SSB_",ts_yrs[2],sep=""),rep.new)], " ")[[1]][3])
    #Quant.out[i,13]<-as.numeric(strsplit(rep.new[grep(paste("SSB_",ts_yrs[2],sep=""),rep.new)], " ")[[1]][3])/as.numeric(strsplit(rep.new[grep("SSB_Virgin",rep.new)], " ")[[1]][3])
    #Quant.out[i,14]<-as.numeric(strsplit(rep.new[grep("SSB_MSY",rep.new)], " ")[[1]][2])/as.numeric(strsplit(rep.new[grep("SSB_Initial",rep.new)], " ")[[1]][3])
    #Quant.out[i,15]<-as.numeric(strsplit(rep.new[grep("Bmsy/Bzero",rep.new)], " ")[[1]][1])
    #Quant.out[i,17]<-as.numeric(strsplit(forecast.file[grep("calculate_FMSY",forecast.file)+13],split="[[:blank:]]+")[[1]][2])/as.numeric(strsplit(forecast.file[grep("BIO_Smry_unfished",forecast.file)],split="[[:blank:]]+")[[1]][2])
    # if(!is.na(as.numeric(strsplit(rep.new[grep("annF_MSY",rep.new)], " ")[[1]][2]))){Quant.out[i,16]<-as.numeric(strsplit(rep.new[grep("annF_MSY",rep.new)], " ")[[1]][2])}
    # Quant.out[i,17]<-as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)], " ")[[1]][2])
    # Quant.out[i,18]<-as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)+2], " ")[[1]][2])
    # Quant.out[i,19]<-as.numeric(strsplit(rep.new[grep("Convergence",rep.new)], " ")[[1]][2])
    # Quant.out[i,23]<-as.numeric(strsplit(rep.new[grep(paste("F_",ts_yrs[2],sep=""),rep.new)], " ")[[1]][3])/as.numeric(strsplit(rep.new[grep("annF_MSY",rep.new)], " ")[[1]][2])
#    if(!is.na(as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2]))){Quant.out[i,26]<-as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2])}
#    if(!is.na(as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2]))){Quant.out[i,27]<-as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2])}
    #Quant.out[i,24]<-as.numeric(strsplit(rep.new[grep("Crash_Pen",rep.new)], " ")[[1]][2])
    
    if(Dep.in[1]>=0)
    {

        if(FMSY_M.in[1]<0 & BMSY_B0.in[1]<0&abs(Quant.out[i,20]-Quant.out[i,21])<0.01&Quant.out[i,24]<0.0001){i<-i+1}
        if(FMSY_M.in[1]>=0 & BMSY_B0.in[1]>=0 | h.in[1]==99 | BH_FMSY_comp==T)
          {
            if(h.in[1]==99)
              {
                if(abs(Quant.out[i,20]-Quant.out[i,21])<0.01&Quant.out[i,24]<0.0001&(abs(Quant.out[i,14]-Input.draws.MQs[i,3])/Input.draws.MQs[i,3])<0.6){i<-i+1}
                else
                  {
                    if(n.bad==0){Quant.out.bad<-Quant.out[i,]}
                    if(n.bad>0){Quant.out.bad<-rbind(Quant.out.bad,Quant.out[i,])}
                    n.bad<-n.bad+1
                    if(length(M.in)>6|length(Dep.in)>3|length(h.in)>3|length(FMSY_M.in)>3|length(BMSY_B0.in)>3)
                      {
                        Quant.out[i,]<-NA
                        i<-i+1
                      }
                  }    
              }

            else
            {
              if(abs(Quant.out[i,20]-Quant.out[i,21])<0.01&Quant.out[i,24]<0.0001&(abs(Quant.out[i,14]-(Input.draws.MQs[i,2])/100)/(Input.draws.MQs[i,2]/100))<0.5){i<-i+1}
              else
                {
                   if(n.bad==0){Quant.out.bad<-Quant.out[i,]}
                   if(n.bad>0){Quant.out.bad<-rbind(Quant.out.bad,Quant.out[i,])}
                   n.bad<-n.bad+1
                   if(length(M.in)>6|length(Dep.in)>3|length(h.in)>3|length(FMSY_M.in)>3|length(BMSY_B0.in)>3)
                     {
                       Quant.out[i,]<-NA
                       i<-i+1
                     }
                }
            }
          }
                
        else
        {
          if(n.bad==0){Quant.out.bad<-Quant.out[i,]}
          if(n.bad>0){Quant.out.bad<-rbind(Quant.out.bad,Quant.out[i,])}
          n.bad<-n.bad+1
          if(length(M.in)>6|length(Dep.in)>3|length(h.in)>3|length(FMSY_M.in)>3|length(BMSY_B0.in)>3)
            {
              Quant.out[i,]<-NA
              i<-i+1
            }
        }
      # }
    }
    
    if(Dep.in[1]<0)
    {
      if(Quant.out[i,24]<0.0001){i<-i+1}
      else
      {
        #Run using .par file if above conditions not met
        print("*** RUNNING WITH .PAR ***")
        starter.new<-readLines(paste(filepath,"/starter.ss",sep=""))
        par_line<-strsplit(starter.new[grep("ss.par",starter.new)], " ")[[1]]
        par_line[1]<-1
        starter.new[grep("ss.par",starter.new)]<-paste(par_line, collapse=" ")
        write(starter.new,paste(filepath,"/starter.ss",sep=""))
        RUN.SS(paste(filepath,"/",sep=""),ss.cmd=" -nohess -nox > out.txt 2>&1",OS.in=OStype)
        # if(OStype=="Windows"){RUN.SS(paste(filepath,"/",sep=""), ss.exe="ss",ss.cmd=" -nohess -nox > out.txt 2>&1")}
        # if(OStype=="OSX_Linux"){RUN.SS(paste(filepath,"/",sep=""), ss.exe="./ss",ss.cmd=" -nohess -nox > out.txt 2>&1")}
        #RUN.SS(paste(filepath,"/",sep=""), ss.exe="ss",ss.cmd=" -nohess -nox > out.txt 2>&1")
        rep.new<-readLines(paste(filepath,"/Report.sso",sep=""))
        SSS.output.list[[i]]<-SS_output(paste(filepath,"/",sep=""), covar=FALSE,verbose=FALSE,printstats=FALSE)
        #forecast.file<-readLines(paste(filepath,"/Forecast-report.sso",sep=""))
#       for(iii in 1:length(ts_yrs[1]:ts_yrs[2]))
#     	{
#     		SB.out[i,iii]<-as.numeric(strsplit(rep.new[grep(paste("SSB_",sb.years[iii],sep=""),rep.new)], " ")[[1]][3])
#     		TB.out[i,iii]<-as.numeric(strsplit(rep.new[grep("TIME_SERIES",rep.new)+3+iii], " ")[[2]][5])
#     		SumAge.out[i,iii]<-as.numeric(strsplit(rep.new[grep("TIME_SERIES",rep.new)+3+iii], " ")[[2]][6])
#     		SPR.out[i,iii]<-1-as.numeric(strsplit(rep.new[grep(paste("SPRratio_",sb.years[iii]+1,sep=""),rep.new)], " ")[[1]][3])
# #        B_BMSY.out[i,iii]<-as.numeric(strsplit(rep.new[grep("Yr  B/Bmsy  F/Fmsy",rep.new)+iii], " ")[[1]][2])
#     	}
        SB.out[i,]<-SSS.output.list[[i]]$timeseries$SpawnBio
        TB.out[i,]<-SSS.output.list[[i]]$timeseries$Bio_all
        SumAge.out[i,]<-SSS.output.list[[i]]$timeseries$Bio_smry
        Dep.series.out[i,]<-SSS.output.list[[i]]$sprseries$Deplete

        #Dep.series.out<-SB.out/SB.out[,1]
        Quant.out[i,1]<-SSS.output.list[[i]]$parameters[1,3]
        Quant.out[i,2]<-SSS.output.list[[i]]$parameters[2,3]
        Quant.out[i,3]<-SSS.output.list[[i]]$parameters[3,3]
        Quant.out[i,4]<-SSS.output.list[[i]]$parameters[4,3]
        # Quant.out[i,1]<-as.numeric(strsplit(rep.new[grep("NatM_p_1_Fem_GP_1",rep.new)], " ")[[1]][3])
        # Quant.out[i,2]<-as.numeric(strsplit(rep.new[grep("L_at_Amin_Fem_GP_1",rep.new)], " ")[[1]][3])
        # Quant.out[i,3]<-as.numeric(strsplit(rep.new[grep("L_at_Amax_Fem_GP_1",rep.new)], " ")[[1]][3])
        # Quant.out[i,4]<-as.numeric(strsplit(rep.new[grep("VonBert_K_Fem_GP_1",rep.new)], " ")[[1]][3])
    if(sexes==T)
    {
        Quant.out[i,5]<-SSS.output.list[[i]]$parameters[13,3]
        Quant.out[i,6]<-SSS.output.list[[i]]$parameters[14,3]
        Quant.out[i,7]<-SSS.output.list[[i]]$parameters[15,3]
        Quant.out[i,8]<-SSS.output.list[[i]]$parameters[16,3]
        # Quant.out[i,5]<-as.numeric(strsplit(rep.new[grep("NatM_p_1_Mal_GP_1",rep.new)], " ")[[1]][3])
        # Quant.out[i,6]<-as.numeric(strsplit(rep.new[grep("L_at_Amin_Mal_GP_1",rep.new)], " ")[[1]][3])
        # Quant.out[i,7]<-as.numeric(strsplit(rep.new[grep("L_at_Amax_Mal_GP_1",rep.new)], " ")[[1]][3])
        # Quant.out[i,8]<-as.numeric(strsplit(rep.new[grep("VonBert_K_Mal_GP_1",rep.new)], " ")[[1]][3])
    }
        #if(SR_type==3){Quant.out[i,9]<-as.numeric(strsplit(rep.new[grep("SR_BH_steep",rep.new)], " ")[[1]][3])}
        if(SR_type==3){Quant.out[i,9]<-SSS.output.list[[i]]$parameters[24,3]}
        if(SR_type==7){Quant.out[i,9]<-as.numeric(strsplit(rep.new[grep("SR_surv_Beta",rep.new)-1], " ")[[1]][3])}
        if(SR_type==7){Quant.out[i,29]<-as.numeric(strsplit(rep.new[grep("SR_surv_Beta",rep.new)], " ")[[1]][3])}
        if(SR_type==8){Quant.out[i,9]<-as.numeric(strsplit(rep.new[grep("SR_RkrPower_steep",rep.new)], " ")[[1]][3])}
        if(SR_type==8){Quant.out[i,29]<-as.numeric(strsplit(rep.new[grep("SR_RkrPower_gamma",rep.new)], " ")[[1]][3])}
        Quant.out[i,10]<-SSS.output.list[[i]]$parameters[23,3]
        Quant.out[i,11]<-SSS.output.list[[i]]$SBzero
        Quant.out[i,12]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep(paste("SSB_",ts_yrs[2],sep=""),SSS.output.list[[i]]$derived_quants[,1]),][2])
        Quant.out[i,13]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep(paste("Bratio_",ts_yrs[2],sep=""),SSS.output.list[[i]]$derived_quants[,1]),][2])
        Quant.out[i,14]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep("SSB_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2])
        Quant.out[i,15]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep("B_MSY/SSB_unfished",SSS.output.list[[i]]$derived_quants[,1]),][2])
        if(!is.na(as.numeric(SSS.output.list[[i]]$derived_quants[grep("annF_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2]))){Quant.out[i,16]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep("annF_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2])}
        Quant.out[i,17]<-SSS.output.list[[i]]$likelihoods_used[1,1]
        Quant.out[i,18]<-SSS.output.list[[i]]$likelihoods_used[4,1]
        Quant.out[i,19]<-SSS.output.list[[i]]$maximum_gradient_component
        if(Dep.in[1]>=0){Quant.out[i,20]<-as.numeric(dat.new$CPUE[2,4])}
        if(Dep.in[1]>=0){Quant.out[i,21]<-as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][2])}
        Quant.out[i,22]<-SSS.output.list[[i]]$parameters[23,8]
        Quant.out[i,23]<-as.numeric(SSS.output.list[[i]]$derived_quants[grep(paste("F_",ts_yrs[2],sep=""),SSS.output.list[[i]]$derived_quants[,1]),][2])/as.numeric(SSS.output.list[[i]]$derived_quants[grep("annF_MSY",SSS.output.list[[i]]$derived_quants[,1]),][2])
        Quant.out[i,24]<-SSS.output.list[[i]]$likelihoods_used[11,1]
        # Quant.out[i,10]<-as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][3])
        # Quant.out[i,11]<-as.numeric(strsplit(rep.new[grep("SSB_Initial",rep.new)], " ")[[1]][3])
        # Quant.out[i,12]<-as.numeric(strsplit(rep.new[grep(paste("SSB_",ts_yrs[2],sep=""),rep.new)], " ")[[1]][3])
        # Quant.out[i,13]<-as.numeric(strsplit(rep.new[grep(paste("SSB_",ts_yrs[2],sep=""),rep.new)], " ")[[1]][3])/as.numeric(strsplit(rep.new[grep("SSB_Initial",rep.new)], " ")[[1]][3])
        # #Quant.out[i,14]<-as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",ts_yrs[2]+1,sep=""),rep.new)], " ")[[1]][2])
        # #Quant.out[i,15]<-as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",ts_yrs[2]+1,sep=""),rep.new)], " ")[[1]][2])
        # Quant.out[i,14]<-as.numeric(strsplit(rep.new[grep("SSB_MSY",rep.new)], " ")[[1]][2])/as.numeric(strsplit(rep.new[grep("SSB_Initial",rep.new)], " ")[[1]][3])
        # Quant.out[i,15]<-as.numeric(strsplit(rep.new[grep("Bmsy/Bzero",rep.new)], " ")[[1]][1])
        # #Quant.out[i,17]<-as.numeric(strsplit(forecast.file[grep("Maximum_Sustainable_Yield;_where_Yield_is_Dead_Catch",forecast.file)+14],split="[[:blank:]]+")[[1]][2])/as.numeric(strsplit(forecast.file[grep("BIO_Smry_unfished",forecast.file)],split="[[:blank:]]+")[[1]][2])
        # if(!is.na(as.numeric(strsplit(rep.new[grep("annF_MSY",rep.new)], " ")[[1]][2]))){Quant.out[i,16]<-as.numeric(strsplit(rep.new[grep("annF_MSY",rep.new)], " ")[[1]][2])}
        # Quant.out[i,17]<-as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)], " ")[[1]][2])
        # Quant.out[i,18]<-as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)+2], " ")[[1]][2])
        # Quant.out[i,19]<-as.numeric(strsplit(rep.new[grep("Convergence",rep.new)], " ")[[1]][2])
        # Quant.out[i,20]<-as.numeric(dat.new$CPUE[2,4])
        # Quant.out[i,21]<-as.numeric(strsplit(rep.new[grep(paste("Bratio_",as.numeric(dat.new$CPUE[2,1]),sep=""),rep.new)], " ")[[1]][3])
        # Quant.out[i,22]<-as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][8])
        # Quant.out[i,23]<-as.numeric(strsplit(rep.new[grep(paste("F_",ts_yrs[2],sep=""),rep.new)], " ")[[1]][3])/as.numeric(strsplit(rep.new[grep("annF_MSY",rep.new)], " ")[[1]][2])
 #       if(!is.na(as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2]))){Quant.out[i,26]<-as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2])}
 #       if(!is.na(as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2]))){Quant.out[i,27]<-as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",ts_yrs[2]+2,sep=""),rep.new)], " ")[[1]][2])}
        # Quant.out[i,24]<-as.numeric(strsplit(rep.new[grep("Crash_Pen",rep.new)], " ")[[1]][2])
        starter.new<-readLines(paste(filepath,"/starter.ss",sep=""))
        par_line<-strsplit(starter.new[grep("ss.par",starter.new)], " ")[[1]]
        par_line[1]<-0
        starter.new[grep("ss.par",starter.new)]<-paste(par_line, collapse=" ")
        write(starter.new,paste(filepath,"/starter.ss",sep=""))
        if(Quant.out[i,24]<0.0001){i<-i+1}
        else
        {
          if(n.bad==0){Quant.out.bad<-Quant.out[i,]}
          if(n.bad>0){Quant.out.bad<-rbind(Quant.out.bad,Quant.out[i,])}
          n.bad<-n.bad+1
        }
      }
    }
    print(paste0("Kept ",i-1))
    ii<-ii+1
    if(file.exists(paste(filepath,"/Report.sso",sep=""))){file.remove(paste(filepath,"/Forecast-report.sso",sep=""))}
    if(file.exists(paste(filepath,"/Report.sso",sep=""))){file.remove(paste(filepath,"/Report.sso",sep=""))}
  colnames(Quant.out)<-colnames(Quant.out.bad)<-c("M_f","L1_f","Linf_f","k_f","M_m","L1_m","Linf_m","k_m","h","R0","SB0",paste("SSB_",ts_yrs[2],sep=""),"Term_Yr_Dep","SBMSY","SBMSY/SB0","FMSY","-lnL","LL_survey","Gradient","Dep.Obs","Dep.Exp","R0_init",paste("F_",ts_yrs[2],"/FMSY",sep=""),"Crash_penalty","Rick_gamma")
  if(ncol(Input.draws)==10){colnames(Input.draws)<-c("h","Dep","M_f","L1_f","Linf_f","k_f","M_m","L1_m","Linf_m","k_m")}
  if(ncol(Input.draws)==11){colnames(Input.draws)<-c("Sfrac","Dep","M_f","L1_f","Linf_f","k_f","Beta","M_m","L1_m","Linf_m","k_m")}
  if(SR_type>=8 | h.in[1]==99 | BH_FMSY_comp==T)
    {
      Input.draws<-cbind(Input.draws,Input.draws.MQs,SR_expo.out,SRMQ_par)
      ltcolnames<-length(colnames(Input.draws))
      colnames(Input.draws)[c(ltcolnames-1,ltcolnames)]<-c("Beta","Obj_fxn")
    }
  end.time<-Sys.time()
  OFL.out.save<-do.call("rbind",OFL.out)
  ABC.out.save<-do.call("rbind",ABC.out)
  colnames(OFL.out.save)<-colnames(ABC.out.save)<-c((ts_yrs[2]+1):(ts_yrs[2]+nforecasts))
  Spp.quant.out<-list(Input.draws,Quant.out,SB.out,Dep.series.out[,-1],TB.out,SumAge.out,OFL.out.save,ABC.out.save,Quant.out.bad,ii-1,(as.numeric(end.time)-as.numeric(start.time))/60)
  names(Spp.quant.out)<-c("Priors","Posteriors","SB_series","Rel_Stock_status_series","Total_Biomass","Summary_Biomass","OFL","ABC","Rejected_draws","Total draws","Runtime_minutes")
  SSS.out<-Spp.quant.out
  save(SSS.output.list,file=paste(filepath,"/SSSoutput.DMP",sep=""))
  save(SSS.out,file=paste(filepath,"/SSS_out.DMP",sep=""))
  
  }
  return(Spp.quant.out)
}

