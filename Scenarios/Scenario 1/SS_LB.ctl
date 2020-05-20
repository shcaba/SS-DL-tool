#V3.30
#C file created using the SS_writectl function in the R package r4ss
#C file write time: 2020-05-19 16:36:41
#
0 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
4 # recr_dist_method for parameters
1 # not yet implemented; Future usage:Spawner-Recruitment; 1=global; 2=by area
1 # number of recruitment settlement assignments 
0 # unused option
# for each settlement assignment:
#_GPattern	month	area	age
1	1	1	0	#_recr_dist_pattern1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern
# begin and end years of blocks
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
0 0 0 0 0 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
#_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
0 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
 1e-03	   2.00	 0.12300000	-2.92000000	 0.22	3	 -1	0	0	0	0	0	0	0	#_NatM_p_1_Fem_GP_1        
 0e+00	 100.00	 0.00000000	 0.00000000	10.00	0	 -3	0	0	0	0	0	0	0	#_L_at_Amin_Fem_GP_1       
 1e+00	1000.00	55.00000000	55.00000000	10.00	0	 -2	0	0	0	0	0	0	0	#_L_at_Amax_Fem_GP_1       
 1e-03	   2.00	 0.19000000	 0.19000000	 0.05	0	 -3	0	0	0	0	0	0	0	#_VonBert_K_Fem_GP_1       
 3e-02	   5.00	 0.10000000	 0.10000000	 0.50	0	 -4	0	0	0	0	0	0	0	#_CV_young_Fem_GP_1        
 3e-02	   5.00	 0.10000000	 0.10000000	 0.50	0	 -4	0	0	0	0	0	0	0	#_CV_old_Fem_GP_1          
 0e+00	   3.00	 0.00001000	 0.00001003	99.00	0	-99	0	0	0	0	0	0	0	#_Wtlen_1_Fem_GP_1         
 2e+00	   4.00	 3.10260000	 3.10260000	99.00	0	-99	0	0	0	0	0	0	0	#_Wtlen_2_Fem_GP_1         
 1e-04	1000.00	47.00000000	47.00000000	99.00	0	-99	0	0	0	0	0	0	0	#_Mat50%_Fem_GP_1          
-2e+00	   4.00	-0.58888780	-0.58888780	99.00	0	-99	0	0	0	0	0	0	0	#_Mat_slope_Fem_GP_1       
-3e+00	   3.00	 1.00000000	 1.00000000	 0.80	0	 -3	0	0	0	0	0	0	0	#_Eggs/kg_inter_Fem_GP_1   
-3e+00	   3.00	 0.00000000	 0.00000000	 0.80	0	 -3	0	0	0	0	0	0	0	#_Eggs/kg_slope_wt_Fem_GP_1
 1e-03	   2.00	 0.12300000	-2.92000000	 0.22	3	 -1	0	0	0	0	0	0	0	#_NatM_p_1_Mal_GP_1        
 0e+00	 100.00	 0.00000000	 0.00000000	10.00	0	 -3	0	0	0	0	0	0	0	#_L_at_Amin_Mal_GP_1       
 1e+00	1000.00	55.00000000	55.00000000	10.00	0	 -2	0	0	0	0	0	0	0	#_L_at_Amax_Mal_GP_1       
 1e-03	   2.00	 0.19000000	 0.19000000	 0.05	0	 -3	0	0	0	0	0	0	0	#_VonBert_K_Mal_GP_1       
 3e-02	   5.00	 0.10000000	 0.10000000	 0.50	0	 -4	0	0	0	0	0	0	0	#_CV_young_Mal_GP_1        
 3e-02	   5.00	 0.10000000	 0.10000000	 0.50	0	 -4	0	0	0	0	0	0	0	#_CV_old_Mal_GP_1          
 0e+00	   3.00	 0.00001003	 0.00001003	99.00	0	-99	0	0	0	0	0	0	0	#_Wtlen_1_Mal_GP_1         
 2e+00	   4.00	 3.10390000	 3.10390000	99.00	0	-99	0	0	0	0	0	0	0	#_Wtlen_2_Mal_GP_1         
 1e-01	  10.00	 1.00000000	 1.00000000	 1.00	0	 -1	0	0	0	0	0	0	0	#_CohortGrowDev            
 1e-02	   0.99	 0.50000000	 0.50000000	 0.50	0	-99	0	0	0	0	0	0	0	#_FracFemale_GP_1          
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; 2=Ricker; 3=std_B-H; 4=SCAA;5=Hockey; 6=B-H_flattop; 7=survival_3Parm;8=Shepard_3Parm
0 # 0/1 to use steepness in initial equ recruitment calculation
0 # future feature: 0/1 to make realized sigmaR a function of SR curvature
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn # parm_name
 1.0	15.0	9.0	9.0	 9.00	0	  1	0	0	0	0	0	0	0	#_SR_LN(R0)  
 0.2	 1.0	0.7	0.7	 0.24	3	 -1	0	0	0	0	0	0	0	#_SR_BH_steep
 0.5	 1.2	0.7	0.7	99.00	0	 -6	0	0	0	0	0	0	0	#_SR_sigmaR  
-5.0	 5.0	0.0	0.0	99.00	0	-99	0	0	0	0	0	0	0	#_SR_regime  
 0.0	 2.0	0.0	1.0	99.00	0	-99	0	0	0	0	0	0	0	#_SR_autocorr
#_no timevary SR parameters
0 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1984 # first year of main recr_devs; early devs can preceed this era
2018 # last year of main recr_devs; forecast devs start in following year
-1 #_recdev phase
1 # (0/1) to read 13 advanced options
0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
-4 #_recdev_early_phase
-4 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
1984 #_last_yr_nobias_adj_in_MPD; begin of ramp
1984 #_first_yr_fullbias_adj_in_MPD; begin of plateau
2018 #_last_yr_fullbias_adj_in_MPD
2018 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-5 #min rec_dev
5 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
#Fishing Mortality info
0.03 # F ballpark
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
100 # max F or harvest rate, depends on F_Method
4 # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE
0	100	1e-06	1	999	0	-1	#_InitF_seas_1_flt_1Fishery1
0	100	1e-06	1	999	0	-1	#_InitF_seas_1_flt_2Fishery2
#
#_Q_setup for fleets with cpue or survey data
-9999 0 0 0 0 0 # terminator
#_Q_parms(if_any);Qunits_are_ln(q)
#_no timevary Q parameters
#
#_size_selex_patterns
#_Pattern	Discard	Male	Special
24	0	0	0	#_1 Fishery1
24	0	0	0	#_2 Fishery2
#
#_age_selex_patterns
#_Pattern	Discard	Male	Special
10	0	0	0	#_1 Fishery1
10	0	0	0	#_2 Fishery2
#
#_SizeSelex
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
  26	64	 50.00000	 50.00000	99	0	 1	0	0	0	0	0	0	0	#_SizeSel_P_1_Fishery1(1)
 -15	15	 15.00000	 15.00000	99	0	-1	0	0	0	0	0	0	0	#_SizeSel_P_2_Fishery1(1)
  -4	12	  3.58539	  3.58539	99	0	 1	0	0	0	0	0	0	0	#_SizeSel_P_3_Fishery1(1)
 -10	 6	 15.00000	 15.00000	99	0	-1	0	0	0	0	0	0	0	#_SizeSel_P_4_Fishery1(1)
-999	15	-15.00000	-10.00000	99	0	-2	0	0	0	0	0	0	0	#_SizeSel_P_5_Fishery1(1)
  -5	15	 15.00000	 15.00000	99	0	-1	0	0	0	0	0	0	0	#_SizeSel_P_6_Fishery1(1)
  26	64	 50.00000	 50.00000	99	0	 1	0	0	0	0	0	0	0	#_SizeSel_P_1_Fishery2(2)
 -15	15	 15.00000	 15.00000	99	0	-1	0	0	0	0	0	0	0	#_SizeSel_P_2_Fishery2(2)
  -4	12	  3.58539	  3.58539	99	0	 1	0	0	0	0	0	0	0	#_SizeSel_P_3_Fishery2(2)
 -10	 6	 15.00000	 15.00000	99	0	-1	0	0	0	0	0	0	0	#_SizeSel_P_4_Fishery2(2)
-999	15	-15.00000	-10.00000	99	0	-2	0	0	0	0	0	0	0	#_SizeSel_P_5_Fishery2(2)
  -5	15	 15.00000	 15.00000	99	0	-1	0	0	0	0	0	0	0	#_SizeSel_P_6_Fishery2(2)
#_AgeSelex
#_No age_selex_parm
#_no timevary selex parameters
#
0 #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
# Tag loss and Tag reporting parameters go next
0 # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# Input variance adjustments factors: 
#_Factor Fleet Value
-9999 1 0 # terminator
#
15 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 2 changes to default Lambdas (default value is 1.0)
#_like_comp	fleet	phase	value	sizefreq_method
    8	1	1	1	1	#_catch_Fishery_Phz1                                                                          
    9	1	1	0	1	#_init_equ_catch_Fishery_lambda_for_init_equ_catch_can_only_enable/disable for_all_fleets_Phz1
-9999	0	0	0	0	#_terminator                                                                                  
#
0 # 0/1 read specs for more stddev reporting
#
999
