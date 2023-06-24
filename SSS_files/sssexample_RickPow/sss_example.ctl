#V3.30.03.05
#_data_and_control_files: 2017pop.dat // 2017pop.ctl
#_SS-V3.30.03.05-safe;_2017_05_11;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_SS-V3.30.03.05-safe;user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_SS-V3.30.03.05-safe;user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  1=like 3.24; 2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # Recruitment: 1=global; 2=by area (future option)
1 #  number of recruitment settlement assignments 
0 # year_x_area_x_settlement_event interaction requested (only for recr_dist_method=1)
#GPat month  area age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_blocks_per_pattern 
# begin and end years of blocks

#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#  autogen
1 1 1 1 1 # autogen
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if min=-12345
# 1st element for biology, 2nd for SR, 3rd for Q, 5th for selex, 4th reserved
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
0 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
0.055 #_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0  #_placeholder for future growth feature
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
0 #_First_Mature_Age
2 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
0.0000001  0.03 0.03 -2.92 0.44  3 -5  0 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1
15  25  20.7718 20.8  10  0 -3  0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
35  45  41.6366 41.4  10  0 -3  0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
0.1 0.4 0.165916  0.166 0.05  0 -3  0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
0.03  5 0.1 1.31  0.5 0 -5  0 0 0 0 0 0 0 # CV_young_Fem_GP_1
0.03  5 0.1 2.68  0.5 0 -5  0 0 0 0 0 0 0 # CV_old_Fem_GP_1
0 3 1.00E-05  1.00E-05  99  0 -99 0 0 0 0 0 0 0 # Wtlen_1_Fem
2 4 3.1026  3.1026  99  0 -99 0 0 0 0 0 0 0 # Wtlen_2_Fem
20  40  32.1  32.1  99  0 -99 0 0 0 0 0 0 0 # Mat50%_Fem
-2  4 -1  -1  99  0 -99 0 0 0 0 0 0 0 # Mat_slope_Fem
0 6 8.66E-10  1 99  0 -99 0 0 0 0 0 0 0 # Eggs_scalar_Fem
-3  10 4.9767  5 99  0 -99 0 0 0 0 0 0 0 # Eggs_exp_len_Fem
0.0000001  0.03 0.03 -2.92 0.44  3 -5  0 0 0 0 0 0 0 # NatM_p_1_Mal_GP_1
15  25  20.7718 20.8  10  0 -3  0 0 0 0 0 0 0 # L_at_Amin_Mal_GP_1
35  45  41.6366 41.4  10  0 -3  0 0 0 0 0 0 0 # L_at_Amax_Mal_GP_1
0.1 0.4 0.165916  0.166 0.05  0 -3  0 0 0 0 0 0 0 # VonBert_K_Mal_GP_1
0.03  5 0.1 1.31  0.5 0 -5  0 0 0 0 0 0 0 # CV_young_Mal_GP_1
0.03  5 0.1 2.68  0.5 0 -5  0 0 0 0 0 0 0 # CV_old_Mal_GP_1
0 3 9.88E-06  9.88E-06  99  0 -99 0 0 0 0 0 0 0 # Wtlen_1_Mal
2 4 3.1039  3.1039  99  0 -99 0 0 0 0 0 0 0 # Wtlen_2_Mal
#0 2 1 1 99  0 -99 0 0 0 0 0 0 0 # RecrDist_GP_1
#0 2 1 1 99  0 -99 0 0 0 0 0 0 0 # RecrDist_Area_1
#0 2 1 1 99  0 -99 0 0 0 0 0 0 0 # RecrDist_Bseas_1
0 2 1 1 99  0 -99 0 0 0 0 0 0 0 # CohortGrowDev
0.01  0.99  0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Spawner-Recruitment
10 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
0.0001 20 9 9 5 0 1 0 0 0 0 0 0 0 # SR_LN_R0
0.2 1 0.632007 0.632007 0.15 2 -2 0 0 0	0 0	0 0	# SR_RkrPower_steep
0 10 0.921123 0.921123 -1 0.8 -4 0 0 0 0 0 0 0	# SR_RkrPower_gamma
           0.5           1.2           0.7           0.7            99             0         -6          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0            99             0        -99          0          0          0          0          0          0          0 # SR_regime
             0             2             0             1            99             0        -99          0          0          0          0          0          0          0 # SR_autocorr
0 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1917 # first year of main recr_devs; early devs can preceed this era
2016 # last year of main recr_devs; forecast devs start in following year
-1 #_recdev phase 
0 # (0/1) to read 13 advanced options
#  0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
# -3 #_recdev_early_phase
# -5 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
# 1 #_lambda for Fcast_recr_like occurring before endyr+1
# -1955 #_last_early_yr_nobias_adj_in_MPD
# -1975 #_first_yr_fullbias_adj_in_MPD
# -2012 #_last_yr_fullbias_adj_in_MPD
# -2014 #_first_recent_yr_nobias_adj_in_MPD
# -1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
# 0 #_period of cycles in recruitment (N parms read below)
# -6 #min rec_dev
# 6 #max rec_dev
# 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#
#Fishing Mortality info 
0.03 # F ballpark
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
2.9 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
5  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
# F rates by fleet
#
#_Q_setup
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         0         0         1  #  Depletion
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -15            15      -3.00892             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Depl
#_no timevary Q parameters
#
#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
 24 0 0 0 # 1 Fishery
 0 0 0 0 # 2 Depl
#
#_age_selex_types
#_Pattern Discard Male Special
 10 0 0 0 # 1 Fishery
 10 0 0 0 # 2 Depl
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           10            91       30.7974            36            99             0         -1          0          0          0          0          0          0          0  #  Size_DblN_peak_Fishery(1)
           -15            15            15            15            99             0         -3          0          0          0          0          0          0          0  #  Size_DblN_top_logit_Fishery(1)
            -4            12       5.15329             3            99             0         -1          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_Fishery(1)
           -10             6            15             4            99             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_Fishery(1)
          -999            15           -15           -10            99             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_Fishery(1)
           -15            20            15            10            99             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_Fishery(1)
## timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
# info on dev vectors created for selex parms are reported with other devs after tag parameter section 
#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
 -9999   1    0  # terminator
#
2 #_maxlambdaphase
1 #_sd_offset
# read 12 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
-9999  1  1  1  1  #  terminator
#
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

