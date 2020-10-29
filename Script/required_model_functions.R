
### FUNCTION TO GENERATE BETWEEN-DISTRICT MOVEMENT MATRIX FOR A PARTICULAR SCENARIO AS INPUT TO METAPOP MODEL ACCOUNTING FOR CHANGING MOBILITY OVER TIME
# This function has an outputs of:
# 1) timing vector as an input for metapopulation model; 2) array of daily movement matrices for mild case; 3) array of daily movement matrices for cases with symptoms; 4) array of transposed daily movement matrices for susceptible populations in the metapopulation model
# The inputs are:
# 1) important dates; 2) lockdown matrix (NOT USED in this study = NULL); 3) processed google mobility data;
# 4) OR for Ramadan and Non-Ramadan as in how much reduction in between-district movements caused by, for example, 50% reduction in google mobility measures
# 5) movement_calibration as manual calibration from estimated CDRs movement (value=0-1, NOT USED in this study = 0)
# 6) reduction_pct_sick_case as manual movement reduction for cases with symptoms from estimated CDRs movement (value=0-1, NOT USED in this study = 0)
# 7) reduction_pct_sick_mild as manual movement reduction for cases with no symptoms/mild from estimated CDRs movement (value=0-1, NOT USED in this study = 0)
# 8) reduction_pct_init + outside_java as manual movement reduction during lockdown for before new normal period from estimated CDRs movement (value=0-1, NOT USED in this study = 0)
# 9) reduction_pct_nn + outside_java as manual movement reduction during lockdown for the new normal period from estimated CDRs movement (value=0-1, NOT USED in this study = 0)
# 10) reduction_pct_ramadan_all manual movement reduction during ramadan from estimated CDRs movement (value=0-1, NOT USED in this study = 0)
movement_matrix_process <- function(init_date,last_date,ramadan_start_date,ramadan_end_date,
                                    lockdown_matrix_loc=NULL,lockdown_init_date,google_mobility,admin_df,
                                    mobility_OR,mobility_OR_ramadan,normal_mtx,ramadan_mtx,
                                    movement_calibration_pct=0,reduction_pct_sick_case=0,reduction_pct_sick_mild=0,
                                    reduction_pct_init=0,reduction_pct_nn=0,
                                    reduction_pct_outside_java_init=0,reduction_pct_outside_java_nn=0,reduction_pct_ramadan_all=0,dt=0.1){
  
  
  ## FIND TIMINGS OF RAMADAN AND NON-RAMADAN PERIODS
  time_period <- as.numeric(last_date-init_date+1) # time period to run over
  all_simulations_dates <- seq(init_date,last_date,by="days")
  ramadan_dates <- seq(ramadan_start_date,ramadan_end_date,by="days")
  ramadan_index <- match(ramadan_dates,all_simulations_dates)
  new_normal_index <- match(ymd("2020-06-08"),all_simulations_dates)
  
  # Read lockdown matrix - NOT USED in this study = NULL#
  if (is.null(lockdown_matrix_loc)){
    all_lockdown_matrix <- matrix(0,nrow(normal_mtx),time_period)
  } else{
    lockdown_matrix <- read.csv(lockdown_matrix_loc,header = TRUE,stringsAsFactors = FALSE) # lockdown_matrix.csv
    lockdown_matrix_plain <- as.matrix(lockdown_matrix[,-c(1,2,3)])
    colnames(lockdown_matrix_plain) <- NULL
    pre_lockdown_dates <- seq(init_date,(lockdown_init_date-1),by="days")
    pre_lockdown_matrix <- matrix(0,nrow(lockdown_matrix),length(pre_lockdown_dates))
    all_lockdown_matrix <- cbind(pre_lockdown_matrix,lockdown_matrix_plain)
  }
  
  # INCORPORATE GOOGLE MOBILITY CHANGES and calculate the between-district movement reductions from google mobility based on the assumed Odds Ratio
  pre_google_mobility_dates <- seq(init_date,ymd("2020-02-14"),by="days")
  pre_google_mobility_matrix <- matrix(0,nrow(normal_mtx),length(pre_google_mobility_dates))
  google_mobility_java_updated_to_matrix <- google_mobility %>% select(prov,date,mov_reduce) %>% mutate(mov_reduce=mov_reduce/100)
  google_mobility_df_long <- admin_df %>% select(prov=PROVINCE,district=REGION) %>% left_join(google_mobility_java_updated_to_matrix)
  google_mobility_df <- google_mobility_df_long %>% pivot_wider(names_from=date,values_from=mov_reduce)
  google_mobility_matrix <- as.matrix(google_mobility_df[,-c(1,2)])
  colnames(google_mobility_matrix) <- NULL
  all_google_mobility_matrix <- cbind(pre_google_mobility_matrix,google_mobility_matrix)
  ramadan_google_mobility_matrix <- all_google_mobility_matrix[,ramadan_index]
  all_google_mobility_matrix_calib <- mobility_OR * all_google_mobility_matrix/(1-all_google_mobility_matrix)
  ramadan_google_mobility_matrix_calib <- mobility_OR_ramadan * ramadan_google_mobility_matrix/(1-ramadan_google_mobility_matrix)
  all_google_mobility_matrix <- all_google_mobility_matrix_calib/(1+all_google_mobility_matrix_calib)
  ramadan_google_mobility_matrix <- ramadan_google_mobility_matrix_calib/(1+ramadan_google_mobility_matrix_calib)
  all_google_mobility_matrix[,ramadan_index] <- ramadan_google_mobility_matrix
  
  # Create an initial normal movement matrix <- no initial movement calibration as movement_calibration_pct=0 as default
  normal_mtx_calib <- normal_mtx
  for (j in seq_len(nrow(normal_mtx_calib))){ 
    for (k in seq_len(nrow(normal_mtx_calib))){
      if (k != j){
        # sort out the row
        reduction_row <- normal_mtx_calib[j,k] * movement_calibration_pct
        normal_mtx_calib[j,k] <- normal_mtx_calib[j,k] - reduction_row
        normal_mtx_calib[j,j] <- normal_mtx_calib[j,j] + reduction_row
      }
    }
  }
  
  normal_mtx_calib_t <- t(normal_mtx_calib)
  
  # Create an initial ramadan movement matrix <- no initial movement calibration as movement_calibration_pct=0 as default
  ramadan_mtx_calib <- ramadan_mtx
  for (j in seq_len(nrow(ramadan_mtx_calib))){ 
    for (k in seq_len(nrow(ramadan_mtx_calib))){
      if (k != j){
        # sort out the row
        reduction_row <- ramadan_mtx_calib[j,k] * movement_calibration_pct
        ramadan_mtx_calib[j,k] <- ramadan_mtx_calib[j,k] - reduction_row
        ramadan_mtx_calib[j,j] <- ramadan_mtx_calib[j,j] + reduction_row
      }
    }
  }
  
  # Create an initial infected movement matrices for case with symptoms & no-symptoms/mild <- no initial movement calibration as reduction_pct_sick_case=0 and reduction_pct_sick_mild=0 as default
  reduction_pct_sick_case <- reduction_pct_sick_case # 0.7
  movement_matrix_sick_case <- normal_mtx_calib
  for (j in seq_len(nrow(movement_matrix_sick_case))){ # moving through each admin2 lockdown status on each day
    for (k in seq_len(nrow(movement_matrix_sick_case))){
      if (k != j){
        # sort out the row
        reduction_row <- movement_matrix_sick_case[j,k] * reduction_pct_sick_case
        movement_matrix_sick_case[j,k] <- movement_matrix_sick_case[j,k] - reduction_row
        # movement_matrix_sick_case[j,j] <- movement_matrix_sick_case[j,j] + reduction_row
      }
    }
  }
  movement_matrix_sick_case_t <- t(movement_matrix_sick_case)
  
  reduction_pct_sick_mild <- reduction_pct_sick_mild # 0
  movement_matrix_sick_mild <- normal_mtx_calib
  for (j in seq_len(nrow(movement_matrix_sick_mild))){ # moving through each admin2 lockdown status on each day
    for (k in seq_len(nrow(movement_matrix_sick_mild))){
      if (k != j){
        # sort out the row
        reduction_row <- movement_matrix_sick_mild[j,k] * reduction_pct_sick_mild
        movement_matrix_sick_mild[j,k] <- movement_matrix_sick_mild[j,k] - reduction_row
        # movement_matrix_sick_mild[j,j] <- movement_matrix_sick_mild[j,j] + reduction_row
      }
    }
  }
  movement_matrix_sick_mild_t <- t(movement_matrix_sick_mild)
  
  # These lists contain daily movement matrices as inputs for the SEIR metapopulation model, each one for infected (case&mild) and susceptible
  # Matrices for infected people were transposed, to reflect loop by column in the model
  lockdown_mov_matrix_list_case <- list()
  lockdown_mov_matrix_list_mild <- list()
  lockdown_mov_matrix_untransposed_list <- list()
  
  for (i in seq_len(ncol(all_lockdown_matrix))){ # moving through dates; in this study lockdown status doesn't affect anything
    # Choosing initial matrix; Ramadan or Normal
    if (i %in% ramadan_index){
      movement_matrix_calibrated <- ramadan_mtx_calib
    } else {
      movement_matrix_calibrated <- normal_mtx_calib
    }
    
    # Reduction for based on google mobility - THIS STUDY ACCOUNTED ONLY THIS REDUCTION
    for (j in seq_len(nrow(movement_matrix_calibrated))){ 
      reduction_pct_calib <- all_google_mobility_matrix[j,i]
      for (k in seq_len(nrow(movement_matrix_calibrated))){
        if (k != j){
          # sort out the row
          reduction_row <- movement_matrix_calibrated[j,k] * reduction_pct_calib
          movement_matrix_calibrated[j,k] <- movement_matrix_calibrated[j,k] - reduction_row
          # movement_matrix_calibrated[j,j] <- movement_matrix_calibrated[j,j] + reduction_row
        }
      }
    }
    
    # Reduction based on lockdown status - NOT USED IN THE STUDY as reduction_pct_init, reduction_pct_outside_java_init, reduction_pct_nn, reduction_pct_outside_java_nn = 0 as default
    new_matrix <- movement_matrix_calibrated
    if (i < new_normal_index){
      reduction_pct <- reduction_pct_init 
      reduction_pct_outside_java <- reduction_pct_outside_java_init 
    } else {
      reduction_pct <- reduction_pct_nn 
      reduction_pct_outside_java <- reduction_pct_outside_java_nn
    }
    for (j in seq_len(nrow(new_matrix))){
      if (all_lockdown_matrix[j,i]==1){ # if it's on a lockdown
        for (k in seq_len(nrow(new_matrix))){
          if (k != j){
            if (k == 2 | j == 2){
              # sort out the row
              reduction_row <- new_matrix[j,k] * reduction_pct_outside_java
              new_matrix[j,k] <- new_matrix[j,k] - reduction_row
              # new_matrix[j,j] <- new_matrix[j,j] + reduction_row
            } else {
              # sort out the row
              reduction_row <- new_matrix[j,k] * reduction_pct
              new_matrix[j,k] <- new_matrix[j,k] - reduction_row
              # new_matrix[j,j] <- new_matrix[j,j] + reduction_row
            }
          }
        }
        for (k in seq_len(nrow(new_matrix))){
          if (k != j & all_lockdown_matrix[k,i]==0){
            if (k == 2 | j == 2){
              # sort out the column
              reduction_col <- new_matrix[k,j] * reduction_pct_outside_java
              new_matrix[k,j] <- new_matrix[k,j] - reduction_col
              new_matrix[k,k] <- new_matrix[k,k] + reduction_col
            } else {
              # sort out the column
              reduction_col <- new_matrix[k,j] * reduction_pct
              new_matrix[k,j] <- new_matrix[k,j] - reduction_col
              new_matrix[k,k] <- new_matrix[k,k] + reduction_col
            }
          }
        }
      }
    }
    
    # Further reduction during Ramadan - NOT USED IN THIS STUDY as reduction_pct_ramadan_all = 0 as default
    if (i %in% ramadan_index){
      new_matrix_2 <- new_matrix
      for (j in seq_len(nrow(new_matrix_2))){ 
        for (k in seq_len(nrow(new_matrix_2))){
          if (k != j){
            # sort out the row
            reduction_row <- new_matrix_2[j,k] * reduction_pct_ramadan_all
            new_matrix_2[j,k] <- new_matrix_2[j,k] - reduction_row
            # new_matrix_2[j,j] <- new_matrix_2[j,j] + reduction_row
          }
        }
      }
    } else {
      new_matrix_2 <- new_matrix
    }
    
    # Further reduction for infected individuals - NOT USED IN THIS STUDY as reduction_pct_sick_case & reduction_pct_sick_mild = 0 as default
    new_matrix_2_case <- new_matrix_2
    for (j in seq_len(nrow(new_matrix_2_case))){ 
      for (k in seq_len(nrow(new_matrix_2_case))){
        if (k != j){
          # sort out the row
          reduction_row <- new_matrix_2_case[j,k] * reduction_pct_sick_case
          new_matrix_2_case[j,k] <- new_matrix_2_case[j,k] - reduction_row
          # new_matrix_2_case[j,j] <- new_matrix_2_case[j,j] + reduction_row
        }
      }
    }
    new_matrix_2_mild <- new_matrix_2
    for (j in seq_len(nrow(new_matrix_2_mild))){ # moving through each admin2 lockdown status on each day
      for (k in seq_len(nrow(new_matrix_2_mild))){
        if (k != j){
          # sort out the row
          reduction_row <- new_matrix_2_mild[j,k] * reduction_pct_sick_mild
          new_matrix_2_mild[j,k] <- new_matrix_2_mild[j,k] - reduction_row
          # new_matrix_2_mild[j,j] <- new_matrix_2_mild[j,j] + reduction_row
        }
      }
    }
    lockdown_mov_matrix_list_case[[i]] <- t(new_matrix_2_case)
    lockdown_mov_matrix_list_mild[[i]] <- t(new_matrix_2_mild)
    lockdown_mov_matrix_untransposed_list[[i]] <- new_matrix_2
  }
  
  tt_matrix <- c(0,(1:time_period)/dt)
  matrices_set_case <- aperm(array(c(movement_matrix_sick_case_t,as.numeric(unlist(lockdown_mov_matrix_list_case))),
                                   dim=c(dim(movement_matrix_sick_case_t), length(lockdown_mov_matrix_list_case)+1)),
                             c(3, 1, 2))
  matrices_set_mild <- aperm(array(c(movement_matrix_sick_mild_t,as.numeric(unlist(lockdown_mov_matrix_list_mild))),
                                   dim=c(dim(movement_matrix_sick_mild_t), length(lockdown_mov_matrix_list_mild)+1)),
                             c(3, 1, 2))
  matrices_set_untransposed <- aperm(array(c(normal_mtx_calib,as.numeric(unlist(lockdown_mov_matrix_untransposed_list))),
                                           dim=c(dim(normal_mtx_calib), length(lockdown_mov_matrix_untransposed_list)+1)),
                                     c(3, 1, 2))
  
  movement_matrix_list <- list(tt_matrix=tt_matrix,
                               matrices_set_case=matrices_set_case,
                               matrices_set_mild=matrices_set_mild,
                               matrices_set_untransposed=matrices_set_untransposed)
  
  return(movement_matrix_list)
}

### FUNCTION TO OBTAIN REQUIRED PER-CONTACT TRANSMISSION PROBABILITY TO RETURN RT FOR HIGHEST LEVEL OF MOBILITY
get_required_pinf <- function(duration_infectiousness_mild, duration_infectiousness_case, p_hosp_average, R0, contacts) {
  pinf <- R0/(contacts * ((1-p_hosp_average)*duration_infectiousness_mild+p_hosp_average*duration_infectiousness_case))
  return(pinf)
}

### FUNCTION TO CALCULATE HOW WITHIN-DISTRICT-LEVEL Rt IS CHANGING IN RELATIVE TERMS OVER COURSE OF SIMULATION AS INPUT FOR METAPOP MODEL
# The output of this function is the daily timing vector and relative Rt on each day for each district based on simulated Rt estimates (mobility to funerals data)
# Rt relative is the value of Rt on the day, divided by the Rt at the highest level of mobility at each district/province
Rt_rel_process <- function(Rt_df,R0_df,admin_df,init_date,last_date,dt=0.1){
  
  ## Daily relative Rt - as a model input for metapopulation SEIR model - this shdws how tranmissibility changes in each day, relative to the highest level of transmissibility
  # This creates lists of daily relative Rt matrices for each district
  
  prov_dist_names_java <- admin_df %>% select(prov=PROVINCE,district=REGION)
  
  # Rt relative
  Rt_rel_list <- list()
  
  all_dates <- data.frame(date=seq(init_date,last_date,by="days"))
  init_date_mobility <- min(Rt_df$date)
  last_date_mobility <- max(Rt_df$date)
  len_fill_early <- as.numeric(ymd("2020-03-02") - init_date)
  len_early <- as.numeric(init_date_mobility - init_date)
  fill_early <- rep(1,len_early)
  
  # from Rt estimates to relative Rt daily - relative to the Rt estimated at the highest level of mobility (assumed as R0)
  for (i in seq_len(nrow(prov_dist_names_java))){
    prov_dist <- prov_dist_names_java[i,]
    Rt_estim <- Rt_df %>% filter(district==prov_dist$district)
    R0_district <- R0_df %>% filter(district==prov_dist$district) %>% pull(R0)
    Rt_rel_estim <- all_dates %>% left_join(Rt_estim) %>% mutate(prov=prov_dist[,1],district=prov_dist[,2],Rt_rel=Rt/R0_district) %>% 
      select(date,prov,district,Rt_rel)
    Rt_rel_estim$Rt_rel[1:len_early] <- fill_early
    Rt_rel_list[[i]] <- Rt_rel_estim
  }
  
  lockdown_transmission_relative_mobility <- list() # lockdown is just naming - have no really impact in the estimates; only current Rt relative to the highest
  normal_transmission <- rep(1,nrow(R0_df))
  
  for (i in seq_len(nrow(all_dates))){
    new_transmission <- normal_transmission
    for (j in seq_len(nrow(R0_df))){ 
      new_transmission[j] <- Rt_rel_list[[j]]$Rt_rel[i]
    }
    lockdown_transmission_relative_mobility[[i]] <- new_transmission
  }
  
  tt_rel_trans <- c(0,(1:time_period)/dt)
  rel_trans_set <- aperm(array(c(normal_transmission,as.numeric(unlist(lockdown_transmission_relative_mobility))), 
                               dim=c(length(normal_transmission), length(lockdown_transmission_relative_mobility)+1)), 
                         c(2, 1))
  
  Rt_rel_final_list <- list(tt_rel_trans=tt_rel_trans,
                            rel_trans_set=rel_trans_set)
  
  return(Rt_rel_final_list)
}




### SETS-uP AND RUNS METAPOPULATION MODEL FOR GIVEN MOVEMENT MATRIX AND ADMIN-SPECIFIC Rts OVER TIME
# outputs of the metapopulation model are:
# 1) time series of deaths: deaths_time_series
# 2) time series of people currently hospitalised: hospitalisations_time_series
# 3) time series of new cases admitted to the hospital: hospitalisations_daily_time_series
# 4) time series of new cases admitted to the hospital and needing non-critical bed: non_critical_daily_time_series
# 5) time series of new cases admitted to the hospital and needing ICU bed: critical_daily_time_series
# 6) time series of people currently in the ICU: ICU_time_series
# 7) time series of daily new infections: infections_time_series
# 8) total infections at the end of the simulations: infections
# 9) simulation seeds: simulations_seed
metapopulation_model_run_list_new <- function(movement_matrix_list,
                                              Rt_rel_list,pinf_list,region_name,
                                              number_model_runs=25,
                                              prob_hosp_reg,prob_non_severe_death_reg,prob_severe_death_reg,prob_critical_given_hosp_reg,
                                              contacts_reg,contacts_reg_average,
                                              time_period,
                                              N_reg,S0,
                                              ICase0_list,replicates=1,randomised_seed=FALSE,saved_seed=FALSE) {
  
  N_reg <- N_reg # number of admin units
  dt <- 0.1 # timestep
  S0 <- S0 # number of starting susceptibles
  E0 <- rep(0, N_reg) # number of starting exposed 
  # ICase0 <- ICase0
  IMild0 <- rep(0, N_reg) # number of starting infectious mild
  R0 <- rep(0, N_reg) # number of starting recovered
  dur_E <- 4.6 # mean duration of incubation period
  dur_IMild <- 2.1 # mean duration of infectious period for mild case
  dur_ICase <- 4.5 # 4.5 # mean duration of infectious period for severe case
  p_hosp <- prob_hosp_reg
  p_hosp_average <- mean(p_hosp)
  delay_onset_hospital <- round(4.51) # 7.62  # time between symptom onset (assumed E->I transition) and hospital admission
  time_in_hospital_recovery <- round(18) # 17.4  # duration of hospitalisation if not requiring critical care
  time_in_hospital_death <- round(9.64) # 9.92  # duration of hospitalisation if not requiring critical care
  time_in_critical_care_recovery <- round(28) # 21  # duration of hospitalisation if requiring critical care
  time_in_critical_care_death <- round(7)  # duration of hospitalisation if requiring critical care
  delay_onset_death <- round(21)  # time between symtom onset (assumed E->I transition) and death
  replicates <- replicates # number of replicates of stochastic model
  
  number_model_runs <- number_model_runs ## number of model runs
  infections <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  infections_mild <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  infections_case <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  critical_care <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  non_critical_care <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  non_severe_recoveries_total <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  severe_recoveries_total <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  non_severe_deaths_total <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  severe_deaths_total <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total infections per run
  hospital_stays <- matrix(nrow = number_model_runs, ncol = N_reg) # tracking total hospital (normal + ICU) bed occupancy in person days per run
  ICU_stays <-  matrix(nrow = number_model_runs, ncol = N_reg) # tracking total ICU bed occupancy in person days per run
  deaths_total <-  matrix(nrow = number_model_runs, ncol = N_reg) # tracking total ICU bed occupancy in person days per run
  
  ##initialise lists 
  hospitalisations_time_series <- list()
  ICU_time_series <- list()
  hospitalisations_daily_time_series <- list()
  non_critical_daily_time_series <- list()
  critical_daily_time_series <- list()
  deaths_time_series <- list()
  infections_time_series <- list()
  #initialise storage for timeseries of each run in each admin unit
  for (i in 1:(N_reg)){
    hospitalisations_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
    ICU_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
    hospitalisations_daily_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
    non_critical_daily_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
    critical_daily_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
    deaths_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
    infections_time_series[[i]] <- matrix(nrow=time_period,ncol=number_model_runs)
  }
  #set seeds assess whether reproducing a previous run
  if(randomised_seed==FALSE | saved_seed==FALSE){
    simulations_seed <- round(runif(n = number_model_runs,min = 1,max = 1000000))
    print("Simulation seeds were randomly drawn")
  } else if (length(saved_seed)!=number_model_runs){
    simulations_seed <- round(runif(n = number_model_runs,min = 1,max = 1000000))
    print("Simulation seeds were randomly drawn")
  } else {
    simulations_seed <- saved_seed
    print("Using saved simulation seeds")
  }
  # Running the model for each spline fit
  for(z in seq_len(number_model_runs)){
    set.seed(simulations_seed[z])
    model_output <- run_SEEIR_metapopulation_model(dt = dt, N_reg = N_reg, S0 = S0, E0 = E0, ICase0 = ICase0_list[[z]], IMild0 = IMild0, R0 = R0,
                                                   dur_E = dur_E, dur_IMild = dur_IMild, dur_ICase = dur_ICase, 
                                                   rel_trans_set = Rt_rel_list[[z]]$rel_trans_set, 
                                                   tt_rel_trans = Rt_rel_list[[z]]$tt_rel_trans, 
                                                   matrices_set_case = movement_matrix_list$matrices_set_case, 
                                                   matrices_set_mild = movement_matrix_list$matrices_set_mild, 
                                                   tt_matrix_case = movement_matrix_list$tt_matrix, tt_matrix_mild = movement_matrix_list$tt_matrix, 
                                                   matrices_set_susceptible = movement_matrix_list$matrices_set_untransposed, 
                                                   tt_matrix_susceptible = movement_matrix_list$tt_matrix, 
                                                   plot = FALSE,
                                                   contacts_reg = contacts_reg, contacts_reg_average = contacts_reg_average,
                                                   time_period = time_period, 
                                                   replicates = replicates, p_hosp = p_hosp, pinf = pinf_list[[z]])
    
    incidence_infections_case <- model_output$infection_incidence_case
    daily_incidence_case <- get_daily_incidence_reg(incidence_infections_case,region_name)
    
    # Processing and Storing Incidence of ALL Infections
    incidence_infections <- model_output$infection_incidence
    daily_incidence <- get_daily_incidence_reg(incidence_infections,region_name)
    infections[z,] <- colSums(daily_incidence)
    for (x in 1:(N_reg)){
      incidence_daily <- daily_incidence %>% pull(x)
      infections_time_series[[x]][,z] <- incidence_daily
    }
    
    # Processing and Storing Incidence of Mild Infections
    incidence_infections_mild <- model_output$infection_incidence_mild
    daily_incidence_mild <- get_daily_incidence_reg(incidence_infections_mild,region_name)
    
    
    # Processing and Storing Incidence of Case Infections
    incidence_infections_case <- model_output$infection_incidence_case
    daily_incidence_case <- get_daily_incidence_reg(incidence_infections_case,region_name)
    hospital_incidence <- as.matrix(daily_incidence_case)
    
    
    delay_floor <- delay_onset_hospital
    # Calculating and Storing ICU Admissions
    raw_ICU_incidence <- Map(calc_ICU, seq_len(ncol(daily_incidence)), prob_critical_given_hosp_reg, MoreArgs = list(hospitalisation_incidence = hospital_incidence))
    ICU_incidence <- matrix(unlist(raw_ICU_incidence), ncol = N_reg, byrow = FALSE)
    non_ICU_incidence <- hospital_incidence - ICU_incidence
    
    df_add <- matrix(0,nrow=delay_floor,ncol=115)
    hospital_incidence_timing <- rbind(df_add,head(hospital_incidence,-delay_floor))
    ICU_incidence_timing <- rbind(df_add,head(ICU_incidence,-delay_floor))
    non_ICU_incidence_timing <- rbind(df_add,head(non_ICU_incidence,-delay_floor))
    
    for (x in 1:(N_reg)){
      hospitalisations_daily_time_series[[x]][,z] <- hospital_incidence_timing[,x]
      non_critical_daily_time_series[[x]][,z] <- non_ICU_incidence_timing[,x]
      critical_daily_time_series[[x]][,z] <- ICU_incidence_timing[,x]
    }
    
    # calculate total deaths
    raw_non_severe_death <- Map(calc_deaths, seq_len(ncol(daily_incidence)), prob_non_severe_death_reg, MoreArgs = list(infection_incidence = non_ICU_incidence))
    non_severe_death <- matrix(unlist(raw_non_severe_death), ncol = N_reg, byrow = FALSE)
    non_severe_recovery <- non_ICU_incidence - non_severe_death
    raw_severe_death <- Map(calc_deaths, seq_len(ncol(daily_incidence)), prob_severe_death_reg, MoreArgs = list(infection_incidence = ICU_incidence))
    severe_death <- matrix(unlist(raw_severe_death), ncol = N_reg, byrow = FALSE)
    severe_recovery <- ICU_incidence - severe_death
    
    for (x in 1:(N_reg)){
      # Generating Estimates of bed occupancy
      hosp_and_deaths <- generate_hosp_req_and_deaths_reg_new(delay_onset_hospital = delay_onset_hospital,
                                                              time_in_hospital_recovery = time_in_hospital_recovery, 
                                                              time_in_hospital_death = time_in_hospital_death, 
                                                              time_in_critical_care_recovery = time_in_critical_care_recovery, 
                                                              time_in_critical_care_death = time_in_critical_care_death, 
                                                              non_severe_recovery = non_severe_recovery[,x], 
                                                              severe_recovery = severe_recovery[,x], 
                                                              non_severe_death = non_severe_death[,x], 
                                                              severe_death = severe_death[,x],
                                                              time_period = time_period)
      non_severe_recovery_demand <- apply(hosp_and_deaths$non_severe_recovery_matrix, 2, sum)
      non_severe_death_demand <- apply(hosp_and_deaths$non_severe_death_matrix, 2, sum)
      hosp_demand <- non_severe_recovery_demand + non_severe_death_demand
      hospitalisations_time_series[[x]][,z] <- hosp_demand
      
      severe_recovery_demand <- apply(hosp_and_deaths$severe_recovery_matrix, 2, sum)
      severe_death_demand <- apply(hosp_and_deaths$severe_death_matrix, 2, sum)
      ICU_demand <- severe_recovery_demand + severe_death_demand
      ICU_time_series[[x]][,z] <- ICU_demand
      
      non_severe_death_timing_sum <- apply(hosp_and_deaths$non_severe_death_matrix_timing, 2, sum)
      severe_death_timing_sum <- apply(hosp_and_deaths$severe_death_matrix_timing, 2, sum)
      all_death_timing_sum <- non_severe_death_timing_sum + severe_death_timing_sum
      deaths_time_series[[x]][,z] <- all_death_timing_sum
      
    }
    
    print(z)  
  }
  ##store relevant outputs
  model_output_list <- list(deaths_time_series=deaths_time_series,
                            hospitalisations_time_series=hospitalisations_time_series,
                            hospitalisations_daily_time_series=hospitalisations_daily_time_series,
                            non_critical_daily_time_series=non_critical_daily_time_series,
                            critical_daily_time_series=critical_daily_time_series,
                            ICU_time_series=ICU_time_series,
                            infections_time_series=infections_time_series,
                            infections=infections,
                            simulations_seed=simulations_seed)
  
  return(model_output_list)
}

### RUNS THE METAPOPULATION MODEL
run_SEEIR_metapopulation_model <- function(dt, N_reg, S0, E0, ICase0, IMild0, R0, dur_E, dur_IMild, dur_ICase, rel_trans_set, tt_rel_trans, 
                                           matrices_set_case, matrices_set_mild, tt_matrix_case, tt_matrix_mild, 
                                           matrices_set_susceptible, tt_matrix_susceptible, plot, contacts_reg, contacts_reg_average,
                                           time_period, replicates, p_hosp, pinf) {
 
  # Load Model Instance
  basic_SEEIR <- odin::odin("Script/Model/SEEIR_Stochastic_Metapopulation_Model_Updated.R")
 
  # Convert and Generate Parameters As Required
  gamma_E <- 2 * 1/dur_E
  gamma_IMild <- 1/dur_IMild
  gamma_ICase <- 1/dur_ICase
  
  
  # Collate Parameters Into List
  pars <- list(S0 = S0, E0 = E0, ICase0 = ICase0, IMild0 = IMild0, R0 = R0, 
               gamma_E = gamma_E, gamma_IMild = gamma_IMild, gamma_ICase = gamma_ICase,
               tt_rel_trans = tt_rel_trans, rel_trans_set = rel_trans_set,
               N_reg = N_reg, contacts_reg = contacts_reg,
               mix_mat_set_case = matrices_set_case, mix_mat_set_mild = matrices_set_mild, tt_matrix_case = tt_matrix_case, tt_matrix_mild = tt_matrix_mild, 
               mix_mat_set_susceptible = matrices_set_susceptible, tt_matrix_susceptible = tt_matrix_susceptible, 
               dt = dt, p_hosp = p_hosp, pinf = pinf)
  
  # Running the Model
  mod <- basic_SEEIR(user = pars)
  t <- seq(from = 1, to = time_period/dt)
  tmp <- mod$run(t, replicate = replicates)
  results <- mod$transform_variables(tmp)
  
  # Different Output Depending On Number Replicates Required
  if (replicates == 1) {
    S <- apply(results$S[, , 1], 1, sum)
    E <- apply(results$E1[, , 1], 1, sum) + apply(results$E2[, , 1], 1, sum)
    I <- apply(results$I[, , 1], 1, sum)
    IMild <- apply(results$IMild[, , 1], 1, sum)
    ICase <- apply(results$ICase[, , 1], 1, sum)
    R <- apply(results$R[, , 1], 1, sum)
    
    # Outputs
    epidemic_final_size <- max(R)/max(S)
    incidence_infections <- data.frame(time = results$time, incidence = results$n_EI[, , 1])
    incidence_infections_mild <- data.frame(time = results$time, incidence = results$n_EIMild[, , 1])
    incidence_infections_case <- data.frame(time = results$time, incidence = results$n_EICase[, , 1])
    
    model_output <- data.frame(t = results$time, S = S, E = E, I = I, IMild = IMild, ICase = ICase, R = R)
    model_output <- model_output %>%
      gather(class, value, - t)
    
    # Plotting
    if (plot == TRUE) {
      plot(apply(results$R, 2, max)/apply(results$S, 2, max), type = "l", ylim = c(0, 1))
      a <- ggplot(model_output, aes(x = t, y = value, colour = class)) +
        geom_line(size = 2) +
        labs(x = "Time (Days)", y = "") +
        theme_bw() +
        lims(y = c(0, max(model_output$value))) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              legend.position = "right", legend.title = element_blank(),
              axis.title.y = element_text(size = 13.5, vjust = +3), axis.title.x = element_text(size = 13.5, vjust = +0),
              plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), legend.text = element_text(size = 12),
              legend.background = element_rect(fill = NA), axis.text = element_text(size = 14, face = "bold"))
      print(a)
    }
    return(list(final_size = epidemic_final_size,
                infection_incidence = incidence_infections,
                infection_incidence_mild = incidence_infections_mild,
                infection_incidence_case = incidence_infections_case,
                model_output_full = model_output))
  } else {
    output_list <- list()
    output_mild_list <- list()
    output_case_list <- list()
    for (j in 1:replicates) {
      output_list[[j]] <- results$n_EI[, , j]
      output_mild_list[[j]] <- results$n_EIMild[, , j]
      output_case_list[[j]] <- results$n_EICase[, , j]
      time <- results$time[, 1]
    }
    return(list(infection_incidence = output_list,
                infection_incidence_mild = output_mild_list,
                infection_incidence_case = output_case_list,
                time = time))
  }
}

#returns daily incidence (cases, infections etc.) by region
get_daily_incidence_reg <- function(incidence_infections,region_names) {
   colnames(incidence_infections) <- c("time", region_names)
  incidence_infections <- incidence_infections %>%
    mutate(day = ceiling(time)) %>%
    select(day, everything(), -time) %>%
    gather(region_name, value, -day) %>%
    mutate(region_name = factor(region_name, levels = region_names)) %>%
    group_by(region_name, day) %>%
    summarise(incidence = sum(value)) %>%
    spread(region_name, incidence) %>%
    select(-day)
}

### Post-processing of the metapopulation model output: generating the timing of deaths and recovery of each hospitalised case
# The general idea: outcome of each individual that was hospitalised (whether in critical or non-critical care) was already known from the metapop model output
# This function generates the time span of each individuals in critical/non-critical care based on the durations assumed
generate_hosp_req_and_deaths_reg_new <- function(delay_onset_hospital, time_in_hospital_recovery, time_in_hospital_death, 
                                                 time_in_critical_care_recovery, time_in_critical_care_death,
                                                 non_severe_recovery, severe_recovery, non_severe_death, severe_death,
                                                 time_period) {
  
  total_non_severe_recovery <- non_severe_recovery
  non_severe_recovery_duration <- list(mode = "numeric")
  
  total_severe_recovery <- severe_recovery
  severe_recovery_duration <- list(mode = "numeric")
  
  total_non_severe_death <- non_severe_death
  non_severe_death_duration <- list(mode = "numeric")
  
  total_severe_death <- severe_death
  severe_death_duration <- list(mode = "numeric")
  
  ## Minimum and maximum value of the runif is the same - means the durations are constant values
  ## Placeholder for further updates assuming distributions of the durations
  
  ## Creating vectors of durations based on the number of critical/non-critical cases that were recovered/dead for each day
  
  for (i in 1:length(total_non_severe_recovery)) {
    temp_non_severe_recovery_duration <- runif(n = non_severe_recovery[i], min = time_in_hospital_recovery, max = time_in_hospital_recovery)
    temp_non_severe_death_duration <- runif(n = non_severe_death[i], min = time_in_hospital_death, max = time_in_hospital_death)
    temp_severe_recovery_duration <- runif(n = severe_recovery[i], min = time_in_critical_care_recovery, max = time_in_critical_care_recovery)
    temp_severe_death_duration <- runif(n = severe_death[i], min = time_in_critical_care_death, max = time_in_critical_care_death)
    non_severe_recovery_duration[[i]] <- temp_non_severe_recovery_duration
    non_severe_death_duration[[i]] <- temp_non_severe_death_duration
    severe_recovery_duration[[i]] <- temp_severe_recovery_duration
    severe_death_duration[[i]] <- temp_severe_death_duration
  }
  
  # Create matrices of zeroes: row=total cases in critical/non-critical care that were dead/recovered; col=time period of simulations
  # The idea is to fill the cell as 1 when the case was occupying critical/non-critical bed before the case died/recovered
  non_severe_recovery_matrix <- matrix(0, nrow = sum(total_non_severe_recovery), ncol = time_period)
  counter_non_severe_recovery <- 1
  non_severe_death_matrix <- matrix(0, nrow = sum(total_non_severe_death), ncol = time_period)
  counter_non_severe_death <- 1
  severe_recovery_matrix <- matrix(0, nrow = sum(total_severe_recovery), ncol = time_period)
  counter_severe_recovery <- 1
  severe_death_matrix <- matrix(0, nrow = sum(total_severe_death), ncol = time_period)
  counter_severe_death <- 1
  
  # Create matrix of the timing of deaths of each critical/non-critical cases
  # These matrices will be used to create time series output of deaths
  non_severe_death_matrix_timing <- matrix(0, nrow = sum(total_non_severe_death), ncol = time_period)
  counter_non_severe_death_timing <- 1
  severe_death_matrix_timing <- matrix(0, nrow = sum(total_severe_death), ncol = time_period)
  counter_severe_death_timing <- 1
  
  for (i in 1:length(non_severe_recovery_duration)) {
    
    # Filling the matrix for non-critical cases who were recovered
    temp_non_severe_recovery <- non_severe_recovery_duration[[i]]
    number_non_severe_recovery <- length(temp_non_severe_recovery)
    current_time <- i
    if (number_non_severe_recovery == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_non_severe_recovery != 0) {
      for (j in 1:length(temp_non_severe_recovery)) {
        if ((current_time + delay_onset_hospital + temp_non_severe_recovery[j] - 1) >= time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            non_severe_recovery_matrix[counter_non_severe_recovery, current_time:time_period] <- 0
          } else {
            non_severe_recovery_matrix[counter_non_severe_recovery, (current_time + delay_onset_hospital):time_period] <- 1
          }
          counter_non_severe_recovery <- counter_non_severe_recovery + 1
        } else {
          non_severe_recovery_matrix[counter_non_severe_recovery, (current_time + delay_onset_hospital):(current_time + delay_onset_hospital + temp_non_severe_recovery[j] - 1)] <- rep(1, temp_non_severe_recovery[j])
          counter_non_severe_recovery <- counter_non_severe_recovery + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    # Filling the matrix for non-critical cases who were dead
    temp_non_severe_death <- non_severe_death_duration[[i]]
    number_non_severe_death <- length(temp_non_severe_death)
    current_time <- i
    if (number_non_severe_death == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_non_severe_death != 0) {
      for (j in 1:length(temp_non_severe_death)) {
        if ((current_time + delay_onset_hospital + temp_non_severe_death[j] - 1) >= time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            non_severe_death_matrix[counter_non_severe_death, current_time:time_period] <- 0
          } else {
            non_severe_death_matrix[counter_non_severe_death, (current_time + delay_onset_hospital):time_period] <- 1
          }
          counter_non_severe_death <- counter_non_severe_death + 1
        } else {
          non_severe_death_matrix[counter_non_severe_death, (current_time + delay_onset_hospital):(current_time + delay_onset_hospital + temp_non_severe_death[j] - 1)] <- rep(1, temp_non_severe_death[j])
          counter_non_severe_death <- counter_non_severe_death + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    # Filling the matrix for critical cases who were recovered
    temp_severe_recovery <- severe_recovery_duration[[i]]
    number_severe_recovery <- length(temp_severe_recovery)
    current_time <- i
    if (number_severe_recovery == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_severe_recovery != 0) {
      for (j in 1:length(temp_severe_recovery)) {
        if ((current_time + delay_onset_hospital + temp_severe_recovery[j] - 1) >= time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            severe_recovery_matrix[counter_severe_recovery, current_time:time_period] <- 0
          } else {
            severe_recovery_matrix[counter_severe_recovery, (current_time + delay_onset_hospital):time_period] <- 1
          }
          counter_severe_recovery <- counter_severe_recovery + 1
        } else {
          severe_recovery_matrix[counter_severe_recovery, (current_time + delay_onset_hospital):(current_time + delay_onset_hospital + temp_severe_recovery[j] - 1)] <- rep(1, temp_severe_recovery[j])
          counter_severe_recovery <- counter_severe_recovery + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    # Filling the matrix for critical cases who were dead
    temp_severe_death <- severe_death_duration[[i]]
    number_severe_death <- length(temp_severe_death)
    current_time <- i
    if (number_severe_death == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_severe_death != 0) {
      for (j in 1:length(temp_severe_death)) {
        if ((current_time + delay_onset_hospital + temp_severe_death[j] - 1) >= time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            severe_death_matrix[counter_severe_death, current_time:time_period] <- 0
          } else {
            severe_death_matrix[counter_severe_death, (current_time + delay_onset_hospital):time_period] <- 1
          }
          counter_severe_death <- counter_severe_death + 1
        } else {
          severe_death_matrix[counter_severe_death, (current_time + delay_onset_hospital):(current_time + delay_onset_hospital + temp_severe_death[j] - 1)] <- rep(1, temp_severe_death[j])
          counter_severe_death <- counter_severe_death + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    ## Deaths timing
    
    # Filling the matrix for the timing of deaths of non-critical cases
    temp_non_severe_death <- non_severe_death_duration[[i]]
    number_non_severe_death <- length(temp_non_severe_death)
    current_time <- i
    if (number_non_severe_death == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_non_severe_death != 0) {
      for (j in 1:length(temp_non_severe_death)) {
        if ((current_time + delay_onset_hospital + temp_non_severe_death[j] - 1) > time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            non_severe_death_matrix_timing[counter_non_severe_death_timing, current_time:time_period] <- 0
          } else {
            non_severe_death_matrix_timing[counter_non_severe_death_timing, (current_time + delay_onset_hospital):time_period] <- 0
          }
          counter_non_severe_death_timing <- counter_non_severe_death_timing + 1
        } else {
          non_severe_death_matrix_timing[counter_non_severe_death_timing, (current_time + delay_onset_hospital + temp_non_severe_death[j] - 1)] <- 1
          counter_non_severe_death_timing <- counter_non_severe_death_timing + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
    
    # Filling the matrix for the timing of deaths of critical cases
    temp_severe_death <- severe_death_duration[[i]]
    number_severe_death <- length(temp_severe_death)
    current_time <- i
    if (number_severe_death == 0) {
      ## do nothing as no one hospitalised 
    }
    else if (number_severe_death != 0) {
      for (j in 1:length(temp_severe_death)) {
        if ((current_time + delay_onset_hospital + temp_severe_death[j] - 1) > time_period) {
          if((current_time + delay_onset_hospital >= time_period)) {
            severe_death_matrix_timing[counter_severe_death_timing, current_time:time_period] <- 0
          } else {
            severe_death_matrix_timing[counter_severe_death_timing, (current_time + delay_onset_hospital):time_period] <- 0
          }
          counter_severe_death_timing <- counter_severe_death_timing + 1
        } else {
          severe_death_matrix_timing[counter_severe_death_timing, (current_time + delay_onset_hospital + temp_severe_death[j] - 1)] <- 1
          counter_severe_death_timing <- counter_severe_death_timing + 1
        }
      }
    }
    else {
      print("this is a weird condition")
    }
  }
  
  return(list(non_severe_recovery_duration = non_severe_recovery_duration,
              non_severe_recovery_matrix = non_severe_recovery_matrix,
              non_severe_death_duration = non_severe_death_duration,
              non_severe_death_matrix = non_severe_death_matrix,
              severe_recovery_duration = severe_recovery_duration,
              severe_recovery_matrix = severe_recovery_matrix,
              severe_death_duration = severe_death_duration,
              severe_death_matrix = severe_death_matrix,
              non_severe_death_matrix_timing = non_severe_death_matrix_timing,
              severe_death_matrix_timing = severe_death_matrix_timing))
}


### CALCULATES THE INCIDENCE OF NEW ICU ADMISSION BASED ON THE NUMBER OF CASES NEEDS HOSPITALISATIONS
calc_ICU <- function(index, prob_ICU_given_hosp, hospitalisation_incidence) {
  temp <- hospitalisation_incidence[, index]
  ICU_vector <- vector(mode = "numeric", length = length(temp))
  for (i in 1:length(temp)) {
    ICU_vector[i] <- rbinom(n = 1, size = temp[i], prob_ICU_given_hosp)
  }
  return(ICU_vector)
}

### CALCULATES THE INCIDENCE OF DEATH
calc_deaths <- function(index, prob_death, infection_incidence) {
  temp <- unname(unlist(infection_incidence[, index]))
  death_vector <- vector(mode = "numeric", length = length(temp))
  for (i in 1:length(temp)) {
    death_vector[i] <- rbinom(n = 1, size = temp[i], prob_death)
  }
  return(death_vector)
}

## FUNCTION TO CALCULATE THE HOSPITAL BEDS AVAILABILITY PER PERSON FOR FIGURE 4C
hospital_availability_per_person <- function(model_output,
                                             hospital_beds_capacity,
                                             time_period,
                                             date_df){  
  hospital_beds_used <- model_output$hospitalisations_time_series
  new_infections_need_hospital_beds <- model_output$non_critical_daily_time_series
  
  # hospital_beds_capacity <- hospital_beds_region$HOSPITAL_BEDS
  
  hospital_beds_availability_district <- list()
  for (i in c(1,3:115)){
    hospital_beds_availability_rep <- list()
    district_name <- urban_rural_flag_df[i,2]
    prov_name <- urban_rural_flag_df[i,3]
    hosp_beds_available <- hospital_beds_capacity[i] - hospital_beds_used[[i]]
    inf_need_beds <- new_infections_need_hospital_beds[[i]]
    for (j in 1:100){
      hospital_beds_availability_rep[[j]] <- data.frame(time=seq_len(time_period),
                                                        beds_avail=hosp_beds_available[,j],
                                                        need_beds=inf_need_beds[,j],
                                                        prov=prov_name,
                                                        district=district_name,
                                                        rep=j)
    }
    hospital_beds_availability_district[[i]] <- bind_rows(hospital_beds_availability_rep)
  }
  hospital_beds_availability_district_df <- bind_rows(hospital_beds_availability_district) %>% left_join(date_df)
  
  hospital_beds_availability_district_summary <- hospital_beds_availability_district_df %>% group_by(date) %>% 
    summarise(beds_avail_per_person=median(rep(beds_avail,need_beds))) %>% ungroup()
  
  return(list(df=hospital_beds_availability_district_df,summary=hospital_beds_availability_district_summary))
}


## DATA VISUALISATIONS FOR METAPOPULATION MODEL OUTPUT
metapopulation_model_summary_admin1_point_single <- function(model_output_list,
                                                             google_mobility_to_plot,
                                                             time_period,province_names_java,
                                                             init_date,last_date,N_reg,
                                                             cases_df,deaths_df,funerals_df,hospitalised_df,admin_df,reg_exclude,
                                                             max_date=NULL){
  
  deaths_time_series <- model_output_list$deaths_time_series
  hospitalisations_time_series <- model_output_list$hospitalisations_time_series
  hospitalisations_daily_time_series <- model_output_list$hospitalisations_daily_time_series
  ICU_time_series <- model_output_list$ICU_time_series
  
  # GET ADMIN-LEVEL SUMMARIES OF DEATHS, HOSPITALISATIONS AND ICUS
  deaths_ts_admin1 <- list()
  hospitalisations_ts_admin1 <- list()
  hospitalisations_daily_ts_admin1 <- list()
  ICU_ts_admin1 <- list()
  
  date_output <- data.frame(time=1:time_period,date=ymd(seq(init_date,last_date,by="days")))
  date_output <- head(date_output,-2)
  
  for (i in seq_len(N_reg)){
    deaths_ts_admin1[[i]] <- as_tibble(deaths_time_series[[i]]) %>% mutate(time=1:time_period,PROVINCE=admin_df[i,3])
    deaths_ts_admin1[[i]] <- head(deaths_ts_admin1[[i]],-2)
    hospitalisations_ts_admin1[[i]] <- as_tibble(hospitalisations_time_series[[i]]) %>% mutate(time=1:time_period,PROVINCE=admin_df[i,3])
    hospitalisations_ts_admin1[[i]] <- head(hospitalisations_ts_admin1[[i]],-2)
    hospitalisations_daily_ts_admin1[[i]] <- as_tibble(hospitalisations_daily_time_series[[i]]) %>% mutate(time=1:time_period,PROVINCE=admin_df[i,3])
    hospitalisations_daily_ts_admin1[[i]] <- head(hospitalisations_daily_ts_admin1[[i]],-2)
    ICU_ts_admin1[[i]] <- as_tibble(ICU_time_series[[i]]) %>% mutate(time=1:time_period,PROVINCE=admin_df[i,3])
    ICU_ts_admin1[[i]] <- head(ICU_ts_admin1[[i]],-2)
  }
  
  deaths_ts_admin1 <- bind_rows(deaths_ts_admin1)
  deaths_ts_admin1 <- deaths_ts_admin1 %>% filter(!(PROVINCE %in% reg_exclude))
  hospitalisations_ts_admin1 <- bind_rows(hospitalisations_ts_admin1)
  hospitalisations_ts_admin1 <- hospitalisations_ts_admin1 %>% filter(!(PROVINCE %in% reg_exclude))
  hospitalisations_daily_ts_admin1 <- bind_rows(hospitalisations_daily_ts_admin1)
  hospitalisations_daily_ts_admin1 <- hospitalisations_daily_ts_admin1 %>% filter(!(PROVINCE %in% reg_exclude))
  ICU_ts_admin1 <- bind_rows(ICU_ts_admin1)
  ICU_ts_admin1 <- ICU_ts_admin1 %>% filter(!(PROVINCE %in% reg_exclude))
  
  deaths_ts_admin1 <- deaths_ts_admin1 %>% group_by(time,PROVINCE) %>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% ungroup()
  hospitalisations_ts_admin1 <- hospitalisations_ts_admin1 %>% group_by(time,PROVINCE) %>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% ungroup()
  hospitalisations_daily_ts_admin1 <- hospitalisations_daily_ts_admin1 %>% group_by(time,PROVINCE) %>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% ungroup()
  ICU_ts_admin1 <- ICU_ts_admin1 %>% group_by(time,PROVINCE) %>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% ungroup()
  
  deaths_ts_admin1_long <- deaths_ts_admin1 %>% pivot_longer(-c(time,PROVINCE),names_to="draw",values_to="deaths")
  deaths_ts_admin1_long <- date_output %>% left_join(deaths_ts_admin1_long) %>% mutate(prov=PROVINCE) %>% select(time,date,prov,draw,deaths)
  
  hospitalisations_daily_ts_admin1_long <- hospitalisations_daily_ts_admin1 %>% pivot_longer(-c(time,PROVINCE),names_to="draw",values_to="cases")
  hospitalisations_daily_ts_admin1_long <- date_output %>% left_join(hospitalisations_daily_ts_admin1_long) %>% mutate(prov=PROVINCE) %>% 
    select(time,date,prov,draw,cases)
  
  hospitalisations_ts_admin1_long <- hospitalisations_ts_admin1 %>% pivot_longer(-c(time,PROVINCE),names_to="draw",values_to="hospitalised")
  hospitalisations_ts_admin1_long <- date_output %>% left_join(hospitalisations_ts_admin1_long) %>% mutate(prov=PROVINCE) %>% 
    select(time,date,prov,draw,hospitalised)
  ICU_ts_admin1_long <- ICU_ts_admin1 %>% pivot_longer(-c(time,PROVINCE),names_to="draw",values_to="ICU")
  ICU_ts_admin1_long <- date_output %>% left_join(ICU_ts_admin1_long) %>% mutate(prov=PROVINCE) %>% select(time,date,prov,draw,ICU)
  hospitalised_all_ts_admin1_long <- hospitalisations_ts_admin1_long %>%  left_join(ICU_ts_admin1_long) %>% mutate(hospitalised_all=hospitalised+ICU)
  
  if(!is.null(max_date)){
    hospitalisations_daily_ts_admin1_long <- hospitalisations_daily_ts_admin1_long %>% filter(date<max_date)
    cases_df <- cases_df %>% filter(date<max_date)
    hospitalised_all_ts_admin1_long <- hospitalised_all_ts_admin1_long %>% filter(date<max_date)
    hospitalised_df <- hospitalised_df %>% filter(date<max_date)
    deaths_ts_admin1_long <- deaths_ts_admin1_long %>% filter(date<max_date)
    deaths_df <- deaths_df %>% filter(date<max_date)
    funerals_df <- funerals_df %>% filter(date<max_date)
    #google_mobility_add <- google_mobility_add %>% filter(date<max_date)
    # Rt_java_plot_add <- Rt_java_plot_add %>% filter(date<max_date)
  }
  
  max_y_cases <- max(cases_df$cases,na.rm=TRUE)
  max_y_deaths <- max(funerals_df$deaths,na.rm=TRUE)
  max_y_hosp <- max(hospitalised_df$hospitalised,na.rm=TRUE)
  
  p_daily_hospitalisations <- hospitalisations_daily_ts_admin1_long %>%
    group_by(date,prov) %>%
    median_qi(cases, .width = c(.95)) %>% # or curve_interval(cases, .width = c(.5)) %>% median_qi
    ggplot(aes(x = date, y = cases), col="#810F7C") +
    annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
    annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
    annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
    annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) +
    geom_lineribbon(aes(ymin = .lower, ymax = .upper), fill="#810F7C", alpha=0.30) +
    geom_line(data=cases_df,aes(x=date,y=cases_rolling_avg_7),col="black",size=0.75) +
    geom_line(col="#810F7C",size=0.75) +
    scale_x_date(date_breaks="2 months", date_labels = "%b") + coord_cartesian(ylim = c(0, max_y_cases)) + #geom_vline(xintercept = ymd("2020-06-01"), lty=2) +
    facet_wrap(.~prov,nrow=1) + theme_bw() + xlab("Date") + ylab("Daily new reported cases") #+
  
  p_current_hospitalisations <- hospitalised_all_ts_admin1_long %>%
    group_by(date,prov) %>%
    median_qi(hospitalised, .width = c(.95)) %>% # or curve_interval(cases, .width = c(.5)) %>%
    ggplot(aes(x = date, y = hospitalised), col="#2ca25f") +
    annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
    annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
    annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
    annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) +
    geom_lineribbon(aes(ymin = .lower, ymax = .upper), fill="#2ca25f", alpha=0.30) +
    geom_point(data=filter(hospitalised_df, type=="confirmed"),aes(x=date,y=hospitalised),col="black") +
    # geom_line(aes(group = draw), alpha=0.05, data = hospitalised_all_ts_admin1_long, col="#2ca25f") +
    geom_line(col="#2ca25f",size=0.75) +
    scale_x_date(date_breaks="2 months", date_labels = "%b") + coord_cartesian(ylim = c(0, max_y_hosp)) + #geom_vline(xintercept = ymd("2020-06-01"), lty=2) +
    facet_wrap(.~prov,nrow=1) + theme_bw() + xlab("Date") + ylab("Cases currently hospitalised") #+
  # geom_vline(xintercept=ymd(c("2020-03-03","2020-03-15","2020-04-10","2020-04-24","2020-06-08")),lty=2,size=0.5)
  
  p_daily_deaths <- deaths_ts_admin1_long %>%
    group_by(date,prov) %>%
    median_qi(deaths, .width = c(.95)) %>% # or curve_interval(cases, .width = c(.5)) %>%
    ggplot(aes(x = date, y = deaths), col="#BD0026") +
    annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
    annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
    annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
    annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) +
    # geom_point(data=deaths_df,aes(x=date,y=deaths),col="#BD0026") +
    # geom_point(data=funerals_df,aes(x=date,y=deaths),col="#993404") +
    geom_lineribbon(aes(ymin = .lower, ymax = .upper), fill="#BD0026", alpha=0.30) +
    geom_line(data=deaths_df,aes(x=date,y=deaths_rolling_avg_7),col="black",size=0.75) +
    geom_line(data=funerals_df,aes(x=date,y=deaths_rolling_avg_7),col="black",size=0.75,lty=2) +
    # geom_line(aes(group = draw), alpha=0.05, data = deaths_ts_admin1_long, col="#BD0026") +
    geom_line(col="#BD0026",size=0.75) +
    scale_x_date(date_breaks="2 months", date_labels = "%b") + coord_cartesian(ylim = c(0, max_y_deaths)) + #geom_vline(xintercept = ymd("2020-06-01"), lty=2) +
    # geom_line(data=Rt_java_plot_add,aes(y=Rt*40,x=date),col="blue",size=1)  +
    # geom_hline(yintercept = 150,lty=2,col="blue") +
    # scale_y_continuous(sec.axis = sec_axis(~./40, name = "Rt")) +
    facet_wrap(.~prov,nrow=1) + theme_bw() + xlab("Date") + ylab("Daily new reported deaths") #+
  # geom_vline(xintercept=ymd(c("2020-03-03","2020-03-15","2020-04-10","2020-04-24","2020-06-08")),lty=2,size=0.5)
  
 
  fig_list <- list(cases=p_daily_hospitalisations,hospitalised=p_current_hospitalisations,deaths=p_daily_deaths,#mobility=p_movements_daily,combined_fig2=p_2,
                   df_cases_daily=hospitalisations_daily_ts_admin1_long,df_hosp=hospitalised_all_ts_admin1_long,df_deaths_daily=deaths_ts_admin1_long)
  
  return(fig_list)
}

## DATA VISUALISATIONS; COMPARING DIFFERENT ESTIMATED DAILY DEATHS BASED ON DIFFERENT SCENARIOS OF METAPOPULATION MODELS
deaths_compare_scenarios_point_single <- function(model_output_list,scenario_labels,deaths_data,funerals_data,init_date,last_date,admin_df,N_reg,max_date=NULL){
  
  deaths_ts_admin1_list <- list()
  
  date_output <- data.frame(time=1:time_period,date=ymd(seq(init_date,last_date,by="days")))
  date_output <- head(date_output,-2)
  
  for (i in seq_len(length(model_output_list))){
    deaths_ts_admin1_list[[i]] <- list()
    for (j in seq_len(N_reg)){
      admin1_output <- as_tibble(model_output_list[[i]][[j]]) %>% mutate(time=1:time_period,PROVINCE=admin_df[j,3])
      deaths_ts_admin1_list[[i]][[j]] <- head(admin1_output,-2)
    }
  }
  
  deaths_ts_admin1_long <- list()
  
  for (i in seq_len(length(model_output_list))){
    deaths_ts_admin1_df <- bind_rows(deaths_ts_admin1_list[[i]])
    deaths_ts_admin1_df <- deaths_ts_admin1_df %>% group_by(time,PROVINCE) %>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% ungroup()
    deaths_ts_admin1_df_long <- deaths_ts_admin1_df %>% pivot_longer(-c(time,PROVINCE),names_to="draw",values_to="deaths")
    deaths_ts_admin1_long[[i]] <- date_output %>% left_join(deaths_ts_admin1_df_long) %>% mutate(prov=PROVINCE,sc=scenario_labels[i]) %>% 
      select(time,date,prov,draw,deaths,sc)
  }  
  
  deaths_ts_admin1_all_long <- bind_rows(deaths_ts_admin1_long)
  deaths_ts_admin1_all_long <- deaths_ts_admin1_all_long %>% filter(prov!="OUTSIDE JAVA")
  deaths_ts_admin1_all_long$sc <- factor(deaths_ts_admin1_all_long$sc,levels=scenario_labels)
  
  idn_deaths_java_2 <- deaths_data %>% mutate(sc="Data")
  idn_funerals_java_2 <- funerals_data %>% mutate(sc="Data")
  
  color_vec <- c("#1b9e77","#d95f02","#1f78b4","#66a61e","#e6ab02","#a6761d","#fb9a99","#a65628")
  length_sc <- length(scenario_labels)
  
  ramadan_start_date <- ymd("2020-04-24")
  ramadan_end_date <- ymd("2020-06-01")
  
  if(!is.null(max_date)){
    deaths_ts_admin1_all_long <- deaths_ts_admin1_all_long %>% filter(date<max_date)
  }
  
  p_deaths_sc <- deaths_ts_admin1_all_long %>%
    group_by(date,prov,sc) %>%
    median_qi(deaths, .width = c(.75)) %>% # or median_qi(cases, .width = c(.5)) %>% curve_interval
    ggplot(aes(x = date, y = deaths, group=sc, col=sc)) +
    annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
    annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=0, ymax=Inf, alpha=0.5, fill="#00441b") +  
    annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(0,Inf), col="#00441b", lty=2) +
    annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(0,Inf), col="#66c2a4", lty=2) +
    annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(0,Inf), col="#00441b", lty=2) +
    # geom_point(data=idn_deaths_java_2,aes(x=date,y=deaths),col="#BD0026") +
    # geom_point(data=idn_funerals_java_2,aes(x=date,y=deaths),col="#993404") +
    # geom_line(data=idn_deaths_java_2,aes(x=date,y=deaths_rolling_avg_7,col="ZZZZ1",linetype="ZZZZ1"),size=2) +
    # geom_line(data=idn_funerals_java_2,aes(x=date,y=deaths_rolling_avg_7,col="ZZZZ2",linetype="ZZZZ2"),size=2) +
    geom_line(size=2) +
    # annotate("rect", xmin=ramadan_start_date, xmax=ramadan_end_date, ymin=0, ymax=Inf, alpha=0.5, fill="#66c2a4") +
    scale_x_date(date_breaks="2 months", date_labels = "%b") + scale_y_log10() +
    facet_wrap(.~prov,nrow=1) + theme_bw() + xlab("Date") + ylab("Daily new deaths") +
    # geom_vline(xintercept=ymd(c("2020-03-03","2020-03-15","2020-04-10","2020-04-24","2020-06-08")),lty=2,size=0.5) + 
    # scale_linetype_manual(values = c(rep("solid",length_sc),"dashed","dashed"),
    #                       labels = c(scenario_labels[1:length_sc],"7-day rolling avg of deaths (data)","7-day rolling avg of funerals (data)")) +
    # scale_color_manual(values = c(color_vec[1:length_sc],"#BD0026","#993404"),
    #                    labels = c(scenario_labels[1:length_sc],"7-day rolling avg of deaths (data)","7-day rolling avg of funerals (data)")) + 
    scale_color_manual(values = c(color_vec[1:length_sc]),
                       labels = c(scenario_labels[1:length_sc])) + 
    labs(col="Simulation\nscenarios") +
    theme(legend.position = "top") + guides(col=guide_legend(nrow=1,byrow=TRUE))
  
  return(p_deaths_sc)
}
