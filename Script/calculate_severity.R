# Get the population
pop <- get_population("Indonesia")
population_idn <- pop$n
prop_75_80_idn <- population_idn[16]/(population_idn[16]+population_idn[17])

infections_output_summary_list <- list()
# model_output_list <- list()
p_hosp_reg <- vector()
p_hosp_reg_AR_weight <- vector()
p_death_non_critical <- vector()
p_crit_reg <- vector()
ifr_reg <- vector()
seed_reg <- readRDS("Script/Seeds/seed_2_new_squire.rds")
R0_used <- 2.0
c_matrix <- "new_squire"

# Calculations for 75-80 & >80 pop groups
for (i in seq_len(nrow(pop_matrix))){
  pop_reg <- pop_matrix[i,]
  pop_reg[17] <- pop_reg[16]*(1-prop_75_80_idn)
  pop_reg[16] <- pop_reg[16]*(prop_75_80_idn)
  pop_reg <- round(pop_reg)
  names(pop_reg) <- NULL
  
  # Get the mixing matrix
  if (c_matrix == "new_squire"){
    contact_matrix <- get_mixing_matrix("Indonesia")
  } else if (c_matrix == "old_squire"){
    contact_matrix <- get_mixing_matrix("Indonesia")
  }
  
  # run the model
  # seed_model <- round(runif(n = 1,min = 1,max = 1000000))
  seed_model <- seed_reg[i]
  set.seed(seed_model)
  model_output <- run_explicit_SEEIR_model(population = pop_reg/sum(pop_reg)*10000000, 
                                           contact_matrix_set = contact_matrix,
                                           R0 = R0_used, 
                                           time_period = 365,
                                           dt = 0.1,
                                           replicates = 30,
                                           hosp_bed_capacity = sum(pop_reg),
                                           ICU_bed_capacity = sum(pop_reg))
  
  # model_output_list[[i]] <- model_output$output
  infections_output <- format_output(model_output, var_select = c("ICU_incidence","hospital_incidence","n_E2_IMild","delta_D"))
  infections_output_summary <- infections_output %>% group_by(replicate,compartment) %>% summarise(y_total=sum(y)) %>% ungroup() %>% 
    pivot_wider(values_from="y_total",names_from="compartment") %>% mutate(pop=sum(pop_reg)) %>% 
    mutate(prob_hosp=(hospital_incidence+ICU_incidence)/(hospital_incidence+ICU_incidence+n_E2_IMild),prob_crit=ICU_incidence/(hospital_incidence+ICU_incidence),
           ifr=delta_D/(hospital_incidence+ICU_incidence+n_E2_IMild),prob_death_non_severe=(delta_D-0.5*ICU_incidence)/hospital_incidence)
  infections_output_summary_list[[i]] <- infections_output_summary
  p_hosp_reg[i] <- median(infections_output_summary$prob_hosp)
  p_crit_reg[i] <- median(infections_output_summary$prob_crit) 
  ifr_reg[i] <- median(infections_output_summary$ifr)
  seed_reg[i] <- seed_model
  p_death_non_critical[i] <- median(infections_output_summary$prob_death_non_severe)
}

# saveRDS(infections_output_summary_list,paste0("infections_summary_",R0_used,"_",c_matrix,".rds"))
# saveRDS(p_hosp_reg,paste0("p_hosp_",R0_used,"_",c_matrix,".rds"))
# saveRDS(p_crit_reg,paste0("p_crit_",R0_used,"_",c_matrix,".rds"))
# saveRDS(p_death_non_critical,paste0("p_death_non_critical_",R0_used,"_",c_matrix,".rds"))
# saveRDS(ifr_reg,paste0("ifr_",R0_used,"_",c_matrix,".rds"))