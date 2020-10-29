
# Loading In Data Used In Multiple Analyses - treating admin2 as region - as Jakarta is a province and other admin2 outside Java are treated as a single entity
pop_region <- read.csv("Data/raw/admin2-reg-pop-java-by-age.csv",header = TRUE,stringsAsFactors = FALSE)

### Estimated movement matrices based on CDRs data
matrix_idn <- readRDS("processed_inputs/matrix_idn_java_nonjava_length_of_travel.rds") # processed movement Ramadan and non-Ramadan movement matrices from CDRs data
region_name <- pop_region$REGION
region_name_java <- pop_region$REGION[-2]
region_id_java <- pop_region$IDREGION[-2]

### FLAG FOR WHETHER ADMIN UNIT IS URBAN OR RURAL
urban_rural_flag_df <- read.csv("Data/raw/urban_rural_flag.csv",header = TRUE,stringsAsFactors = FALSE)
urban_rural_flag <- urban_rural_flag_df$URBAN_RURAL
urban_rural_flag_java <- urban_rural_flag_df[-2,]

# NAME ADMIN UNITS
province_name <- c("ACEH","NORTH SUMATERA","WEST SUMATERA","RIAU","JAMBI","SOUTH SUMATERA","BENGKULU","LAMPUNG","BANGKA BELITUNG ISLANDS","RIAU ISLANDS",
                   "JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","BALI","WEST NUSA TENGGARA","EAST NUSA TENGGARA",
                   "WEST KALIMANTAN","CENTRAL KALIMANTAN","SOUTH KALIMANTAN","EAST KALIMANTAN","NORTH KALIMANTAN","NORTH SULAWESI","CENTRAL SULAWESI",
                   "SOUTH SULAWESI","SOUTHEAST SULAWESI","GORONTALO","WEST SULAWESI","MALUKU","NORTH MALUKU","WEST PAPUA","PAPUA")

java_provinces <- c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")

### Data on reported cases from https://corona.jakarta.go.id/id/data-pemantauan and https://kcov.id/daftarpositif
idn_pos <- read.csv("Data/raw/idn_positives.csv",header = TRUE,stringsAsFactors = FALSE)
### Data on reported deaths from https://corona.jakarta.go.id/id/data-pemantauan and https://kcov.id/daftarpositif
idn_deaths <- read.csv("Data/raw/idn_deaths.csv",header = TRUE,stringsAsFactors = FALSE)
### Funeral data for Jakarta from https://corona.jakarta.go.id/id/data-pemantauan, other provinces = suspected + confirmed deaths from WHO sitreps https://www.who.int/indonesia/news/novel-coronavirus/situation-reports
idn_funerals <- read.csv("Data/raw/idn_funerals.csv",header = TRUE,stringsAsFactors = FALSE) 

colnames(idn_pos)[-1] <- province_name
colnames(idn_deaths)[-1] <- province_name
colnames(idn_funerals)[-1] <- province_name

### MERGE DATA WITH ADMIN AND DATE
date_reported <- data.frame(date=seq(ymd("2020-03-15"),ymd("2020-08-05"),by="days")) 
idn_pos <- idn_pos %>% pivot_longer(-date,names_to="prov",values_to="cases") %>% mutate(cases=replace(cases, cases<0, 0))
idn_deaths <- idn_deaths %>% pivot_longer(-date,names_to="prov",values_to="deaths")
idn_funerals <- idn_funerals %>% pivot_longer(-date,names_to="prov",values_to="deaths")
idn_pos$prov <- factor(idn_pos$prov,levels=province_name)
idn_deaths$prov <- factor(idn_deaths$prov,levels=province_name)
idn_funerals$prov <- factor(idn_funerals$prov,levels=province_name)

### FILTER FOR JAVA
idn_pos_java <- idn_pos %>% filter(prov %in% c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")) %>% arrange(prov) 
idn_deaths_java <- idn_deaths %>% filter(prov %in% c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")) %>% arrange(prov) 
idn_funerals_java <- idn_funerals %>% filter(prov %in% c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")) %>% arrange(prov) 
idn_pos_java$date <- ymd(idn_pos_java$date)
idn_deaths_java$date <- ymd(idn_deaths_java$date)
idn_funerals_java$date <- ymd(idn_funerals_java$date)

jkt_data_evaluate_hospitalised <- jkt_data_evaluate %>% mutate(prov="JAKARTA")

java_prov <- c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")

### CALCULATE ROLLING AVERAGE OF METRICS FOR Visualisations in Figure 4A and Extended-Data-Fig-4
roll_period <- 7

idn_pos_roll_7 <- vector()
idn_deaths_roll_7 <- vector()
idn_funerals_roll_7 <- vector()

for (i in seq_len(length(java_prov))){
  df_pos <- idn_pos_java %>% filter(prov==java_prov[i]) %>% drop_na() %>% select(date,cases) %>% 
    mutate_each(funs(replace(., . == 0, 0.000001)), -date)
  rollmean_pos <- c(rep(NA,(roll_period-1)),rollmean(df_pos[,2], roll_period, align = "right", na.rm=TRUE))
  idn_pos_roll_7 <- c(idn_pos_roll_7,rollmean_pos)
  
  df_deaths <- idn_deaths_java %>% filter(prov==java_prov[i]) %>% drop_na() %>% select(date,deaths) %>% 
    mutate_each(funs(replace(., . == 0, 0.000001)), -date)
  rollmean_deaths <- c(rep(NA,(roll_period-1)),rollmean(df_deaths[,2], roll_period, align = "right", na.rm=TRUE))
  idn_deaths_roll_7 <- c(idn_deaths_roll_7,rollmean_deaths)
  
  df_funerals <- idn_funerals_java %>% filter(prov==java_prov[i]) %>% drop_na() %>% select(date,deaths) %>%
    mutate_each(funs(replace(., . == 0, 0.000001)), -date)
  if(java_prov[i]=="JAKARTA"){
    rollmean_funerals <- c(rep(NA,(roll_period-1)),rollmean(df_funerals[,2], roll_period, align = "right", na.rm=TRUE))
  } else{
    rollmean_funerals <- c(rep(NA,84),rollmean(df_funerals[,2], roll_period, align = "right", na.rm=TRUE),rep(NA,17))
  }
  idn_funerals_roll_7 <- c(idn_funerals_roll_7,rollmean_funerals)
}

idn_pos_java$cases_rolling_avg_7 <- idn_pos_roll_7
idn_deaths_java$deaths_rolling_avg_7 <- idn_deaths_roll_7
idn_funerals_java$deaths_rolling_avg_7 <- idn_funerals_roll_7


### READ IN HOSPITALISATION DATA from https://corona.jakarta.go.id/id/data-pemantauan and https://kcov.id/daftarpositif (updated daily)
java_hospitalised <- read.csv("Data/raw/idn_hospitalised.csv", header = TRUE, stringsAsFactors = FALSE)
java_hospitalised_long <- java_hospitalised %>% pivot_longer(-c(date,prov),names_to="type",values_to="hospitalised") %>% mutate(date=dmy(date))
java_hospitalised_long$type <- factor(java_hospitalised_long$type,levels=c("confirmed","suspected"))
province_names_java <- unique(urban_rural_flag_java$PROVINCE)

# LOAD REQUIRED MODEL FUNCTIONS
source("Script/required_model_functions.R")

# Loading in Data and Sourcing Required Functions
# Preparing district-level population matrix by age group
pop_matrix <- data.matrix(pop_region[,3:18])
pop_matrix_national <- colSums(pop_matrix)
N_reg <- nrow(pop_matrix) # number of age groups
pop_reg_java <- rowSums(pop_matrix)[-2] # Vector of total populations by district

# Placeholder for further analysis assuming impact of different contact patterns based on age distributions
# get overal number of contacts by age-group then averaging based on the population age-pattern
# need to consider urban and rural contact matrices
# contact_matrix_urban <- get_mixing_matrix("Indonesia") # based on matrix used in squire
# contact_matrix_rural <- get_mixing_matrix("Indonesia") # based on matrix used in squire

# region level contacts average
# contacts_reg_average <- vector()
# for (i in seq_len(nrow(pop_matrix))){
# if (urban_rural_flag[i] == 1){
# contact_matrix <- contact_matrix_urban
# } else {
#  contact_matrix <- contact_matrix_rural
# }
#  contact_matrix_region <- generate_contact_matrix(pop_matrix[i,], contact_matrix) # per capita rates, standardised for demography
# mixing_matrix_region <- t(t(contact_matrix_region)/pop_matrix[i,]) # divided by population, model input
# contacts_by_age_region <- rowSums(mixing_matrix_region)*pop_matrix[i,]
#  contacts_reg_average[i] <- weighted.mean(contacts_by_age_region,pop_matrix[i,])
# }
### AS DEFAULT DONT ALTER CONTACT RATES BY REGION
contacts_reg_average<-rep(1,nrow(pop_matrix)) # set as 1 for all - as this will cancel out in the model
contacts_reg <- contacts_reg_average/rowSums(pop_matrix) # contacts per capita as input for metapopulation model - to calculate force of infections

# Average severity in terms of probability infection requires hospitalisation, critical care and risk of mortality by region weighted by demography
# source("Script/calculate_severity.R") # script to run severity estimates; but can also load pre-run estimates below
# prob_hosp_reg <- p_hosp_reg
# prob_critical_given_hosp_reg <- p_crit_reg
# prob_non_severe_death_reg <- p_death_non_critical
# data for probabilities above can also be read using pre-run estimates by using three lines below
prob_hosp_reg <- readRDS("./processed_inputs/p_hosp_2_new_squire.rds")
prob_critical_given_hosp_reg <- readRDS("./processed_inputs/p_crit_2_new_squire.rds")
prob_non_severe_death_reg <- readRDS("./processed_inputs/p_death_non_critical_2_new_squire.rds")
prob_severe_death_reg <- rep(0.5,nrow(pop_matrix))

# PREPARE MOVEMENT MATRIX
movement_normal_matrix <- matrix_idn[[1]]
movement_ramadan_matrix <- matrix_idn[[2]]
no_movement_matrix <- diag(nrow=N_reg)
movement_normal_matrix_t <- t(movement_normal_matrix)
movement_ramadan_matrix_t <- t(movement_ramadan_matrix)

# metapopulation model input
N_reg <- nrow(pop_matrix) # number of admin units in model
S0 <- rowSums(pop_matrix) # number of starting susceptibles in each admin unit

## Prepare google mobility for java - and calculate rolling average version
google_mobility_java <- google_mobility_prov %>% filter(prov %in% c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")) %>%
  arrange(prov,date) %>% 
  select(-c(COUNTRY_ID,COUNTRY,period)) %>% 
  mutate(average_non_residential=(0.2*retail_and_recreation_percent_change_from_baseline +
                                    0.2*grocery_and_pharmacy_percent_change_from_baseline +
                                    0.2*parks_percent_change_from_baseline +
                                    0.2*transit_stations_percent_change_from_baseline +
                                    0.2*workplaces_percent_change_from_baseline))
google_mobility_java_before_2Mar_avg <- google_mobility_java %>% filter(date <= ymd("2020-03-02")) %>% group_by(prov) %>% 
  summarise(retail_and_recreation_percent_change_from_baseline=mean(retail_and_recreation_percent_change_from_baseline),
            grocery_and_pharmacy_percent_change_from_baseline=mean(grocery_and_pharmacy_percent_change_from_baseline),
            parks_percent_change_from_baseline=mean(parks_percent_change_from_baseline),
            transit_stations_percent_change_from_baseline=mean(transit_stations_percent_change_from_baseline),
            workplaces_percent_change_from_baseline=mean(workplaces_percent_change_from_baseline),
            residential_percent_change_from_baseline=mean(residential_percent_change_from_baseline),
            average_non_residential=mean(average_non_residential))
java_prov <- c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")

google_rolling_average <- list()
for(i in seq_len(length(java_prov))){
  dat_prov <- google_mobility_java %>% filter(prov==java_prov[i])
  rollmean_google <- data.frame(prov=java_prov[i],date=dat_prov$date,
                                roll_avg_mov_reduce=c(rep(NA,(roll_period-1)),rollmean(dat_prov[,9], roll_period, align = "right", na.rm=TRUE)))
  google_rolling_average[[i]] <- rollmean_google
}
google_rolling_average <- bind_rows(google_rolling_average)
google_mobility_java_before_2Mar_avg_roll <- google_mobility_java_before_2Mar_avg %>% select(prov,roll_avg_mov_reduce=average_non_residential)

date_before_2Mar <- rep(seq(ymd("2020-02-15"),ymd("2020-03-02"),by="days"),6)
google_mobility_java_updated <- google_mobility_java %>% filter(date>=ymd("2020-03-03"))
google_rolling_average_updated <- google_rolling_average %>% filter(date>=ymd("2020-03-03"))
mov_avg_2Mar_matrix_java <- google_mobility_java_before_2Mar_avg %>% slice(rep(row_number(), 17)) %>% arrange(prov) %>% mutate(date=date_before_2Mar)
mov_avg_2Mar_matrix_java_roll <- google_mobility_java_before_2Mar_avg_roll %>% slice(rep(row_number(), 17)) %>% arrange(prov) %>% mutate(date=date_before_2Mar)
google_mobility_java_updated <- bind_rows(mov_avg_2Mar_matrix_java,google_mobility_java_updated) %>% arrange(prov,date) %>% 
  mutate(mov_reduce=-1*average_non_residential)
google_rolling_average_updated <- bind_rows(mov_avg_2Mar_matrix_java_roll,google_rolling_average_updated) %>% arrange(prov,date) %>% 
  mutate(mov_roll_reduce=-1*roll_avg_mov_reduce)
google_rolling_average_updated_jkt <- google_rolling_average_updated %>% filter(prov=="JAKARTA") %>% select(date,mov_roll_reduce)


### FIND POPULATION NON-JAVA NB:not relevant as transmission outside Java hardcoded to zero
pop_provinces_total <- read.csv("Data/raw/pop_province_2020.csv")
province_name <- c("ACEH","NORTH SUMATERA","WEST SUMATERA","RIAU","JAMBI","SOUTH SUMATERA","BENGKULU","LAMPUNG","BANGKA BELITUNG ISLANDS","RIAU ISLANDS",
                   "JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","BALI","WEST NUSA TENGGARA","EAST NUSA TENGGARA",
                   "WEST KALIMANTAN","CENTRAL KALIMANTAN","SOUTH KALIMANTAN","EAST KALIMANTAN","NORTH KALIMANTAN","NORTH SULAWESI","CENTRAL SULAWESI",
                   "SOUTH SULAWESI","SOUTHEAST SULAWESI","GORONTALO","WEST SULAWESI","MALUKU","NORTH MALUKU","WEST PAPUA","PAPUA")
pop_provinces_total$prov <- province_name
colnames(pop_provinces_total) <- c("prov","pop")


## Google mobility for provinces outside Java - included as placeholder for future analysis though transmission not modelled
google_mobility_nonjava <- google_mobility_prov %>% filter(!(prov %in% c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN"))) %>%
  arrange(prov,date) %>% left_join(pop_provinces_total) %>% 
  select(-c(COUNTRY_ID,COUNTRY,period)) %>% rowwise() %>% 
  mutate(average_non_residential=mean(c(retail_and_recreation_percent_change_from_baseline,
                                        grocery_and_pharmacy_percent_change_from_baseline,
                                        parks_percent_change_from_baseline,
                                        transit_stations_percent_change_from_baseline,
                                        workplaces_percent_change_from_baseline),na.rm=TRUE)) %>% group_by(date) %>% 
  summarise(retail_and_recreation_percent_change_from_baseline=weighted.mean(retail_and_recreation_percent_change_from_baseline,pop,na.rm=TRUE),
            grocery_and_pharmacy_percent_change_from_baseline=weighted.mean(grocery_and_pharmacy_percent_change_from_baseline,pop,na.rm=TRUE),
            parks_percent_change_from_baseline=weighted.mean(parks_percent_change_from_baseline,pop,na.rm=TRUE),
            transit_stations_percent_change_from_baseline=weighted.mean(transit_stations_percent_change_from_baseline,pop,na.rm=TRUE),
            workplaces_percent_change_from_baseline=weighted.mean(workplaces_percent_change_from_baseline,pop,na.rm=TRUE),
            residential_percent_change_from_baseline=weighted.mean(residential_percent_change_from_baseline,pop,na.rm=TRUE),
            average_non_residential=weighted.mean(average_non_residential,pop,na.rm=TRUE)) %>% 
  mutate(prov="OUTSIDE JAVA") %>% ungroup() %>% select(prov,date,everything())
google_mobility_nonjava_before_2Mar_avg <- google_mobility_nonjava %>% filter(date <= ymd("2020-03-02")) %>% group_by(prov) %>% 
  summarise(retail_and_recreation_percent_change_from_baseline=mean(retail_and_recreation_percent_change_from_baseline),
            grocery_and_pharmacy_percent_change_from_baseline=mean(grocery_and_pharmacy_percent_change_from_baseline),
            parks_percent_change_from_baseline=mean(parks_percent_change_from_baseline),
            transit_stations_percent_change_from_baseline=mean(transit_stations_percent_change_from_baseline),
            workplaces_percent_change_from_baseline=mean(workplaces_percent_change_from_baseline),
            residential_percent_change_from_baseline=mean(residential_percent_change_from_baseline),
            average_non_residential=mean(average_non_residential))

dat_prov <- google_mobility_nonjava
google_rolling_average_nonjava <- data.frame(prov="OUTSIDE JAVA",date=dat_prov$date,
                                             roll_avg_mov_reduce=c(rep(NA,(roll_period-1)),rollmean(dat_prov[,9], roll_period, align = "right", na.rm=TRUE)))

google_mobility_nonjava_before_2Mar_avg_roll <- google_mobility_nonjava_before_2Mar_avg %>% select(prov,roll_avg_mov_reduce=average_non_residential)

date_before_2Mar_nonjava <- seq(ymd("2020-02-15"),ymd("2020-03-02"),by="days")
google_mobility_nonjava_updated <- google_mobility_nonjava %>% filter(date>=ymd("2020-03-03"))
google_rolling_average_nonjava_updated <- google_rolling_average_nonjava %>% filter(date>=ymd("2020-03-03"))
mov_avg_2Mar_matrix_nonjava <- google_mobility_nonjava_before_2Mar_avg %>% slice(rep(row_number(), 17)) %>% arrange(prov) %>% 
  mutate(date=date_before_2Mar_nonjava)
mov_avg_2Mar_matrix_nonjava_roll <- google_mobility_nonjava_before_2Mar_avg_roll %>% slice(rep(row_number(), 17)) %>% arrange(prov) %>% 
  mutate(date=date_before_2Mar_nonjava)
google_mobility_nonjava_updated <- bind_rows(mov_avg_2Mar_matrix_nonjava,google_mobility_nonjava_updated) %>% arrange(prov,date) %>% 
  mutate(mov_reduce=-1*average_non_residential)
google_rolling_average_nonjava_updated <- bind_rows(mov_avg_2Mar_matrix_nonjava_roll,google_rolling_average_nonjava_updated) %>% arrange(prov,date) %>% 
  mutate(mov_roll_reduce=-1*roll_avg_mov_reduce)

google_mobility_java_updated <- bind_rows(google_mobility_java_updated,google_mobility_nonjava_updated)
google_rolling_average_updated <- bind_rows(google_rolling_average_updated,google_rolling_average_nonjava_updated)


###Link Rt jakarta estimates and mobility 
posterior_Rt_mobility_funerals_model <- posterior_Rt_mobility_funerals %>% mutate(mov_reduce=-1*average_non_residential, model="Data") %>% 
  left_join(google_rolling_average_updated_jkt)
posterior_Rt_mobility_funerals_model$model <- factor(posterior_Rt_mobility_funerals_model$model,
                                                     levels=c("Data","Cubic spline","Smooth spline; CV=FALSE","Smooth spline; CV=TRUE"))

## OBTAIN SPLINE FIT OF RELATIONSHIP FROM 100 posterior samples of Epiestim of reconstructed funerals data
init_date <- ymd("2020-01-07")
last_date <- max(google_rolling_average_updated$date)  # ymd("2020-06-19") # put only, if can, the maximum date of google mobility data
time_period <- as.numeric(last_date-init_date+1) # time period to run over
ramadan_start_date <- ymd("2020-04-24")
ramadan_end_date <- ymd("2020-06-07")
lockdown_init_date <- ymd("2020-04-10")
ramadan_period <- seq(ramadan_start_date,ramadan_end_date,by="days")

## LIMIT TO PRE-'NEW-NORMAL' PERIOD
mobility_Rt <- posterior_Rt_mobility_funerals_model %>% filter(date<ymd("2020-06-01")) 
gm_period <- as.numeric(max(mobility_Rt$date) - min(mobility_Rt$date) + 1)
mobility_Rt$sample <- rep(1:100,each=gm_period)

## RUN SPLINES ####
spline_output_list <- list()
spline_model_list <- list()
prov_date <- google_rolling_average_updated %>% select(prov,date)
for (i in 1:100){
  mobility_Rt_data <- mobility_Rt %>% filter(sample==i)
  fit_split_funerals_roll_CV_true <- smooth.spline(mobility_Rt_data$mov_roll_reduce,mobility_Rt_data$Rt,nknots=4)
  predict_split_funerals_roll_CV_true <- data.frame(mov_roll_reduce=predict(fit_split_funerals_roll_CV_true,google_rolling_average_updated$mov_roll_reduce)$x,
                                                    Rt=predict(fit_split_funerals_roll_CV_true,google_rolling_average_updated$mov_roll_reduce)$y,
                                                    sample=i)
  model_output_smooth_spline <- cbind(prov_date,predict_split_funerals_roll_CV_true)
  spline_output_list[[i]] <- model_output_smooth_spline
  spline_model_list[[i]] <- fit_split_funerals_roll_CV_true
}

## USE SPLINES TO INFER Rt at district level 
# Time series of Rt: main scenario - assumes all the same
prov_district <- urban_rural_flag_df %>% select(prov=PROVINCE,district=REGION)
Rt_java_model_input_df_funerals_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_list[[i]] <- prov_district %>% left_join(spline_output_list[[i]]) %>% select(prov,district,date,Rt)
}

# Time series of Rt: rural districts 90% of urban
Rt_java_model_input_df_funerals_rural90_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_rural90_df <- Rt_java_model_input_df_funerals_list[[i]]
  for(j in seq_len(nrow(urban_rural_flag_df))){
    if (urban_rural_flag_df$URBAN_RURAL[j] == 0){
      Rt_java_model_input_df_funerals_rural90_df <- Rt_java_model_input_df_funerals_rural90_df %>% 
        mutate(Rt=ifelse(district==as.character(urban_rural_flag_df$REGION[j]),Rt*0.90,Rt))
    }
  }
  Rt_java_model_input_df_funerals_rural90_list[[i]] <- Rt_java_model_input_df_funerals_rural90_df
}

# Time series of Rt: rural districts 75% of urban
Rt_java_model_input_df_funerals_rural75_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_rural75_df <- Rt_java_model_input_df_funerals_list[[i]]
  for(j in seq_len(nrow(urban_rural_flag_df))){
    if (urban_rural_flag_df$URBAN_RURAL[j] == 0){
      Rt_java_model_input_df_funerals_rural75_df <- Rt_java_model_input_df_funerals_rural75_df %>% 
        mutate(Rt=ifelse(district==as.character(urban_rural_flag_df$REGION[j]),Rt*0.75,Rt))
    }
  }
  Rt_java_model_input_df_funerals_rural75_list[[i]] <- Rt_java_model_input_df_funerals_rural75_df
}

# Time series of Rt: rural districts 60% of urban
Rt_java_model_input_df_funerals_rural60_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_rural60_df <- Rt_java_model_input_df_funerals_list[[i]]
  for(j in seq_len(nrow(urban_rural_flag_df))){
    if (urban_rural_flag_df$URBAN_RURAL[j] == 0){
      Rt_java_model_input_df_funerals_rural60_df <- Rt_java_model_input_df_funerals_rural60_df %>% 
        mutate(Rt=ifelse(district==as.character(urban_rural_flag_df$REGION[j]),Rt*0.60,Rt))
    }
  }
  Rt_java_model_input_df_funerals_rural60_list[[i]] <- Rt_java_model_input_df_funerals_rural60_df
}

## ESTIMATE 'R0' on basis of maximum mobility
# R0: main
R0_district_list <- list()
for (i in 1:100){
  R0_df <- Rt_java_model_input_df_funerals_list[[i]] %>% group_by(prov,district) %>% summarise(R0=max(Rt)) %>% ungroup()
  R0_district_list[[i]] <- prov_district %>% left_join(R0_df)
}

# R0: rural 90%
R0_district_rural90_list <- list()
for (i in 1:100){
  R0_df <- Rt_java_model_input_df_funerals_rural90_list[[i]] %>% group_by(prov,district) %>% summarise(R0=max(Rt)) %>% ungroup()
  R0_district_rural90_list[[i]] <- prov_district %>% left_join(R0_df)
}

# R0: rural 75%
R0_district_rural75_list <- list()
for (i in 1:100){
  R0_df <- Rt_java_model_input_df_funerals_rural75_list[[i]] %>% group_by(prov,district) %>% summarise(R0=max(Rt)) %>% ungroup()
  R0_district_rural75_list[[i]] <- prov_district %>% left_join(R0_df)
}

# R0: rural 60%
R0_district_rural60_list <- list()
for (i in 1:100){
  R0_df <- Rt_java_model_input_df_funerals_rural60_list[[i]] %>% group_by(prov,district) %>% summarise(R0=max(Rt)) %>% ungroup()
  R0_district_rural60_list[[i]] <- prov_district %>% left_join(R0_df)
}

# Scenarios being explored: inter-district Ramadan movement restrictions doesn't exist
google_mobility_java_norestrict_ramadan_movement <- google_mobility_java_updated
google_mobility_java_norestrict_ramadan_movement <- google_mobility_java_norestrict_ramadan_movement %>%
  mutate(mov_reduce=ifelse(date %in% ramadan_period, 0, mov_reduce))

# Scenarios being explored: unmitigated
google_mobility_java_unmitigated <- google_mobility_java_updated
google_mobility_java_unmitigated <- google_mobility_java_unmitigated %>%
  mutate(mov_reduce=0)


# Movement matrix: baseline
movement_matrix_main <- movement_matrix_process(lockdown_matrix_loc=NULL,
                                                init_date=init_date,last_date=last_date,
                                                ramadan_start_date=ramadan_start_date,ramadan_end_date=ramadan_end_date,
                                                lockdown_init_date=lockdown_init_date,
                                                google_mobility=google_mobility_java_updated,
                                                admin_df=urban_rural_flag_df,
                                                normal_mtx=movement_normal_matrix,ramadan_mtx=movement_ramadan_matrix,
                                                mobility_OR=2,mobility_OR_ramadan=2)

# Movement matrix: sensitivity analysis - minimal between-district movement during ramadan (50 fold reduction in odds of moving relative to baseline)
movement_matrix_main_highOR_ramadan <- movement_matrix_process(lockdown_matrix_loc=NULL,
                                                               init_date=init_date,last_date=last_date,
                                                               ramadan_start_date=ramadan_start_date,ramadan_end_date=ramadan_end_date,
                                                               lockdown_init_date=lockdown_init_date,
                                                               google_mobility=google_mobility_java_updated,
                                                               admin_df=urban_rural_flag_df,
                                                               normal_mtx=movement_normal_matrix,ramadan_mtx=movement_ramadan_matrix,
                                                               mobility_OR=2,mobility_OR_ramadan=50*2)

# Movement matrix: unrestricted ramadan
movement_matrix_norestrict_ramadan <- movement_matrix_process(lockdown_matrix_loc=NULL,
                                                              init_date=init_date,last_date=last_date,
                                                              ramadan_start_date=ramadan_start_date,ramadan_end_date=ramadan_end_date,
                                                              lockdown_init_date=lockdown_init_date,
                                                              google_mobility=google_mobility_java_norestrict_ramadan_movement,
                                                              admin_df=urban_rural_flag_df,
                                                              normal_mtx=movement_normal_matrix,ramadan_mtx=movement_ramadan_matrix,
                                                              mobility_OR=2,mobility_OR_ramadan=2)

# Movement matrix: unmitigated
movement_matrix_unmitigated <- movement_matrix_process(lockdown_matrix_loc=NULL,
                                                       init_date=init_date,last_date=last_date,
                                                       ramadan_start_date=ramadan_start_date,ramadan_end_date=ramadan_end_date,
                                                       lockdown_init_date=lockdown_init_date,
                                                       google_mobility=google_mobility_java_unmitigated,
                                                       admin_df=urban_rural_flag_df,
                                                       normal_mtx=movement_normal_matrix,ramadan_mtx=movement_ramadan_matrix,
                                                       mobility_OR=2,mobility_OR_ramadan=2)

# Calculates the relative reduction in Rt in each patch at each timepoint for each of 100 splines
# main scenario
Rt_rel_main_list <- list()
for (i in 1:100){
  Rt_rel_main <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                Rt_df=Rt_java_model_input_df_funerals_list[[i]],R0_df=R0_district_list[[i]],
                                admin_df=urban_rural_flag_df)
  Rt_rel_main_list[[i]] <- Rt_rel_main
  print(i)
}

# rural 90%
Rt_rel_main_rural90_list <- list()
for (i in 1:100){
  Rt_rel_main <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                Rt_df=Rt_java_model_input_df_funerals_rural90_list[[i]],R0_df=R0_district_rural90_list[[i]],
                                admin_df=urban_rural_flag_df)
  Rt_rel_main_rural90_list[[i]] <- Rt_rel_main
  print(i)
}

# rural 75%
Rt_rel_main_rural75_list <- list()
for (i in 1:100){
  Rt_rel_main <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                Rt_df=Rt_java_model_input_df_funerals_rural75_list[[i]],R0_df=R0_district_rural75_list[[i]],
                                admin_df=urban_rural_flag_df)
  Rt_rel_main_rural75_list[[i]] <- Rt_rel_main
  print(i)
}

# rural 60%
Rt_rel_main_rural60_list <- list()
for (i in 1:100){
  Rt_rel_main <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                Rt_df=Rt_java_model_input_df_funerals_rural60_list[[i]],R0_df=R0_district_rural60_list[[i]],
                                admin_df=urban_rural_flag_df)
  Rt_rel_main_rural60_list[[i]] <- Rt_rel_main
  print(i)
}

# Calculate underlying per-contact transmission prob. to achieve maximum Rt from each spline estimate 
# main scenario
pinf_funerals_list <- list()
for (i in 1:100){
  R0_req_funerals <- R0_district_list[[i]]$R0 
  pinf_funerals <- vector()
  for (j in seq_len(nrow(movement_normal_matrix_t))){
    pinf_funerals[j] <- get_required_pinf(duration_infectiousness_mild=2.1, duration_infectiousness_case=4.5, p_hosp_average=prob_hosp_reg[j], 
                                          R0=R0_req_funerals[j], contacts=contacts_reg_average[j]) 
  }
  pinf_funerals_list[[i]] <- pinf_funerals
}

# rural 90%
pinf_funerals_rural90_list <- list()
for (i in 1:100){
  R0_req_funerals <- R0_district_rural90_list[[i]]$R0 
  pinf_funerals <- vector()
  for (j in seq_len(nrow(movement_normal_matrix_t))){
    pinf_funerals[j] <- get_required_pinf(duration_infectiousness_mild=2.1, duration_infectiousness_case=4.5, p_hosp_average=prob_hosp_reg[j], 
                                          R0=R0_req_funerals[j], contacts=contacts_reg_average[j]) 
  }
  pinf_funerals_rural90_list[[i]] <- pinf_funerals
}

# rural 75%
pinf_funerals_rural75_list <- list()
for (i in 1:100){
  R0_req_funerals <- R0_district_rural75_list[[i]]$R0 
  pinf_funerals <- vector()
  for (j in seq_len(nrow(movement_normal_matrix_t))){
    pinf_funerals[j] <- get_required_pinf(duration_infectiousness_mild=2.1, duration_infectiousness_case=4.5, p_hosp_average=prob_hosp_reg[j], 
                                          R0=R0_req_funerals[j], contacts=contacts_reg_average[j]) 
  }
  pinf_funerals_rural75_list[[i]] <- pinf_funerals
}

# rural 60%
pinf_funerals_rural60_list <- list()
for (i in 1:100){
  R0_req_funerals <- R0_district_rural60_list[[i]]$R0 
  pinf_funerals <- vector()
  for (j in seq_len(nrow(movement_normal_matrix_t))){
    pinf_funerals[j] <- get_required_pinf(duration_infectiousness_mild=2.1, duration_infectiousness_case=4.5, p_hosp_average=prob_hosp_reg[j], 
                                          R0=R0_req_funerals[j], contacts=contacts_reg_average[j]) 
  }
  pinf_funerals_rural60_list[[i]] <- pinf_funerals
}

## create samples of seed case in Surabaya, EAST JAVA
# ICase0_list_12 <- list()
# seed_ICase0_12 <- vector()
# for (i in 1:100){
#   seed_rand <- round(runif(1,1,1000000))
#   set.seed(seed_rand)
#   surabaya_init <- rbinom(1,12,0.6)
#   ICase0_list_12[[i]] <- c(12,rep(0,104),surabaya_init,rep(0,9))
#   seed_ICase0_12[i] <- seed_rand
# }
# 
# saveRDS(seed_ICase0_12,"seed_ICase0_jkt12.rds")
# saveRDS(ICase0_list_12,"ICase0_list_jkt12.rds")

ICase0_list_12 <- readRDS("Script/Seeds/ICase0_list_jkt12.rds")

## Load seeds to reproduces similar number as in the text
model_output_seed <- readRDS("Script/Seeds/model_output_seed.rds")

## running the model
# rural 100%
model_output_main_12 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_main,
                                                          Rt_rel_list=Rt_rel_main_list,
                                                          number_model_runs=100,
                                                          pinf_list=pinf_funerals_list,region_name=region_name,
                                                          prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                          prob_severe_death_reg=prob_severe_death_reg,
                                                          prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                          contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                          time_period=time_period,N_reg=N_reg,S0=S0,ICase0_list=ICase0_list_12,randomised_seed = TRUE,
                                                          saved_seed = model_output_seed$simulations_seed)
# saveRDS(model_output_main_12,"Output/Metapopulation/model_output_main_NN_12_19092020_new.rds")

# rural 90%
model_output_rural90 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_main,
                                                          Rt_rel_list=Rt_rel_main_rural90_list,
                                                          number_model_runs=100,
                                                          pinf_list=pinf_funerals_rural90_list,region_name=region_name,
                                                          prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                          prob_severe_death_reg=prob_severe_death_reg,
                                                          prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                          contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                          time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                          saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural90,"Output/Metapopulation/model_output_rural90_NN_12_19092020_new.rds")

# sensitivity analysis for baseline rural 90% with OR = 100
model_output_rural90_highOR_ramadan <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_main_highOR_ramadan,
                                                                         Rt_rel_list=Rt_rel_main_rural90_list,
                                                                         number_model_runs=100,
                                                                         pinf_list=pinf_funerals_rural90_list,region_name=region_name,
                                                                         prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                                         prob_severe_death_reg=prob_severe_death_reg,
                                                                         prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                                         contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                                         time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                                         saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural90_highOR_ramadan,"Output/Metapopulation/model_output_rural90_NN_12_19092020_new_highOR_ramadan.rds")

# rural 75%
model_output_rural75 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_main,
                                                          Rt_rel_list=Rt_rel_main_rural75_list,
                                                          number_model_runs=100,
                                                          pinf_list=pinf_funerals_rural75_list,region_name=region_name,
                                                          prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                          prob_severe_death_reg=prob_severe_death_reg,
                                                          prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                          contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                          time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                          saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural75,"Output/Metapopulation/model_output_rural75_NN_12_19092020_new.rds")

# rural 60%
model_output_rural60 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_main,
                                                          Rt_rel_list=Rt_rel_main_rural60_list,
                                                          number_model_runs=100,
                                                          pinf_list=pinf_funerals_rural60_list,region_name=region_name,
                                                          prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                          prob_severe_death_reg=prob_severe_death_reg,
                                                          prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                          contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                          time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                          saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural60,"Output/Metapopulation/model_output_rural60_NN_12_19092020_new.rds")

## Additional scenarios after choosing rural reductions
# More scenarios
# Rt scenario ramadan 2:
Rt_java_model_input_df_funerals_ramadan_2_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_ramadan_2 <- Rt_java_model_input_df_funerals_rural90_list[[i]]
  for (j in seq_len(nrow(urban_rural_flag_df))){
    Rt_java_model_input_df_funerals_ramadan_2 <- Rt_java_model_input_df_funerals_ramadan_2 %>% 
      mutate(Rt=ifelse(date %in% ramadan_period & district==as.character(urban_rural_flag_df$REGION[j]), R0_district_rural90_list[[i]]$R0[j]*0.75, Rt))
  }
  Rt_java_model_input_df_funerals_ramadan_2_list[[i]] <- Rt_java_model_input_df_funerals_ramadan_2
}

# Rt scenario ramadan 3:
Rt_java_model_input_df_funerals_ramadan_3_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_ramadan_3 <- Rt_java_model_input_df_funerals_rural90_list[[i]]
  for (j in seq_len(nrow(urban_rural_flag_df))){
    Rt_java_model_input_df_funerals_ramadan_3 <- Rt_java_model_input_df_funerals_ramadan_3 %>% 
      mutate(Rt=ifelse(date %in% ramadan_period & district==as.character(urban_rural_flag_df$REGION[j]), R0_district_rural90_list[[i]]$R0[j], Rt))
  }
  Rt_java_model_input_df_funerals_ramadan_3_list[[i]] <- Rt_java_model_input_df_funerals_ramadan_3
}

# Rt scenario unmitigated:
Rt_java_model_input_df_funerals_unmitigated_list <- list()
for (i in 1:100){
  Rt_java_model_input_df_funerals_unmitigated <- Rt_java_model_input_df_funerals_rural90_list[[i]]
  for (j in seq_len(nrow(urban_rural_flag_df))){
    Rt_java_model_input_df_funerals_unmitigated <- Rt_java_model_input_df_funerals_unmitigated %>% 
      mutate(Rt=ifelse(district==as.character(urban_rural_flag_df$REGION[j]), R0_district_rural90_list[[i]]$R0[j], Rt))
  }
  Rt_java_model_input_df_funerals_unmitigated_list[[i]] <- Rt_java_model_input_df_funerals_unmitigated
}

# Rt relative
# Rt_rel: ramadan 2 - create in form of a list
Rt_rel_ramadan_2_list <- list()
for (i in 1:100){
  Rt_rel_ramadan_2 <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                     Rt_df=Rt_java_model_input_df_funerals_ramadan_2_list[[i]],R0_df=R0_district_rural90_list[[i]],
                                     admin_df=urban_rural_flag_df)
  Rt_rel_ramadan_2_list[[i]] <- Rt_rel_ramadan_2
  print(i)
}

# Rt_rel: ramadan 3 - create in form of a list
Rt_rel_ramadan_3_list <- list()
for (i in 1:100){
  Rt_rel_ramadan_3 <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                     Rt_df=Rt_java_model_input_df_funerals_ramadan_3_list[[i]],R0_df=R0_district_rural90_list[[i]],
                                     admin_df=urban_rural_flag_df)
  Rt_rel_ramadan_3_list[[i]] <- Rt_rel_ramadan_3
  print(i)
}

# Rt_rel: ramadan unmitigated - create in form of a list
Rt_rel_unmitigated_list <- list()
for (i in 1:100){
  Rt_rel_unmitigated <- Rt_rel_process(init_date=init_date,last_date=last_date,
                                       Rt_df=Rt_java_model_input_df_funerals_unmitigated_list[[i]],R0_df=R0_district_rural90_list[[i]],
                                       admin_df=urban_rural_flag_df)
  Rt_rel_unmitigated_list[[i]] <- Rt_rel_unmitigated
  print(i)
}

# Unmitigated
model_output_rural90_unmitigated <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_unmitigated,
                                                                      Rt_rel_list=Rt_rel_unmitigated_list,
                                                                      number_model_runs=100,
                                                                      pinf_list=pinf_funerals_rural90_list,region_name=region_name,
                                                                      prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                                      prob_severe_death_reg=prob_severe_death_reg,
                                                                      prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                                      contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                                      time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                                      saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural90_unmitigated,"Output/Metapopulation/model_output_rural90_unmitigated_NN_12_19092020_new.rds")

# Ramadan 1
model_output_rural90_ramadan1 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_norestrict_ramadan,
                                                                   Rt_rel_list=Rt_rel_main_rural90_list,
                                                                   number_model_runs=100,
                                                                   pinf_list=pinf_funerals_rural90_list,region_name=region_name,
                                                                   prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                                   prob_severe_death_reg=prob_severe_death_reg,
                                                                   prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                                   contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                                   time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                                   saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural90_ramadan1,"Output/Metapopulation/model_output_rural90_ramadan1_NN_12_19092020_new.rds")

# Ramadan 2
model_output_rural90_ramadan2 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_norestrict_ramadan,
                                                                   Rt_rel_list=Rt_rel_ramadan_2_list,
                                                                   number_model_runs=100,
                                                                   pinf_list=pinf_funerals_rural90_list,region_name=region_name,
                                                                   prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                                   prob_severe_death_reg=prob_severe_death_reg,
                                                                   prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                                   contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                                   time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                                   saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural90_ramadan2,"Output/Metapopulation/model_output_rural90_ramadan2_NN_12_19092020_new.rds")

# Ramadan 3
model_output_rural90_ramadan3 <- metapopulation_model_run_list_new(movement_matrix_list=movement_matrix_norestrict_ramadan,
                                                                   Rt_rel_list=Rt_rel_ramadan_3_list,
                                                                   number_model_runs=100,
                                                                   pinf_list=pinf_funerals_rural90_list,region_name=region_name,
                                                                   prob_hosp_reg=prob_hosp_reg,prob_non_severe_death_reg=prob_non_severe_death_reg,
                                                                   prob_severe_death_reg=prob_severe_death_reg,
                                                                   prob_critical_given_hosp_reg=prob_critical_given_hosp_reg,
                                                                   contacts_reg=contacts_reg,contacts_reg_average=contacts_reg_average,
                                                                   time_period=time_period,N_reg=N_reg,S0=S0,ICase0=ICase0_list_12,randomised_seed = TRUE,
                                                                   saved_seed = model_output_main_12$simulations_seed)
# saveRDS(model_output_rural90_ramadan3,"Output/Metapopulation/model_output_rural90_ramadan3_NN_12_19092020_new.rds")
