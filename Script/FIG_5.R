## Load Required Packages
#library(tidyverse); library(socialmixr); library(extraDistr); library(tictoc); 
#library(zoo); library(readxl); library(lubridate); library(FSA); library(patchwork); library(gridExtra); library(ggpubr); library(corrr); library(ggcorrplot)
#library(squire); library(parsedate); library(RColorBrewer); library(EpiEstim); library(incidence); library(scales); library(epitrix); library(matrixStats);
#library(magrittr); library(cowplot); library(epidemia); library(tidybayes)
# rm(list=ls())

source("Script/functions_pmcmc.R")

## Summarise population data to province-level + add extra age group of 75-80

pop_region <- read.csv("Data/raw/admin2-reg-pop-java-by-age.csv",header = TRUE,stringsAsFactors = FALSE)
urban_rural_flag_df <- read.csv("Data/raw/urban_rural_flag.csv",header = TRUE,stringsAsFactors = FALSE)
pop_district <- pop_region %>% left_join(urban_rural_flag_df) %>% select(-IDREGION,-URBAN_RURAL) %>% select(prov=PROVINCE,district=REGION,everything())
pop_province <- pop_district %>% group_by(prov) %>% summarise_at(vars(X0_4:X75.), sum, na.rm = TRUE)
pop_indonesia <- get_population("Indonesia")
pop_indonesia_over75 <- pop_indonesia[16:17,3]
pop_indonesia_over75_prop <- pop_indonesia_over75/sum(pop_indonesia_over75)
pop_province <- pop_province %>% mutate(X75_80=X75.*pop_indonesia_over75_prop[1],XOVER80=X75.*pop_indonesia_over75_prop[2]) %>% select(-X75.)
pop_province <- pop_province %>% pivot_longer(-prov,names_to="age_group",values_to="pop")
pop_province$age_group <- factor(pop_province$age_group,levels=unique(pop_province$age_group))
pop_province <- pop_province %>% arrange(prov,age_group)
pop_province$pop <- round(pop_province$pop)

### READ DATA

cases_hospitalised_java <- read.csv("Data/raw/idn_hospitalised.csv")
cases_hospitalised_java <- cases_hospitalised_java %>% mutate(date=dmy(date)) %>% select(date,prov,confirmed)

hosp_capacity_java <- read.csv("Data/raw/hospital_capacity_java.csv") # hospital capacity data; ICU capacities for provinces other than Jakarta are assumed to be 10% of total bed capacity
reported_deaths_java <- readRDS("processed_inputs/reported_deaths_java.rds") # if directly from previous ones: reported_deaths_java <- idn_deaths_calib
all_deaths_java <- readRDS("processed_inputs/simulated_all_deaths_java.rds") # if directly from previous ones: all_deaths_java <- all_deaths_java
java_provinces <- c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN")

reported_deaths_java_all <- reported_deaths_java %>% group_by(date) %>% summarise(deaths=sum(deaths)) %>% ungroup()
all_deaths_java_all <- all_deaths_java %>% group_by(date) %>% summarise(deaths=sum(deaths)) %>% ungroup()

date_t_df <- data.frame(date=seq(max(reported_deaths_java$date)-300,ymd("2022-06-30"),by="days"),t=seq(-300,666))

java_province_df <- data.frame(prov=java_provinces,region=c("JAKARTA","OUTSIDE JAKARTA","OUTSIDE JAKARTA","OUTSIDE JAKARTA","OUTSIDE JAKARTA","OUTSIDE JAKARTA"))
hosp_capacity_java_region <- hosp_capacity_java %>% left_join(java_province_df) %>% group_by(region) %>% summarise(hosp_beds=sum(hosp_beds),icu_beds=sum(icu_beds)) %>% 
  ungroup()
pop_province_all <- pop_province %>% group_by(prov) %>% summarise(pop=sum(pop)) %>% ungroup() %>% filter(prov!="OUTSIDE JAVA")
pop_province_all_region <- pop_province_all %>% left_join(java_province_df) %>% group_by(region) %>% summarise(pop=sum(pop)) %>% ungroup()
hosp_capacity_java_region <- hosp_capacity_java_region %>% left_join(pop_province_all_region) %>% 
  mutate(hosp_beds_per_mil=hosp_beds/pop*1000000,icu_beds_per_mil=icu_beds/pop*1000000)


pmcmc_fits <- readRDS("processed_inputs/best_40k_14d.rds")
# pmcmc_fits <- best_40K_14d # if running province_level_fitting_squire.R

provinces <- unlist(lapply(pmcmc_fits, function(x) {x$parameters$country}))
type <- unlist(lapply(pmcmc_fits, function(x) {x$parameters$all_deaths}))
prov_type <- data.frame(prov=provinces,type=type,index=seq_len(length(provinces)))

pmcmc_output_reported_deaths_list <- list()
pmcmc_output_all_deaths_list <- list()
for (i in 1:6){
  index_prov_reported <- prov_type %>% filter(prov==java_provinces[i] & type==FALSE) %>% pull(index)
  index_prov_all <- prov_type %>% filter(prov==java_provinces[i] & type==TRUE) %>% pull(index)
  pmcmc_fits[[index_prov_reported]]$parameters$country <- "Indonesia"
  pmcmc_fits[[index_prov_all]]$parameters$country <- "Indonesia"
  pmcmc_output_reported_deaths_list[[i]] <- pmcmc_fits[[index_prov_reported]]
  pmcmc_output_all_deaths_list[[i]] <- pmcmc_fits[[index_prov_all]]
}

## plotting fits
# reported
data_input <- reported_deaths_java
p_pmcmc_reported_deaths_list <- list()
for (i in 1:6){
  df_prov <- data_input %>% filter(prov==java_provinces[i])
  p_pmcmc_reported_deaths_list[[i]] <- plot_pmcmc_output_deaths(pmcmc_output_reported_deaths_list[[i]],df_prov,java_provinces[i])
}

p_pmcmc_reported_deaths <- plot_grid(p_pmcmc_reported_deaths_list[[1]],p_pmcmc_reported_deaths_list[[2]],p_pmcmc_reported_deaths_list[[3]],
                                     p_pmcmc_reported_deaths_list[[4]],p_pmcmc_reported_deaths_list[[5]],p_pmcmc_reported_deaths_list[[6]],
                                     rel_heights = c(1, 1, 1, 1, 1, 1), align = "hv", nrow = 3)

pdf(file = "Output/Extended-Data-FIG-9.pdf", width = 15, height = 10)
  p_pmcmc_reported_deaths
dev.off()

# all
data_input <- all_deaths_java
p_pmcmc_all_deaths_list <- list()
for (i in 1:6){
  df_prov <- data_input %>% filter(prov==java_provinces[i])
  p_pmcmc_all_deaths_list[[i]] <- plot_pmcmc_output_deaths(pmcmc_output_all_deaths_list[[i]],df_prov,java_provinces[i])
}

p_pmcmc_all_deaths <- plot_grid(p_pmcmc_all_deaths_list[[1]],p_pmcmc_all_deaths_list[[2]],p_pmcmc_all_deaths_list[[3]],
                                p_pmcmc_all_deaths_list[[4]],p_pmcmc_all_deaths_list[[5]],p_pmcmc_all_deaths_list[[6]],
                                rel_heights = c(1, 1, 1, 1, 1, 1), align = "hv", nrow = 3)

pdf(file = "Output/Extended-Data-FIG-10.pdf", width = 15, height = 10)
  p_pmcmc_all_deaths
dev.off()

# combining into java level
pop_all_java <- pop_province %>% filter(prov != "OUTSIDE JAVA") %>% pull(pop)
pop_all_java <- sum(pop_all_java)
pop_all_province <- pop_province %>% group_by(prov) %>% summarise(pop=sum(pop)) %>% ungroup() %>% filter(prov != "OUTSIDE JAVA")

output_reported_deaths_java_list <- list()
output_all_deaths_java_list <- list()
for(i in 1:6){
  output_reported_deaths_java_list[[i]] <- format_output(pmcmc_output_reported_deaths_list[[i]],var_select = c("delta_D","S")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i])
  output_all_deaths_java_list[[i]] <- format_output(pmcmc_output_all_deaths_list[[i]],var_select = c("delta_D","S")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i])
}
output_reported_deaths_java_prov <- bind_rows(output_reported_deaths_java_list)
output_all_deaths_java_prov <- bind_rows(output_all_deaths_java_list)

#Plot calibrated deaths and % susceptible for each mortality metric
min_t_reported_deaths <- min(output_reported_deaths_java_prov$t)
min_t_all_deaths <- min(output_all_deaths_java_prov$t)
columns_reported_deaths <- expand.grid(t=seq(min_t_reported_deaths,0),replicate=1:100,prov=java_provinces,compartment=c("delta_D","S"))
columns_all_deaths <- expand.grid(t=seq(min_t_all_deaths,0),replicate=1:100,prov=java_provinces,compartment=c("delta_D","S"))

output_reported_deaths_java_prov <- columns_reported_deaths %>% left_join(output_reported_deaths_java_prov)
output_all_deaths_java_prov <- columns_all_deaths %>% left_join(output_all_deaths_java_prov)

for (i in 1:6){
  pop_total_prov <- pop_all_province %>% filter(prov == java_provinces[i]) %>% pull(pop)
  output_reported_deaths_java_prov <- output_reported_deaths_java_prov %>% 
    mutate(y = if_else(is.na(y) & compartment == "S" & prov == java_provinces[i], pop_total_prov, y))
  output_all_deaths_java_prov <- output_all_deaths_java_prov %>% 
    mutate(y = if_else(is.na(y) & compartment == "S" & prov == java_provinces[i], pop_total_prov, y))
}

output_reported_deaths_java <- output_reported_deaths_java_prov %>% group_by(replicate,compartment,t) %>% summarise(y=sum(y,na.rm=TRUE)) %>% ungroup() %>% 
  pivot_wider(names_from=compartment,values_from=y) %>% mutate(infected=pop_all_java-S,attack_rate=infected/pop_all_java*100,susceptible=round(S/pop_all_java*100,2))
output_all_deaths_java <- output_all_deaths_java_prov %>% group_by(replicate,compartment,t) %>% summarise(y=sum(y)) %>% ungroup() %>% 
  pivot_wider(names_from=compartment,values_from=y) %>% mutate(infected=pop_all_java-S,attack_rate=infected/pop_all_java*100,susceptible=round(S/pop_all_java*100,2))

output_reported_deaths_java_prov <- output_reported_deaths_java_prov %>% 
  pivot_wider(names_from=compartment,values_from=y) %>% mutate(infected=pop_all_java-S,attack_rate=infected/pop_all_java*100,susceptible=round(S/pop_all_java*100,2))
output_all_deaths_java_prov <- output_all_deaths_java_prov %>% 
  pivot_wider(names_from=compartment,values_from=y) %>% mutate(infected=pop_all_java-S,attack_rate=infected/pop_all_java*100,susceptible=round(S/pop_all_java*100,2))

start_date_reported_deaths <- max(reported_deaths_java$date) + min_t_reported_deaths
start_date_all_deaths <- max(all_deaths_java$date) + min_t_all_deaths
date_t_reported_deaths <- data.frame(date=seq(start_date_reported_deaths,max(reported_deaths_java$date),by="days"),t=seq(min_t_reported_deaths,0))
date_t_all_deaths <- data.frame(date=seq(start_date_all_deaths,max(all_deaths_java$date),by="days"),t=seq(min_t_all_deaths,0))

output_reported_deaths_java <- output_reported_deaths_java %>% left_join(date_t_reported_deaths)
output_all_deaths_java <- output_all_deaths_java %>% left_join(date_t_all_deaths)
output_reported_deaths_java_prov <- output_reported_deaths_java_prov %>% left_join(date_t_df)
output_all_deaths_java_prov <- output_all_deaths_java_prov %>% left_join(date_t_df)

y_coeff <- 2.5

fig_5A <- ggplot() + geom_line(data=output_reported_deaths_java,aes(x=date,y=delta_D,group=replicate,col="a"),alpha=0.2) + ylim(0,250) +
  geom_point(aes(date, deaths, col="b"), reported_deaths_java_all) + geom_smooth(aes(date, deaths, col="b"), reported_deaths_java_all, span = 0.3,lty=2) + 
  geom_line(data=output_all_deaths_java,aes(x=date,y=delta_D,group=replicate,col="c"),alpha=0.2) +
  geom_point(aes(date, deaths, col="d"), all_deaths_java_all) + geom_smooth(aes(date, deaths, col="d"), all_deaths_java_all, span = 0.3,lty=2) + 
  theme_bw() + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  stat_lineribbon(data=output_reported_deaths_java,aes(x=date, y=susceptible*y_coeff, col="e"), .width = c(0.95), fill = "#49006a") +
  stat_lineribbon(data=output_all_deaths_java,aes(x=date, y=susceptible*y_coeff, col="f"), .width = c(0.95), fill = "#e7298a") +
  scale_y_continuous(name = "Daily number of deaths",sec.axis = sec_axis(~./y_coeff, name="% Susceptible Population")) +
  theme(legend.position="top",strip.background = element_blank(),strip.text.x = element_blank(),legend.text = element_text(size=10)) + 
    scale_colour_manual(values = c("#081d58","#41b6c4","#004529","#78c679","#49006a","#e7298a"),
                      labels = c("Model (Conf.)","Data (Conf.)","Model (Susp.)","Data (Susp.)",
                                 "% Susceptible (Conf.)","% Susceptible (Susp.)")) +
  guides(color=guide_legend(override.aes=list(fill=NA,pch=c(NA,16,NA,16,NA,NA),lty=c(1,NA,1,NA,1,1)))) + 
  coord_cartesian(ylim=c(0,250),xlim=c(ymd("2020-03-01"),ymd("2020-09-02"))) + xlab("Date") + labs(col="")

### FUTURE PROJECTIONS Rc=0.75,1.25 and 2
proj_0.75_all_deaths_list <- list()
proj_0.75_reported_deaths_list <- list()
proj_1.25_all_deaths_list <- list()
proj_1.25_reported_deaths_list <- list()
proj_2.00_all_deaths_list <- list()
for (i in 1:6){
  proj_0.75_all_deaths_list[[i]] <- projections(r = pmcmc_output_all_deaths_list[[i]], 
                                                R0 = 0.75,
                                                tt_R0 = 0,time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_0.75_reported_deaths_list[[i]] <- projections(r = pmcmc_output_reported_deaths_list[[i]], 
                                                     R0 = 0.75,
                                                     tt_R0 = 0,time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_1.25_all_deaths_list[[i]] <- projections(r = pmcmc_output_all_deaths_list[[i]], 
                                                R0 = 1.25,
                                                tt_R0 = 0,time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_1.25_reported_deaths_list[[i]] <- projections(r = pmcmc_output_reported_deaths_list[[i]], 
                                                     R0 = 1.25,
                                                     tt_R0 = 0,time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_2.00_all_deaths_list[[i]] <- projections(r = pmcmc_output_all_deaths_list[[i]], 
                                                R0 = 2.00,
                                                tt_R0 = 0,time_period = 666) # to cover 1 sept up to 31 dec 2021
}
## FORMAT OUTPUT
proj_list <- list()
for (i in 1:6){
  proj_0.75_all_deaths <- format_output(proj_0.75_all_deaths_list[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="0.75 - All deaths")
  proj_0.75_reported_deaths <- format_output(proj_0.75_reported_deaths_list[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="0.75 - Reported deaths")
  proj_1.25_all_deaths <- format_output(proj_1.25_all_deaths_list[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="1.25 - All deaths")
  proj_1.25_reported_deaths <- format_output(proj_1.25_reported_deaths_list[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="1.25 - Reported deaths")
  proj_2.00_all_deaths <- format_output(proj_2.00_all_deaths_list[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="2.00 - All deaths")
  proj_list[[i]] <- bind_rows(proj_0.75_all_deaths,proj_0.75_reported_deaths,proj_1.25_all_deaths,proj_1.25_reported_deaths,proj_2.00_all_deaths)
}
proj_df <- bind_rows(proj_list)

proj_df <- proj_df %>% left_join(date_t_df) %>% left_join(java_province_df)
proj_df$prov <- factor(proj_df$prov,levels=java_provinces)
proj_df_prov <- proj_df %>% group_by(replicate,compartment,t,scenario_Rt,date,prov) %>% summarise(y=sum(y)) %>% ungroup()

proj_df_prov_deaths <- proj_df_prov %>% filter(compartment=="deaths" & 
                                                 scenario_Rt %in% c("0.75 - All deaths","0.75 - Reported deaths","1.25 - All deaths","1.25 - Reported deaths")) %>% 
  group_by(replicate,prov,scenario_Rt) %>% mutate(cum_rolling7=rollapplyr(y,width=7,FUN=sum,partial=TRUE)) %>% ungroup() %>% arrange(prov,scenario_Rt,replicate,date)

proj_df_prov_deaths_summary <- proj_df_prov_deaths %>% group_by(prov,scenario_Rt,date,t,compartment) %>% 
  summarise(median_cum7_deaths=median(cum_rolling7)) %>% ungroup()

# Find index where average deaths in the past week < 7 in order to simulate relaxing of measures at low levels
index_lower7 <- proj_df_prov_deaths_summary %>% filter(date > ymd("2020-09-30") & median_cum7_deaths<7) %>% group_by(prov,scenario_Rt,compartment) %>% 
  summarise(min_date=min(date),min_t=min(t)) %>% ungroup() %>% mutate(prov=factor(prov,levels=java_provinces)) %>% arrange(prov)

index_lower7_all_deaths <- index_lower7 %>% filter(scenario_Rt=="1.25 - All deaths") %>% pull(min_t)
index_lower7_reported_deaths <- index_lower7 %>% filter(scenario_Rt=="1.25 - Reported deaths") %>% pull(min_t)
index_lower7_all_deaths_0.75 <- index_lower7 %>% filter(scenario_Rt=="0.75 - All deaths") %>% pull(min_t)
index_lower7_reported_deaths_0.75 <- index_lower7 %>% filter(scenario_Rt=="0.75 - Reported deaths") %>% pull(min_t)

# rerun simulations with relaxing of measures at low levels
proj_1.25_all_deaths_list_new <- list()
proj_1.25_reported_deaths_list_new <- list()
proj_0.75_all_deaths_list_newnormal <- list()
proj_0.75_all_deaths_list_oldnormal <- list()
proj_0.75_reported_deaths_list_newnormal <- list()
proj_0.75_reported_deaths_list_oldnormal <- list()
for (i in 1:6){
  proj_1.25_all_deaths_list_new[[i]] <- projections(r = pmcmc_output_all_deaths_list[[i]], 
                                                    R0 = c(1.25,2.00),
                                                    tt_R0 = c(0,index_lower7_all_deaths[i]),time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_1.25_reported_deaths_list_new[[i]] <- projections(r = pmcmc_output_reported_deaths_list[[i]], 
                                                         R0 = c(1.25,2.00),
                                                         tt_R0 = c(0,index_lower7_reported_deaths[i]),time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_0.75_all_deaths_list_newnormal[[i]] <- projections(r = pmcmc_output_all_deaths_list[[i]], 
                                                          R0 = c(0.75,1.25),
                                                          tt_R0 = c(0,index_lower7_all_deaths_0.75[i]),time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_0.75_all_deaths_list_oldnormal[[i]] <- projections(r = pmcmc_output_all_deaths_list[[i]], 
                                                          R0 = c(0.75,2.00),
                                                          tt_R0 = c(0,index_lower7_all_deaths_0.75[i]),time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_0.75_reported_deaths_list_newnormal[[i]] <- projections(r = pmcmc_output_reported_deaths_list[[i]], 
                                                               R0 = c(0.75,1.25),
                                                               tt_R0 = c(0,index_lower7_reported_deaths_0.75[i]),time_period = 666) # to cover 1 sept up to 31 dec 2021
  proj_0.75_reported_deaths_list_oldnormal[[i]] <- projections(r = pmcmc_output_reported_deaths_list[[i]], 
                                                               R0 = c(0.75,2.00),
                                                               tt_R0 = c(0,index_lower7_reported_deaths_0.75[i]),time_period = 666) # to cover 1 sept up to 31 dec 2021
}

proj_list <- list()
for (i in 1:6){
  proj_0.75_all_deaths_NN <- format_output(proj_0.75_all_deaths_list_newnormal[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="0.75 back to 1.25 - All deaths")
  proj_0.75_all_deaths_ON <- format_output(proj_0.75_all_deaths_list_oldnormal[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="0.75 back to 2.00 - All deaths")
  proj_0.75_reported_deaths_NN <- format_output(proj_0.75_reported_deaths_list_newnormal[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="0.75 back to 1.25 - Reported deaths")
  proj_0.75_reported_deaths_ON <- format_output(proj_0.75_reported_deaths_list_oldnormal[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="0.75 back to 2.00 - Reported deaths")
  proj_1.25_all_deaths <- format_output(proj_1.25_all_deaths_list_new[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="1.25 - All deaths")
  proj_1.25_reported_deaths <- format_output(proj_1.25_reported_deaths_list_new[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="1.25 - Reported deaths")
  proj_2.00_all_deaths <- format_output(proj_2.00_all_deaths_list[[i]],var_select = c("deaths","hospital_demand","ICU_demand")) %>% drop_na() %>% 
    mutate(prov=java_provinces[i],scenario_Rt="2.00 - All deaths")
  proj_list[[i]] <- bind_rows(proj_0.75_all_deaths_NN,proj_0.75_all_deaths_ON,
                              proj_0.75_reported_deaths_NN,proj_0.75_reported_deaths_ON,
                              proj_1.25_all_deaths,proj_1.25_reported_deaths,proj_2.00_all_deaths)
}
proj_df_new <- bind_rows(proj_list)

proj_df_new <- proj_df_new %>% left_join(date_t_df) %>% left_join(java_province_df)
proj_df_new$prov <- factor(proj_df_new$prov,levels=java_provinces)
proj_df_region <- proj_df_new %>% group_by(replicate,compartment,t,scenario_Rt,date,region) %>% summarise(y=sum(y)) %>% ungroup() %>% filter(date>ymd("2020-08-15")) %>% 
  arrange(region,scenario_Rt,compartment,replicate,date,t) %>% ungroup() %>% #%>%  pivot_wider(names_from=compartment,values_from=y)
  left_join(pop_province_all_region) %>% mutate(y_per_mil=y/pop*1000000)

# PLOT SCENARIOS IN FIGURES 
proj_df_region_C <- proj_df_region %>% filter(scenario_Rt %in% c("0.75 back to 1.25 - All deaths","0.75 back to 1.25 - Reported deaths",
                                                                 "0.75 back to 2.00 - All deaths",
                                                                 "1.25 - All deaths","1.25 - Reported deaths","2.00 - All deaths"))
proj_df_region_C$scenario_Rt <- factor(proj_df_region_C$scenario_Rt,levels=c("1.25 - Reported deaths","1.25 - All deaths",
                                                                             "2.00 - All deaths","0.75 back to 1.25 - Reported deaths",
                                                                             "0.75 back to 2.00 - All deaths","0.75 back to 1.25 - All deaths"))
  
fig_5C <- proj_df_region_C %>% filter(compartment=="deaths") %>% 
  ggplot(aes(x=date,y=y,col=scenario_Rt,fill=scenario_Rt)) +
  stat_lineribbon(.prob = .95, alpha = 0.5) + 
  stat_summary(fun.y = median,  geom = 'line', size=1) +
  theme_bw() + xlab("Date") + ylab("Daily number of deaths") + 
  facet_wrap(.~region,scales = "free") +
  # scale_fill_manual(values=c("#00441b","#662506","#41ae76","#ec7014","#e7298a","#49006a","#800026"),
  #                   labels=c("Suppression 1A","Suppression 1B","Suppression 2A","Suppression 2B","Current A","Current B","Return to normal A")) +
  scale_colour_manual(values=c("#49006a","#e7298a","#800026","#41ae76","#662506","#ec7014"),
                      labels=c("Rc=1.25->2.00 (Conf.)","Rc=1.25->2.00 (Susp.)","Rc=2.00 (Susp.)","Rc=0.75->1.25 (Conf.)","Rc=0.75->2.00 (Susp.)","Rc=0.75->1.25 (Susp.)"),
                      breaks=c("1.25 - Reported deaths","1.25 - All deaths",
                               "2.00 - All deaths","0.75 back to 1.25 - Reported deaths",
                               "0.75 back to 2.00 - All deaths","0.75 back to 1.25 - All deaths")) + 
  scale_fill_manual(values=c("#49006a","#e7298a","#800026","#41ae76","#662506","#ec7014"),
                    labels=c("Rc=1.25->2.00 (Conf.)","Rc=1.25->2.00 (Susp.)","Rc=2.00 (Susp.)","Rc=0.75->1.25 (Conf.)","Rc=0.75->2.00 (Susp.)","Rc=0.75->1.25 (Susp.)"),
                    breaks=c("1.25 - Reported deaths","1.25 - All deaths",
                             "2.00 - All deaths","0.75 back to 1.25 - Reported deaths",
                             "0.75 back to 2.00 - All deaths","0.75 back to 1.25 - All deaths")) +
  theme(legend.position="top",legend.text = element_text(size=10)) + 
  labs(col="Projection\nscenarios",fill="Projection\nscenarios") +
  scale_x_date(date_breaks = "2 months",date_labels = "%b %Y") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(col=guide_legend(nrow=2,byrow=FALSE))

fig_5D <- ggplot() +
  stat_lineribbon(data=filter(proj_df_region,scenario_Rt=="1.25 - All deaths" & compartment!="deaths"),
                  aes(x=date,y=y_per_mil,col="b",fill="b",linetype=compartment),
                  .prob = .95, alpha = 0.25) + 
  stat_summary(data=filter(proj_df_region,scenario_Rt=="1.25 - All deaths" & compartment!="deaths"),
               aes(x=date,y=y_per_mil,col="b",fill="b",linetype=compartment),
               fun.y = median,  geom = 'line', size=1) + 
  stat_lineribbon(data=filter(proj_df_region,scenario_Rt=="1.25 - All deaths" & compartment!="deaths"),
                  aes(x=date,y=y_per_mil,col="b",fill="b",linetype=compartment),
                  .prob = .95, alpha = 0.25) +
  stat_summary(data=filter(proj_df_region,scenario_Rt=="1.25 - All deaths" & compartment!="deaths"),
               aes(x=date,y=y_per_mil,col="b",fill="b",linetype=compartment),
               fun.y = median,  geom = 'line', size=1) + 
  stat_lineribbon(data=filter(proj_df_region,scenario_Rt=="1.25 - Reported deaths" & compartment!="deaths"),
                  aes(x=date,y=y_per_mil,col="a",fill="a",linetype=compartment),
                  .prob = .95, alpha = 0.25) +
  stat_summary(data=filter(proj_df_region,scenario_Rt=="1.25 - Reported deaths" & compartment!="deaths"),
               aes(x=date,y=y_per_mil,col="a",fill="a",linetype=compartment),
               fun.y = median,  geom = 'line', size=1) + 
  stat_lineribbon(data=filter(proj_df_region,scenario_Rt=="1.25 - Reported deaths" & compartment!="deaths"),
                  aes(x=date,y=y_per_mil,col="a",fill="a",linetype=compartment),
                  .prob = .95, alpha = 0.25)  + 
  stat_summary(data=filter(proj_df_region,scenario_Rt=="1.25 - Reported deaths" & compartment!="deaths"),
               aes(x=date,y=y_per_mil,col="a",fill="a",linetype=compartment),
               fun.y = median,  geom = 'line', size=1) + 
  geom_hline(data=hosp_capacity_java_region, aes(yintercept = hosp_beds_per_mil),size=0.75,lty=1,col="#0570b0") +
  geom_hline(data=hosp_capacity_java_region, aes(yintercept = icu_beds_per_mil),size=0.75,lty=2,col="#0570b0") +
  theme_bw() + xlab("Date") + ylab("Hospital beds demand\nper million populations") + 
  facet_wrap(.~region) +
  theme(legend.position="top",legend.text = element_text(size=10)) + 
  labs(fill="Projection scenarios",col="Projection scenarios",linetype="Type") + 
  scale_fill_manual(values=c("#49006a","#e7298a"), #"#e7298a","#49006a"  "#004529","#081d58"
                    labels=c("Rc=1.25->2.00 (Conf.)","Rc=1.25->2.00 (Susp.)")) +
  scale_colour_manual(values=c("#49006a","#e7298a"), #"#e7298a","#49006a"  "#004529","#081d58"
                      labels=c("Rc=1.25->2.00 (Conf.)","Rc=1.25->2.00 (Susp.)")) + 
  scale_x_date(date_breaks = "2 months",date_labels = "%b %Y") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_manual(values=c(1,2),labels=c("Isolation beds","ICU beds")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE,override.aes = list(size = 0.75)))

## Compare estimated current attack rates based upon reported and suspected deaths
## reported deaths
attack_rate_samples_reported_deaths_list <- list()
for(i in 1:6){
  pop_prov <- pop_province %>% filter(prov==java_provinces[i]) %>% pull(pop)
  
  infections_output <- format_output(pmcmc_output_reported_deaths_list[[i]], var_select = c("S"))
  infections_output_summary <- infections_output %>% filter(t==0) %>% mutate(infections=sum(pop_prov)-y,attack_rate=infections/sum(pop_prov))
  
  attack_rate_samples_reported_deaths_list[[i]] <- infections_output_summary %>% pull(attack_rate)
}

## all deaths
attack_rate_samples_all_deaths_list <- list()
for(i in 1:6){
  pop_prov <- pop_province %>% filter(prov==java_provinces[i]) %>% pull(pop)
  
  infections_output <- format_output(pmcmc_output_all_deaths_list[[i]], var_select = c("S"))
  infections_output_summary <- infections_output %>% filter(t==0) %>% mutate(infections=sum(pop_prov)-y,attack_rate=infections/sum(pop_prov))
  
  attack_rate_samples_all_deaths_list[[i]] <- infections_output_summary %>% pull(attack_rate)
}

## summary
df_attack_rate_reported <- list()
df_attack_rate_all <- list()
for (i in 1:6){
  df_attack_rate_reported[[i]] <- data.frame(prov=java_provinces[i],type="Reported deaths",attack_rate=attack_rate_samples_reported_deaths_list[[i]])
  df_attack_rate_all[[i]] <- data.frame(prov=java_provinces[i],type="All deaths",attack_rate=attack_rate_samples_all_deaths_list[[i]])
}
df_attack_rate_reported <- bind_rows(df_attack_rate_reported)
df_attack_rate_all <- bind_rows(df_attack_rate_all)
df_attack_rate <- bind_rows(df_attack_rate_reported,df_attack_rate_all)

fig_5B <- df_attack_rate %>% ggplot(aes(x=prov,y=attack_rate*100,fill=type)) + geom_boxplot() + theme_bw() +
  xlab("Province") + ylab("Attack rate (%) - up to 02 Sep 2020") + labs(fill="") + 
  theme(legend.position="top",strip.background = element_blank(),strip.text.x = element_blank(),legend.text = element_text(size=10)) + 
  scale_fill_manual(values=c("#49006a","#e7298a"),labels=c("Confirmed deaths","Suspected deaths"))

fig_5 <- plot_grid(fig_5A,fig_5B,fig_5C,fig_5D,
                   rel_heights = c(1, 1, 1, 1), align = "hv",
                   labels = c("A", "B", "C", "D"), nrow = 2)

pdf(file = "Output/FIG-5.pdf", width = 15, height = 10)
  fig_5
dev.off()