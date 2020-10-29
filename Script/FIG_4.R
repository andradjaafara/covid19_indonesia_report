
# Read metapopulation model output if previously not run
model_output_main_12 <- readRDS("Output/Metapopulation/model_output_main_NN_12_19092020_new.rds")
model_output_rural90 <- readRDS("Output/Metapopulation/model_output_rural90_NN_12_19092020_new.rds")
model_output_rural75 <- readRDS("Output/Metapopulation/model_output_rural75_NN_12_19092020_new.rds")
model_output_rural60 <- readRDS("Output/Metapopulation/model_output_rural60_NN_12_19092020_new.rds")
model_output_rural90_unmitigated <- readRDS("Output/Metapopulation/model_output_rural90_unmitigated_NN_12_19092020_new.rds")
model_output_rural90_ramadan1 <- readRDS("Output/Metapopulation/model_output_rural90_ramadan1_NN_12_19092020_new.rds")
model_output_rural90_ramadan2 <- readRDS("Output/Metapopulation/model_output_rural90_ramadan2_NN_12_19092020_new.rds")
model_output_rural90_ramadan3 <- readRDS("Output/Metapopulation/model_output_rural90_ramadan3_NN_12_19092020_new.rds")
model_output_rural90_highOR_ramadan <- readRDS("Output/Metapopulation/model_output_rural90_NN_12_19092020_new_highOR_ramadan.rds")

max_date_1 <- ymd("2020-05-31")
max_date_2 <- last_date
### SUMMARISE BASELINE MODEL OUTPUT ####
p_model_output_main <- metapopulation_model_summary_admin1_point_single(model_output_list=model_output_rural90,
                                                                        google_mobility_to_plot=google_mobility_java_updated,
                                                                        time_period=time_period,province_names_java=province_names_java,
                                                                        init_date=init_date,last_date=last_date,N_reg=N_reg,
                                                                        cases_df=idn_pos_java,deaths_df=idn_deaths_java,
                                                                        funerals_df=idn_funerals_java,
                                                                        hospitalised_df=java_hospitalised_long,
                                                                        admin_df=urban_rural_flag_df,reg_exclude=c("OUTSIDE JAVA"),
                                                                        max_date=max_date_2)
### SUMMARISE DEATHS OF ALL SCENARIOS ####
output_deaths_combin <- list(model_output_rural90$deaths_time_series,
                             model_output_rural90_ramadan1$deaths_time_series,
                             model_output_rural90_ramadan2$deaths_time_series,
                             model_output_rural90_ramadan3$deaths_time_series,
                             model_output_rural90_unmitigated$deaths_time_series)

p_output_deaths_combin <- deaths_compare_scenarios_point_single(model_output_list=output_deaths_combin,
                                                                scenario_labels=c("Baseline scenario","Ramadan 1",
                                                                                  "Ramadan 2","Ramadan 3",
                                                                                  "Unmitigated"),
                                                                deaths_data=idn_deaths_java,funerals_data=idn_funerals_java,
                                                                init_date=init_date,last_date=last_date,admin_df=urban_rural_flag_df,N_reg=N_reg,max_date=max_date_2)

### PLOT 4A and B - baseline and counterfactual deaths
p_4_AB <- plot_grid(p_model_output_main$deaths,p_output_deaths_combin,align="v",labels=c("A","B"),nrow=2,rel_heights = c(0.8,1))



model_output_sc_list <- list(model_output_rural90,
                             model_output_rural90_ramadan1,
                             model_output_rural90_ramadan2,
                             model_output_rural90_ramadan3,
                             model_output_rural90_unmitigated)

model_output_sc_labels <- c("Baseline scenario","Ramadan 1",
                            "Ramadan 2","Ramadan 3",
                            "Unmitigated")

color_vec <- c("#1b9e77","#d95f02","#1f78b4","#66a61e","#e6ab02","#a6761d","#fb9a99","#a65628")
length_sc <- length(model_output_sc_labels)

date_time_df <- data.frame(time=seq_len(time_period),date=seq(init_date,last_date,by="days"))

hosp_avail_summary_list <- list()
for(i in seq_len(length(model_output_sc_labels))){
  hosp_avail <- hospital_availability_per_person(model_output = model_output_sc_list[[i]],
                                                 hospital_beds_capacity = hospital_beds_region$HOSPITAL_BEDS,
                                                 time_period = time_period,
                                                 date_df = date_time_df)
  hosp_avail_summary_list[[i]] <- hosp_avail$summary %>% mutate(sc=model_output_sc_labels[i])
}
hosp_avail_summary_df <- bind_rows(hosp_avail_summary_list)
hosp_avail_summary_df$sc <- factor(hosp_avail_summary_df$sc,levels=model_output_sc_labels)

p_hosp_beds_availability <- hosp_avail_summary_df %>% filter(date>ymd("2020-03-05")) %>% ggplot(aes(x=date,y=beds_avail_per_person,col=sc)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(-Inf,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(-Inf,Inf), col="#00441b", lty=2) + 
  geom_line(size=2) + theme_bw() +
  geom_hline(yintercept = 0,col="red",lty=2,size=1.5) + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  ylab("Median beds availability\nper severe case") + xlab("Date") +
  scale_color_manual(values = c(color_vec[1:length_sc]),
                     labels = c(model_output_sc_labels[1:length_sc])) + 
  labs(col="Simulation\nscenarios") +
  theme(legend.position = "top") + guides(col=guide_legend(nrow=2,byrow=TRUE)) +
  annotate(geom = "text", x = ymd("2020-04-14"), y = 2000, 
           label = substitute(paste(italic('Pembatasan sosial berskala besar (PSBB)'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-04-19"), y = 2000, 
           label = "Large-scale social restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000, 
           label = "Ramadan domestic travel restrictions", angle = 90, size=3, hjust=0.5) +
  geom_segment(aes(x = ymd("2020-06-10"), y = 10000, xend = ymd("2020-09-02"), yend = 10000),
               lineend = "butt", linejoin = "mitre", col="black",
               size = 0.5, arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 9500, 
           label = substitute(paste("Transitional ",italic('PSBB'), " and")), size=3) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 8750, 
           label = substitute(paste(italic('Adaptasi kebiasaan baru (AKB)'))), size=3) + 
  annotate(geom = "text", x = ymd("2020-07-17"), y = 8000, 
           label = "The new normal period", size=3)

p_hosp_beds_availability_v2 <- hosp_avail_summary_df %>% filter(date>ymd("2020-03-05")) %>% ggplot(aes(x=date,y=beds_avail_per_person,col=sc)) +
  annotate("rect", xmin=ymd("2020-04-10"), xmax=ymd("2020-06-05"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#66c2a4") +
  annotate("rect", xmin=ymd("2020-04-24"), xmax=ymd("2020-06-07"), ymin=-Inf, ymax=Inf, alpha=0.5, fill="#00441b") +  
  annotate("line", x=c(ymd("2020-04-10"),ymd("2020-04-10")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-04-24"),ymd("2020-04-24")), y=c(-Inf,Inf), col="#00441b", lty=2) +
  annotate("line", x=c(ymd("2020-06-05"),ymd("2020-06-05")), y=c(-Inf,Inf), col="#66c2a4", lty=2) +
  annotate("line", x=c(ymd("2020-06-07"),ymd("2020-06-07")), y=c(-Inf,Inf), col="#00441b", lty=2) + 
  geom_line(size=2) + theme_bw() +
  geom_hline(yintercept = 0,col="red",lty=2,size=1.5) + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  ylab("Median beds availability\nper severe case") + xlab("Date") +
  scale_color_manual(values = c(color_vec[1:length_sc]),
                     labels = c(model_output_sc_labels[1:length_sc])) + 
  labs(col="Simulation\nscenarios") +
  theme(legend.position = "none") +
  annotate(geom = "text", x = ymd("2020-04-14"), y = 2000, 
           label = substitute(paste(italic('Pembatasan sosial berskala besar (PSBB)'))), angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-04-19"), y = 2000, 
           label = "Large-scale social restrictions", angle = 90, size=3, hjust=0.5) +
  annotate(geom = "text", x = ymd("2020-05-12"), y = 2000, 
           label = "Ramadan domestic travel restrictions", angle = 90, size=3, hjust=0.5)

total_pop <- rowSums(pop_matrix)
attack_rate_main <- colMeans(t(t(model_output_rural90$infections)/total_pop))
attack_rate_unmitigated <- colMeans(t(t(model_output_rural90_unmitigated$infections)/total_pop))

attack_rate_main_31may <- vector()
attack_rate_unmitigated_31may <- vector() 
for(i in 1:115){
  attack_rate_main_31may[i] <- median(colSums(model_output_rural90$infections_time_series[[i]][1:146,])/total_pop[i])   
  attack_rate_unmitigated_31may[i] <- median(colSums(model_output_rural90_unmitigated$infections_time_series[[i]][1:146,])/total_pop[i])
}

prov_summary_model <- urban_rural_flag_df %>% select(prov=PROVINCE,district=REGION) %>% mutate(median_age_bracket=NA,p_hosp=NA,p_crit_given_hosp=NA,beds_hosp_reg=NA,beds_ICU=NA)

prov_summary_model$attack_rate_main <- attack_rate_main
prov_summary_model$attack_rate_unmitigated <- attack_rate_unmitigated
prov_summary_model$attack_rate_main_31may <- attack_rate_main_31may
prov_summary_model$attack_rate_unmitigated_31may <- attack_rate_unmitigated_31may

district_java_model_shp <- readOGR(dsn="Data/map", layer="ADMIN2_JAVA", stringsAsFactors = FALSE)
district_java_model_shp_df <- fortify(district_java_model_shp, region = "ADMIN2")
province_java_model_shp <- readOGR(dsn="Data/map", layer="ADMIN1_JAVA", stringsAsFactors = FALSE)
province_java_model_shp_df <- fortify(province_java_model_shp, region = "ADMIN1")
province_java_model_shp_df$ifr <- NA

prov_summary_model <- prov_summary_model[-2,]
prov_summary_model$id <- prov_summary_model$district

district_java_model_shp_df <- district_java_model_shp_df %>% left_join(prov_summary_model)

map_model <- ggplot(data = district_java_model_shp_df, aes(x = long, y = lat, group = group))

p_AR_main_map <- map_model +
  geom_polygon(aes(fill=attack_rate_main*100),color = 'black', size = 0.5) +
  guides(fill=guide_colorbar(title="Proportion infected - baseline scenario (%)")) +
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") +
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=1.25) +
  theme(legend.position="bottom") + theme_bare + theme(plot.margin = unit(c(0,.5,0,.5), "cm"))

p_AR_unmitigated_map <- map_model +
  geom_polygon(aes(fill=attack_rate_unmitigated*100),color = 'black', size = 0.5) +
  guides(fill=guide_colorbar(title="Proportion infected - unmitigated scenario (%)")) +
  scale_fill_gradient(high = "#bd0026", low = "#ffeda0", guide = "colorbar") +
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=1.25) +
  theme(legend.position="bottom") + theme_bare + theme(plot.margin = unit(c(0,.5,0,.5), "cm"))

p_AR_main_31may_map <- map_model +
  geom_polygon(aes(fill=attack_rate_main_31may*100),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Proportion infected - baseline scenario (%)")) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(0,65)) +
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=0.75) +
  theme(legend.position="bottom") + theme_bare + theme(plot.margin = unit(c(0,.5,0,.5), "cm"))

p_AR_unmitigated_31may_map <- map_model +
  geom_polygon(aes(fill=attack_rate_unmitigated_31may*100),color = 'black', size = 0.25) +
  guides(fill=guide_colorbar(title="Proportion infected - unmitigated scenario (%)")) +
  scale_fill_viridis(option = "magma", direction = -1, limits=c(0,65)) +
  coord_fixed(1.3) + geom_polygon(data=province_java_shp_df, aes(x = long, y = lat, group = group), fill=NA, col="black", size=0.75) +
  theme(legend.position="bottom") + theme_bare + theme(plot.margin = unit(c(0,.5,0,.5), "cm"))

p_4_AB <- plot_grid(p_model_output_main$deaths,p_output_deaths_combin,align="v",labels=c("A","B"),nrow=2,rel_heights = c(0.8,1))
p_4_DE <- plot_grid(p_AR_main_31may_map,p_AR_unmitigated_31may_map,align="v",nrow=2,labels=c("D","E"))
p_4_CDE <- plot_grid(p_hosp_beds_availability,p_4_DE,align="h",labels=c("C",NA),rel_widths = c(0.9,1))
p_4 <- plot_grid(p_4_AB,p_4_CDE,rel_heights = c(1,0.95),align="v",nrow=2)



pdf(file = "Output/FIG-4.pdf", width = 10, height = 10)
  p_4
dev.off()

## Table 1

p_model_output_unmitigated <- metapopulation_model_summary_admin1_point_single(model_output_list=model_output_rural90_unmitigated,
                                                                               google_mobility_to_plot=google_mobility_java_updated,
                                                                               time_period=time_period,province_names_java=province_names_java,
                                                                               init_date=init_date,last_date=last_date,N_reg=N_reg,
                                                                               cases_df=idn_pos_java,deaths_df=idn_deaths_java,
                                                                               funerals_df=idn_funerals_java,
                                                                               hospitalised_df=java_hospitalised_long,
                                                                               admin_df=urban_rural_flag_df,reg_exclude=c("OUTSIDE JAVA"),
                                                                               max_date=max_date_2)

# From beginning up to 31st May
# Data
idn_deaths_java_cumsum <- idn_deaths_java %>% group_by(prov) %>% mutate(cumsum_deaths = cumsum(deaths)) %>% arrange(prov) %>% ungroup()
idn_deaths_java_31may <- idn_deaths_java_cumsum %>% filter(date==ymd("2020-05-31")) %>% select(prov,total_deaths_data=cumsum_deaths)
idn_pos_java_cumsum <- idn_pos_java %>% group_by(prov) %>% mutate(cumsum_cases = cumsum(cases)) %>% arrange(prov) %>% ungroup()
idn_pos_java_31may <- idn_pos_java_cumsum %>% filter(date==ymd("2020-05-31")) %>% select(prov,total_cases_data=cumsum_cases)

# Modelled
modelled_deaths_main_31may <- p_model_output_main$df_deaths_daily %>% filter(date<=ymd("2020-05-31")) %>% group_by(prov,draw) %>% 
  summarise(total_deaths_main=sum(deaths)) %>% ungroup()
modelled_deaths_unmitigated_31may <- p_model_output_unmitigated$df_deaths_daily %>% filter(date<=ymd("2020-05-31")) %>% group_by(prov,draw) %>% 
  summarise(total_deaths_unmitigated=sum(deaths)) %>% ungroup()

modelled_deaths_main_31may_java <- p_model_output_main$df_deaths_daily %>% filter(date<=ymd("2020-05-31")) %>% group_by(draw) %>% 
  summarise(total_deaths_main=sum(deaths)) %>% ungroup() %>% mutate(prov="Java")
modelled_deaths_unmitigated_31may_java <- p_model_output_unmitigated$df_deaths_daily %>% filter(date<=ymd("2020-05-31")) %>% group_by(draw) %>% 
  summarise(total_deaths_unmitigated=sum(deaths)) %>% ungroup() %>% mutate(prov="Java")

modelled_deaths_all_31may <- modelled_deaths_main_31may %>% left_join(modelled_deaths_unmitigated_31may) %>% 
  mutate(total_deaths_averted=total_deaths_unmitigated-total_deaths_main)

modelled_deaths_all_31may_java <- modelled_deaths_main_31may_java %>% left_join(modelled_deaths_unmitigated_31may_java) %>% 
  mutate(total_deaths_averted=total_deaths_unmitigated-total_deaths_main)

p <- c(0.025, 0.5, 0.975)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

modelled_deaths_summary_31may <- modelled_deaths_all_31may %>% group_by(prov) %>% 
  summarise_at(vars(total_deaths_main, total_deaths_unmitigated, total_deaths_averted), funs(!!!p_funs))

modelled_deaths_summary_31may_java <- modelled_deaths_all_31may_java %>% group_by(prov) %>% 
  summarise_at(vars(total_deaths_main, total_deaths_unmitigated, total_deaths_averted), funs(!!!p_funs))

table_1_up_to_31may <- rbind(modelled_deaths_summary_31may,modelled_deaths_summary_31may_java)
table_1_up_to_31may$prov <- factor(table_1_up_to_31may$prov,levels=c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","Java"))
table_1_up_to_31may <- table_1_up_to_31may %>% arrange(prov) %>% 
  select("prov","total_deaths_main_2.5%","total_deaths_main_50%","total_deaths_main_97.5%",
         "total_deaths_unmitigated_2.5%","total_deaths_unmitigated_50%","total_deaths_unmitigated_97.5%",
         "total_deaths_averted_2.5%","total_deaths_averted_50%","total_deaths_averted_97.5%")

write.csv(table_1_up_to_31may,"Output/table_1_beginning_up_to_31_may_22092020.csv",row.names = FALSE)

# From 13th up to 31st May
# Modelled
modelled_deaths_main_13_31may <- p_model_output_main$df_deaths_daily %>% filter(date<=ymd("2020-05-31") & date>=ymd("2020-05-13")) %>% group_by(prov,draw) %>% 
  summarise(total_deaths_main=sum(deaths)) %>% ungroup()
modelled_deaths_unmitigated_13_31may <- p_model_output_unmitigated$df_deaths_daily %>% filter(date<=ymd("2020-05-31") & date>=ymd("2020-05-13")) %>% 
  group_by(prov,draw) %>% 
  summarise(total_deaths_unmitigated=sum(deaths)) %>% ungroup()

modelled_deaths_main_13_31may_java <- p_model_output_main$df_deaths_daily %>% filter(date<=ymd("2020-05-31") & date>=ymd("2020-05-13")) %>% group_by(draw) %>% 
  summarise(total_deaths_main=sum(deaths)) %>% ungroup() %>% mutate(prov="Java")
modelled_deaths_unmitigated_13_31may_java <- p_model_output_unmitigated$df_deaths_daily %>% filter(date<=ymd("2020-05-31") & date>=ymd("2020-05-13")) %>% 
  group_by(draw) %>% 
  summarise(total_deaths_unmitigated=sum(deaths)) %>% ungroup() %>% mutate(prov="Java")

modelled_deaths_all_13_31may <- modelled_deaths_main_13_31may %>% left_join(modelled_deaths_unmitigated_13_31may) %>% 
  mutate(total_deaths_averted=total_deaths_unmitigated-total_deaths_main)

modelled_deaths_all_13_31may_java <- modelled_deaths_main_13_31may_java %>% left_join(modelled_deaths_unmitigated_13_31may_java) %>% 
  mutate(total_deaths_averted=total_deaths_unmitigated-total_deaths_main)

p <- c(0.025, 0.5, 0.975)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

modelled_deaths_summary_13_31may <- modelled_deaths_all_13_31may %>% group_by(prov) %>% 
  summarise_at(vars(total_deaths_main, total_deaths_unmitigated, total_deaths_averted), funs(!!!p_funs))

modelled_deaths_summary_13_31may_java <- modelled_deaths_all_13_31may_java %>% group_by(prov) %>% 
  summarise_at(vars(total_deaths_main, total_deaths_unmitigated, total_deaths_averted), funs(!!!p_funs))

table_1_13_up_to_13_31may <- rbind(modelled_deaths_summary_13_31may,modelled_deaths_summary_13_31may_java)
table_1_13_up_to_13_31may$prov <- factor(table_1_13_up_to_13_31may$prov,levels=c("JAKARTA","WEST JAVA","CENTRAL JAVA","YOGYAKARTA","EAST JAVA","BANTEN","Java"))
table_1_13_up_to_13_31may <- table_1_13_up_to_13_31may %>% arrange(prov) %>% 
  select("prov","total_deaths_main_2.5%","total_deaths_main_50%","total_deaths_main_97.5%")

write.csv(table_1_13_up_to_13_31may,"Output/table_1_13_up_to_31_may_22092020.csv",row.names = FALSE)

# Supplementary figure for comparison of rural transmission scenarios

output_deaths_combin_rural_sc <- list(model_output_main_12$deaths_time_series,
                                      model_output_rural90$deaths_time_series,
                                      model_output_rural75$deaths_time_series,
                                      model_output_rural60$deaths_time_series)

p_output_deaths_combin_rural_sc  <- deaths_compare_scenarios_point_single(model_output_list=output_deaths_combin_rural_sc,
                                                                          scenario_labels=c("Rural 100%","Rural 90%",
                                                                                            "Rural 75%","Rural 60%"),
                                                                          deaths_data=idn_deaths_java,funerals_data=idn_funerals_java,
                                                                          init_date=init_date,last_date=last_date,admin_df=urban_rural_flag_df,
                                                                          N_reg=N_reg,max_date=max_date_2)

pdf(file = "Output/Extended-Data-FIG-5.pdf", width = 10, height = 4)
  p_output_deaths_combin_rural_sc
dev.off()

# Supplementary figure for simulated cases

pdf(file = "Output/Extended-Data-FIG-4.pdf", width = 10, height = 3)
  p_model_output_main$cases
dev.off()

# Supplementary figure for comparison of high mobility reduction OR during Ramadan

output_deaths_combin_ramadan_OR_sc <- list(model_output_rural90$deaths_time_series,
                                           model_output_rural90_highOR_ramadan$deaths_time_series)

p_output_deaths_combin_ramadan_OR_sc  <- deaths_compare_scenarios_point_single(model_output_list=output_deaths_combin_ramadan_OR_sc,
                                                                               scenario_labels=c("Ramadan mobility OR = 2","Ramadan mobility OR = 100"),
                                                                               deaths_data=idn_deaths_java,funerals_data=idn_funerals_java,
                                                                               init_date=init_date,last_date=last_date,admin_df=urban_rural_flag_df,
                                                                               N_reg=N_reg,max_date=max_date_2)

pdf(file = "Output/Extended-Data-FIG-6.pdf", width = 10, height = 4)
  p_output_deaths_combin_ramadan_OR_sc
dev.off()

